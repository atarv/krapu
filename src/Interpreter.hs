{-|
Module         : Interpreter
Description    : Interpreter interpretes the AST (runs the program)
Copyright      : (c) Aleksi Tarvainen, 2021
License        : BSD3
Maintainer     : a.aleksi.tarvainen@student.jyu.fi
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}

module Interpreter where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.IORef
import           Data.List                      ( intercalate )
import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , (<|)
                                                )
import           Data.Map.Strict                ( Map )

import           AST

import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T


-- Took some inspiration from one of the course's example projects: vt-2016
-- and Write Yourself a Scheme in 48 hours

-- | Result types are only those that expressions evaluate to
data Result
    = ResUnit
    | ResInt Integer
    | ResBool Bool
    deriving (Show, Eq, Ord)

-- | Symbol table holds variables and their values
type SymTable = NonEmpty (Map Identifier (IORef Result))

-- | A single function definition
data FnDef = FnDef [Identifier] Block

-- | Function definitions
type FnDefs = NonEmpty (Map Identifier FnDef)

-- | Environment of the interpreted program
data Env = Env { symTable :: SymTable, fnDefs :: FnDefs }

type Interpreter a = StateT Env IO a

-- | Creates a blank environment
emptyEnv :: Env
emptyEnv = Env (Map.empty :| []) (Map.empty :| [])

-- | @findVarRef e i@ Find reference to value of the variable which identifier 
-- @i@ refers to (in environment @e@).
findVarRef :: Identifier -> Interpreter (Maybe (IORef Result))
findVarRef idf = gets $ foldr (lookIdentifier idf) Nothing . symTable

lookIdentifier :: Ord k => k -> Map k a -> Maybe a -> Maybe a
lookIdentifier idf vars found = case idf `Map.lookup` vars of
    Nothing -> found
    valRef  -> valRef

-- | Lookup a variable's value. Fails if variable is not defined yet.
lookupVar :: Identifier -> Interpreter Result
lookupVar idf = do
    findVarRef idf >>= \case
        Just valRef -> liftIO $ readIORef valRef
        Nothing ->
            let (Identifier var) = idf
            in  fail $ "Variable '" <> T.unpack var <> "' not found"

-- | Define a new variable and it's value. Note that there is no check for
--  existing values, because shadowing variables is allowed.
defineVar :: Identifier -> Result -> Interpreter ()
defineVar idf val = do
    valRef <- liftIO $ newIORef val
    env    <- get
    put $ env { symTable = addVar valRef (symTable env) }
    where addVar valRef (ctx :| ctxs) = Map.insert idf valRef ctx :| ctxs

-- | Assign value to variable
assignVar :: Identifier -> Result -> Interpreter Result
assignVar idf val = do
    findVarRef idf >>= \case
        Just valRef -> liftIO $ do
            modifyIORef' valRef (const val)
            readIORef valRef
        Nothing ->
            let (Identifier var) = idf
            in  fail $ "Variable '" <> T.unpack var <> "' not found"

-- | Add item definition to environment
defineItem :: Item -> Interpreter ()
-- defineItem env@Env { fnDefs = ctx :| ctxs } = \case
defineItem = \case
    Function identifier params _ body -> do
        -- First check that there isn't already a function with same identifier
        -- in current context. Shadowing functions in outer scopes is allowed.
        env <- get
        case identifier `Map.lookup` NonEmpty.head (fnDefs env) of
            Nothing -> do
                env <- get
                let (ctx :| ctxs) = fnDefs env
                    newFn         = FnDef (fmap fst params) body
                    updatedDefs   = Map.insert identifier newFn ctx :| ctxs
                put $ env { fnDefs = updatedDefs }
            Just _ ->
                let (Identifier idf) = identifier
                in  fail
                        $  "function '"
                        <> T.unpack idf
                        <> "' is already defined"

-- | Lookup function definition from environment
lookupFn :: Identifier -> Interpreter FnDef
lookupFn identifier = do
    env <- get
    case foldr (lookIdentifier identifier) Nothing (fnDefs env) of
        Nothing ->
            let Identifier idf = identifier
            in  fail $ "'" <> T.unpack idf <> "' is not defined"
        Just def -> pure def

-- | @withinNewScope env a@ Execute given action @a@ in a new scope added to 
-- the environment @env@. Once the action is complete, scope is exited 
-- automatically.
withinNewScope :: Interpreter a -> Interpreter a
withinNewScope action = modify' beginScope >> action <* modify' exitScope
  where
    beginScope env = env { fnDefs   = Map.empty <| fnDefs env
                         , symTable = Map.empty <| symTable env
                         }
    exitScope env = env { fnDefs   = removeScope $ fnDefs env
                        , symTable = removeScope $ symTable env
                        }
    -- Using NonEmpty.fromList here should be safe since a new scope is 
    -- always added before exiting
    removeScope = NonEmpty.fromList . NonEmpty.tail

-- | Print environment starting from the innermost context
printEnv :: Interpreter ()
printEnv = do
    contexts <- NonEmpty.zip <$> gets symTable <*> gets fnDefs
    let showContexts = traverse
            (\(syms, defs) -> (<> showFnDefs defs) <$> showValVarPairs syms)
            contexts
    liftIO $ showContexts >>= putStr . separate
  where
    separate        = intercalate "---\n" . NonEmpty.toList
    showValVarPairs = Map.foldMapWithKey
        (\(Identifier idf) valRef -> do
            val <- liftIO $ readIORef valRef
            return $ T.unpack idf <> " = " <> show val <> "\n"
        )
    showFnDefs = Map.foldMapWithKey
        (\(Identifier idf) (FnDef params _) ->
            mconcat ["fn ", T.unpack idf, "(", commaSep params, ")\n"]
        )
    commaSep = intercalate ", " . fmap (\(Identifier idf) -> T.unpack idf)

-- | Blocks evaluate to their outer expression's result.
evalBlock :: Block -> Interpreter Result
evalBlock (Block stmts outerExpr) = withinNewScope $ do
    mapM_ addItem       stmts
    mapM_ execStatement stmts
    eval outerExpr

-- | Statements change the program environment (declare new variables and change
-- their values, define new functions etc.) rather than return values (as
-- opposed to expressions).
execStatement :: Statement -> Interpreter ()
execStatement = \case
    StatementEmpty              -> pure ()
    StatementExpr expr          -> void $ eval expr
    -- Items should be added to environment before executing
    StatementItem _             -> pure ()
    StatementLet idf _type expr -> eval expr >>= defineVar idf

-- | Add item definition to environment. Other types of statements are ignored.
addItem :: Statement -> Interpreter ()
addItem (StatementItem item) = defineItem item
addItem _                    = pure ()

-- | Evaluate an expression. There will always be a result and side effects may
-- be performed.
eval :: Expr -> Interpreter Result
eval = \case
    -- Literals
    Unit        -> pure ResUnit
    IntLit  i   -> pure $ ResInt i
    BoolLit b   -> pure $ ResBool b

    -- Variables
    Var     idf -> lookupVar idf

     -- Arithmetic
    lhs :+ rhs  -> binaryIntOp (+) lhs rhs
    lhs :- rhs  -> binaryIntOp (-) lhs rhs
    lhs :* rhs  -> binaryIntOp (*) lhs rhs
    lhs :/ rhs  -> binaryIntOp div lhs rhs
    Negate expr -> eval expr >>= \case
        ResInt i -> pure $ ResInt (-i)
        val      -> errUnexpectedType "integer" val
    Plus expr -> eval expr >>= \case
        ResInt i -> pure $ ResInt i
        val      -> errUnexpectedType "integer" val

--     -- Boolean
    lhs :&& rhs -> binaryBoolOp (&&) lhs rhs
    lhs :|| rhs -> binaryBoolOp (||) lhs rhs
    Not expr    -> eval expr >>= \case
        ResBool b -> pure $ ResBool (not b)
        val       -> errUnexpectedType "bool" val

     -- Comparison
    lhs     :<                         rhs -> comparisonOp (<) lhs rhs
    lhs     :>                         rhs -> comparisonOp (>) lhs rhs
    lhs     :<=                        rhs -> comparisonOp (<=) lhs rhs
    lhs     :>=                        rhs -> comparisonOp (>=) lhs rhs

     -- Equality
    lhs     :==                        rhs -> equalityOp (==) lhs rhs
    lhs     :!=                        rhs -> equalityOp (/=) lhs rhs

    -- Variables
    Var idf :=                         rhs -> eval rhs >>= assignVar idf
    err := _ -> fail $ "Cannot assign to expression " <> show err

--     -- Expressions with blocks
    IfExpr  ((cond, conseq) : elseIfs) alt -> eval cond >>= \case
        ResBool b -> if b then evalBlock conseq else eval $ IfExpr elseIfs alt
        val       -> errUnexpectedType "bool" val
    IfExpr [] (Just alt)             -> evalBlock alt
    IfExpr [] Nothing                -> pure ResUnit

    ExprBlock block                  -> evalBlock block
    While condition body             -> evalWhile condition body
    Loop body                        -> evalWhile (BoolLit True) body

--     -- Misc
    FnCall (Identifier "debug") args -> do
        evaledArgs <- evalArgs args
        liftIO $ mapM_ print evaledArgs
        pure ResUnit
    FnCall (Identifier "debug_env") [] -> do
        printEnv
        pure ResUnit
    FnCall fnName args -> do
        fnDef <- lookupFn fnName
        evalArgs args >>= evalFnCall fnDef

    Break _expr -> fail "break not implemented"
  where
    binaryIntOp op lhs rhs = evalToPair lhs rhs >>= \case
        (ResInt l, ResInt r) -> pure $ ResInt (l `op` r)
        (l       , r       ) -> typeMismatch l r
    binaryBoolOp op lhs rhs = evalToPair lhs rhs >>= \case
        (ResBool l, ResBool r) -> pure $ ResBool (l `op` r)
        (l        , r        ) -> typeMismatch l r
    comparisonOp op lhs rhs = evalToPair lhs rhs >>= \case
        (l@(ResInt _), r@(ResInt _)) -> pure $ ResBool (l `op` r)
        (l           , r           ) -> typeMismatch l r
    equalityOp op lhs rhs = evalToPair lhs rhs >>= \case
        (l@(ResBool _), r@(ResBool _)) -> pure $ ResBool (l `op` r)
        (l@(ResInt  _), r@(ResInt _) ) -> pure $ ResBool (l `op` r)
        (l@ResUnit    , r@ResUnit    ) -> pure $ ResBool (l `op` r)
        (l            , r            ) -> typeMismatch l r
    typeMismatch lhs rhs =
        fail $ concat ["Type mismatch: ", show lhs, ", ", show rhs]
    errUnexpectedType expected value =
        fail $ concat
            ["Unexpected type: expected ", expected, ", got ", show value]
    -- Evaluate two expressions to a pair so they can be pattern matched easily
    evalToPair :: Expr -> Expr -> Interpreter (Result, Result)
    evalToPair a b = (,) <$> eval a <*> eval b

    evalWhile :: Expr -> Block -> Interpreter Result
    evalWhile condition body = do
        eval condition >>= \case
            ResBool continue -> if continue
                then evalBlock body >> evalWhile condition body
                else pure ResUnit
            err ->
                fail
                    $  "Loop predicate must evaluate to boolean value, got"
                    <> show err

    evalArgs :: [Expr] -> Interpreter [Result]
    evalArgs args = traverse eval args

    evalFnCall :: FnDef -> [Result] -> Interpreter Result
    evalFnCall (FnDef params body) args = do
        let paramCount = length params
            argCount   = length args
            errMsg =
                mconcat
                    [ "Expected "
                    , show paramCount
                    , " arguments, got "
                    , show argCount
                    ]
        when (paramCount /= argCount) $ fail errMsg
        let argDefs = zip params args
        -- TODO: function params get their own scope which is probably 
        -- unnecessary and causes (probably very slight) overhead, but shouldn't
        -- change the semantics.
        withinNewScope $ do
            mapM_ (uncurry defineVar) argDefs -- add parameter values to scope
            evalBlock body

-- | Run the main program of the crate. Fails if main is not defined.
runProgram :: Crate -> IO ()
runProgram (Crate items) = void $ flip execStateT emptyEnv $ do
    mapM_ defineItem items
    eval (FnCall (Identifier "main") [])
