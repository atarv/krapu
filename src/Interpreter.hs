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

import           Control.Exception              ( bracket )
import           Control.Monad
import           Data.Foldable
import           Data.Functor
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
data Env = Env { symTable :: IORef SymTable, fnDefs :: FnDefs }

-- | Creates a blank environment
emptyEnv :: IO Env
emptyEnv = Env <$> newIORef (Map.empty :| []) <*> pure (Map.empty :| [])

-- | @findVarRef e i@ Find reference to value of the variable which identifier 
-- @i@ refers to (in environment @e@).
findVarRef :: Env -> Identifier -> IO (Maybe (IORef Result))
findVarRef Env { symTable = envRef } idf =
    readIORef envRef <&> foldr (lookIdentifier idf) Nothing

lookIdentifier :: Ord k => k -> Map k a -> Maybe a -> Maybe a
lookIdentifier idf vars found = case idf `Map.lookup` vars of
    Nothing -> found
    valRef  -> valRef

-- | Lookup a variable's value. Fails if variable is not defined yet.
lookupVar :: Env -> Identifier -> IO Result
lookupVar envRef idf = do
    findVarRef envRef idf >>= \case
        Just valRef -> readIORef valRef
        Nothing ->
            let (Identifier var) = idf
            in  fail $ "Variable '" <> T.unpack var <> "' not found"

-- | Define a new variable and it's value. Note that there is no check for
--  existing values, because shadowing variables is allowed.
defineVar :: Env -> Identifier -> Result -> IO Env
defineVar env@Env { symTable = envRef } idf val = do
    valRef <- newIORef val
    modifyIORef' envRef (addVar valRef)
    pure env
    where addVar valRef (ctx :| ctxs) = Map.insert idf valRef ctx :| ctxs

-- | Assign value to variable
assignVar :: Env -> Identifier -> Result -> IO Env
assignVar envRef idf val = do
    findVarRef envRef idf >>= \case
        Just valRef -> modifyIORef' valRef (const val)
        Nothing ->
            let (Identifier var) = idf
            in  fail $ "Variable '" <> T.unpack var <> "' not found"
    pure envRef

-- | Add item definition to environment
defineItem :: Env -> Item -> IO Env
defineItem env@Env { fnDefs = ctx :| ctxs } = \case
    Function identifier params _ body -> do
        -- First check that there isn't already a function with same identifier
        -- in current context. Shadowing functions in outer scopes is allowed.
        case identifier `Map.lookup` NonEmpty.head (fnDefs env) of
            Nothing -> do
                let newFn       = FnDef (fmap fst params) body
                    updatedDefs = Map.insert identifier newFn ctx :| ctxs
                pure $ env { fnDefs = updatedDefs }
            Just _ ->
                let (Identifier idf) = identifier
                in  fail
                        $  "function '"
                        <> T.unpack idf
                        <> "' is already defined"

-- | Lookup function definition from environment
lookupFn :: Env -> Identifier -> IO FnDef
lookupFn env identifier =
    case foldr (lookIdentifier identifier) Nothing (fnDefs env) of
        Nothing ->
            let Identifier idf = identifier
            in  fail $ "'" <> T.unpack idf <> "' is not defined"
        Just def -> pure def

-- | @withinNewScope env a@ Execute given action @a@ in a new scope added to 
-- the environment @env@. Once the action is complete, scope is exited 
-- automatically.
withinNewScope :: Env -> (Env -> IO b) -> IO b
withinNewScope env = bracket (beginScope env) exitScope
  where
    beginScope env = do
        modifyIORef' (symTable env) (Map.empty <|)
        pure $ env { fnDefs = Map.empty <| fnDefs env }
    exitScope env = do
        -- Using @NonEmpty.fromList@ here should be safe since a new scope is 
        -- always added before exiting
        modifyIORef' (symTable env) (NonEmpty.fromList . NonEmpty.tail)
        pure $ env { fnDefs = NonEmpty.fromList . NonEmpty.tail $ fnDefs env }

-- | Print environment starting from the innermost context
-- TODO: Show function names and parameters
printEnv :: Env -> IO ()
printEnv env =
    readIORef (symTable env) >>= traverse showValVarPairs >>= putStr . separate
  where
    separate        = intercalate "---\n" . NonEmpty.toList
    showValVarPairs = Map.foldMapWithKey
        (\(Identifier idf) valRef -> do
            val <- readIORef valRef
            return $ T.unpack idf <> " = " <> show val <> "\n"
        )

-- | Blocks evaluate to their outer expression's result.
evalBlock :: Env -> Block -> IO (Env, Result)
evalBlock env = \case
    Block stmts outerExpr ->
        withinNewScope env
            $   flip addItems       stmts
            >=> flip execStatements stmts
            >=> flip eval           outerExpr
  where
    execStatements = foldM execStatement
    addItems       = foldM addItem

-- | Statements change the program environment (declare new variables and change
-- their values, define new functions etc.) rather than return values (as
-- opposed to expressions).
execStatement :: Env -> Statement -> IO Env
execStatement env = \case
    StatementEmpty              -> pure env
    StatementExpr expr          -> fst <$> eval env expr
    -- Items should be added to environment before executing
    StatementItem _             -> pure env
    StatementLet idf _type expr -> do
        (env', result) <- eval env expr
        defineVar env' idf result

-- | Add item definition to environment. Other types of statements are ignored.
addItem :: Env -> Statement -> IO Env
addItem env (StatementItem item) = defineItem env item
addItem env _                    = pure env

-- | Evaluate an expression. There will always be a result and side effects may
-- be performed.
eval :: Env -> Expr -> IO (Env, Result)
eval env = \case
    -- Literals
    Unit        -> pure (env, ResUnit)
    IntLit  i   -> pure (env, ResInt i)
    BoolLit b   -> pure (env, ResBool b)

    -- Variables
    Var     idf -> (env, ) <$> lookupVar env idf

    -- Arithmetic
    lhs :+ rhs  -> binaryIntOp env (+) lhs rhs
    lhs :- rhs  -> binaryIntOp env (-) lhs rhs
    lhs :* rhs  -> binaryIntOp env (*) lhs rhs
    lhs :/ rhs  -> binaryIntOp env div lhs rhs
    Negate expr -> eval env expr >>= \case
        (env', ResInt i) -> pure (env', ResInt (-i))
        (_   , val     ) -> errUnexpectedType "integer" val
    Plus expr -> eval env expr >>= \case
        (env', ResInt i) -> pure (env', ResInt i)
        (_   , val     ) -> errUnexpectedType "integer" val

    -- Boolean
    lhs :&& rhs -> binaryBoolOp env (&&) lhs rhs
    lhs :|| rhs -> binaryBoolOp env (||) lhs rhs
    Not expr    -> eval env expr >>= \case
        (env', ResBool b) -> pure (env', ResBool (not b))
        (_   , val      ) -> errUnexpectedType "bool" val

    -- Comparison
    lhs       :<  rhs -> comparisonOp env (<) lhs rhs
    lhs       :>  rhs -> comparisonOp env (>) lhs rhs
    lhs       :<= rhs -> comparisonOp env (<=) lhs rhs
    lhs       :>= rhs -> comparisonOp env (>=) lhs rhs

    -- Equality
    lhs       :== rhs -> equalityOp env (==) lhs rhs
    lhs       :!= rhs -> equalityOp env (/=) lhs rhs

    -- Assignment
    (Var idf) :=  rhs -> eval env rhs >>= \case
        (env', res) -> (, res) <$> assignVar env' idf res
    err := _ -> fail $ "Cannot assign to expression " <> show err

    -- Expressions with blocks
    IfExpr ((cond, conseq) : elseIfs) alt -> eval env cond >>= \case
        (env', ResBool b) ->
            if b then evalBlock env' conseq else eval env' $ IfExpr elseIfs alt
        (_, val) -> errUnexpectedType "bool" val
    IfExpr [] (Just alt)             -> evalBlock env alt
    IfExpr [] Nothing                -> pure (env, ResUnit)
    ExprBlock block                  -> evalBlock env block
    While condition body             -> evalWhile env condition body
    Loop body                        -> evalWhile env (BoolLit True) body

    -- Misc
    FnCall (Identifier "debug") args -> do
        (env', evaledArgs) <- evalArgs env args
        mapM_ print evaledArgs
        pure (env', ResUnit)
    FnCall (Identifier "debug_env") [] -> do
        printEnv env
        pure (env, ResUnit)
    FnCall fnName args -> do
        fnDef              <- lookupFn env fnName
        (env', evaledArgs) <- evalArgs env args
        evalFnCall env' fnDef evaledArgs

    Break _expr -> fail "break not implemented"
  where
    binaryIntOp env op lhs rhs = evalToPair env lhs rhs >>= \case
        (env', ResInt l, ResInt r) -> pure (env', ResInt (l `op` r))
        (_   , l       , r       ) -> typeMismatch l r
    binaryBoolOp env op lhs rhs = evalToPair env lhs rhs >>= \case
        (env', ResBool l, ResBool r) -> pure (env', ResBool (l `op` r))
        (_   , l        , r        ) -> typeMismatch l r
    comparisonOp env op lhs rhs = evalToPair env lhs rhs >>= \case
        (env', l@(ResInt _), r@(ResInt _)) -> pure (env', ResBool (l `op` r))
        (_   , l           , r           ) -> typeMismatch l r
    equalityOp env op lhs rhs = evalToPair env lhs rhs >>= \case
        (env', l@(ResBool _), r@(ResBool _)) -> pure (env', ResBool (l `op` r))
        (env', l@(ResInt _) , r@(ResInt _) ) -> pure (env', ResBool (l `op` r))
        (env', l@ResUnit    , r@ResUnit    ) -> pure (env', ResBool (l `op` r))
        (_   , l            , r            ) -> typeMismatch l r
    typeMismatch lhs rhs =
        fail $ concat ["Type mismatch: ", show lhs, ", ", show rhs]
    errUnexpectedType expected value =
        fail $ concat
            ["Unexpected type: expected ", expected, ", got ", show value]
    -- Evaluate two expressions to a pair so they can be pattern matched easily
    evalToPair :: Env -> Expr -> Expr -> IO (Env, Result, Result)
    evalToPair env a b = do
        (env' , resA) <- eval env a
        (env'', resB) <- eval env' b
        pure (env'', resA, resB)

    evalWhile :: Env -> Expr -> Block -> IO (Env, Result)
    evalWhile env condition body = do
        eval env condition >>= \case
            (env', ResBool continue) -> if continue
                then evalBlock env' body
                    >>= \(env'', _) -> evalWhile env'' condition body
                else pure (env', ResUnit)
            (_, err) ->
                fail
                    $  "Loop predicate must evaluate to boolean value, got"
                    <> show err

    evalArgs :: Env -> [Expr] -> IO (Env, [Result])
    evalArgs env args = foldM
        (\(env', results) arg -> fmap (: results) <$> eval env' arg)
        (env, [])
        args

    evalFnCall :: Env -> FnDef -> [Result] -> IO (Env, Result)
    evalFnCall env (FnDef params body) args = do
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
        withinNewScope env $ \env -> do
            foldM (uncurry . defineVar) env argDefs >>= flip evalBlock body

-- | Run the main program of the crate. Fails if main is not defined.
runProgram :: Crate -> IO ()
runProgram (Crate items) = do
    env <- emptyEnv >>= flip (foldM defineItem) items
    void $ eval env (FnCall (Identifier "main") [])
