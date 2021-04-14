{-|
Module         : Interpreter
Description    : Interpreter interpretes the AST (runs the program)
Copyright      : (c) Aleksi Tarvainen, 2021
License        : BSD3
Maintainer     : a.aleksi.tarvainen@student.jyu.fi
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Interpreter where

import           Control.Exception
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Data.Array.IO
import           Data.IORef
import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , (<|)
                                                )
import           Data.Map.Strict                ( Map )
import           Data.Maybe
import           Data.Text                      ( Text )

import           AST

import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T


-- Took some inspiration from one of the course's example projects: vt-2016
-- and Write Yourself a Scheme in 48 hours

-- | Result types are only those that expressions evaluate to
data Result
    = ResUnit
    | ResInt Integer
    | ResBool Bool
    | ResStr Text
    | ResArr (IOArray Integer Result)
    deriving (Eq)

instance Show Result where
    show ResUnit       = "()"
    show (ResInt  i  ) = show i
    show (ResBool b  ) = show b
    show (ResStr  str) = show str
    show (ResArr  _  ) = "[array]"

instance Ord Result where
    compare ResUnit     ResUnit     = EQ
    compare (ResInt  a) (ResInt  b) = compare a b
    compare (ResBool p) (ResBool q) = compare p q
    compare (ResStr  s) (ResStr  t) = compare s t
    compare _           _           = undefined -- TODO:

-- | Symbol table holds variables and their values
type SymTable = NonEmpty (Map Identifier (IORef Result))

-- | A single function definition
data FnDef = FnDef [Identifier] Block

-- | Function definitions
type FnDefs = NonEmpty (Map Identifier FnDef)

-- | Environment of the interpreted program. Krapu is block scoped, so function
-- definitions and symbol tabels are handled in a stack-like way.
data Env =
    Env { symTable :: SymTable -- ^ Variables
        , fnDefs :: FnDefs -- ^ Function definitions
        , returning :: Maybe Result -- ^ If set, the value that is being 
                                    -- returned from function
        , breaking :: Maybe Result  -- ^ If set, this value is being returned 
                                    -- from a loop
        , breakContext :: NonEmpty Bool -- ^ If the head of list is true, break
                                        -- statements are allowed
        }

type Interpreter a = StateT Env IO a

-- | Convert a result value to text in format the user should see it.
display :: Result -> Text
display = T.pack . show

-- * Environment handling

-- | Creates a blank environment
emptyEnv :: Env
emptyEnv =
    Env (Map.empty :| []) (Map.empty :| []) Nothing Nothing (False :| [])

-- | @findVarRef e i@ Find reference to value of the variable which identifier 
-- @i@ refers to (in environment @e@).
findVarRef :: Identifier -> Interpreter (Maybe (IORef Result))
findVarRef idf = gets $ foldr (lookIdentifier idf) Nothing . symTable

lookIdentifier :: Ord k => k -> Map k a -> Maybe a -> Maybe a
lookIdentifier idf vars found = case Map.lookup idf vars of
    Nothing -> found
    valRef  -> valRef

-- | Lookup a variable's value. Fails if variable is not defined yet.
lookupVar :: Identifier -> Interpreter Result
lookupVar idf = findVarRef idf >>= \case
    Just valRef -> liftIO $ readIORef valRef
    Nothing ->
        let (Identifier var) = idf
        in  fail $ "Variable '" <> T.unpack var <> "' not found"

-- | @onHeadOf f xs@ returns the list @xs@ with function @f@ applied on it's 
-- first element.
onHeadOf :: (a -> a) -> NonEmpty a -> NonEmpty a
onHeadOf fn (x :| xs) = fn x :| xs

-- | Define a new variable and it's value. Note that there is no check for
--  existing values, because shadowing variables is allowed.
defineVar :: Identifier -> Result -> Interpreter ()
defineVar idf val = do
    valRef <- liftIO $ newIORef val
    env    <- get
    put $ env { symTable = Map.insert idf valRef `onHeadOf` symTable env }

-- | Assign value to variable
assignVar :: Identifier -> Result -> Interpreter Result
assignVar idf val = findVarRef idf >>= \case
    Just valRef -> liftIO $ do
        modifyIORef' valRef (const val)
        readIORef valRef
    Nothing ->
        let (Identifier var) = idf
        in  fail $ "Variable '" <> T.unpack var <> "' not found"

-- | Add item definition to environment
defineItem :: Item -> Interpreter ()
defineItem = \case
    Function identifier params _ body -> do
        -- First check that there isn't already a function with same identifier
        -- in current context. Shadowing functions in outer scopes is allowed.
        env <- get
        maybe addDefinition (const $ errAlreadyDefined identifier)
            $ Map.lookup identifier (NonEmpty.head (fnDefs env))
      where
        newFnDef = FnDef (fmap fst params) body
        addDefinition =
            modify'
                $ \env -> env
                      { fnDefs = Map.insert identifier newFnDef
                                     `onHeadOf` fnDefs env
                      }
        errAlreadyDefined (Identifier idf) =
            fail $ "function '" <> T.unpack idf <> "' is already defined"

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

-- | Set value that is returned from function. All statements are skipped until
-- function is exited (return value is cleared).
setReturn :: Result -> Interpreter ()
setReturn val = modify' (\env -> env { returning = Just val })

-- | Clear returned value
clearReturn :: Interpreter ()
clearReturn = modify' (\env -> env { returning = Nothing })

-- | Clear the value loop is breaked with
clearBreak :: Interpreter ()
clearBreak = modify' $ \env -> env { breaking = Nothing }

-- | Execute given action with break statement allowed (@True@) or disallowed 
-- (@False@).
withBreakAllowed :: Bool -> Interpreter a -> Interpreter a
withBreakAllowed bool action = do
    modify' $ \env -> env { breakContext = bool <| breakContext env }
    result <- action
    modify' $ \env -> env
        { breakContext = NonEmpty.fromList . NonEmpty.tail $ breakContext env
        }
    pure result

-- | Print environment starting from the innermost context
printEnv :: Interpreter ()
printEnv = do
    contexts <- liftM2 NonEmpty.zip (gets symTable) (gets fnDefs)
    let showContexts = traverse
            (\(syms, defs) -> (<> showFnDefs defs) <$> showValVarPairs syms)
            contexts
    liftIO $ showContexts >>= T.putStr . separate
  where
    separate        = T.intercalate "---\n" . NonEmpty.toList
    showValVarPairs = Map.foldMapWithKey
        (\(Identifier idf) valRef -> do
            val <- liftIO $ readIORef valRef
            case val of
                ResArr arr -> do
                    elems <- displayArray arr
                    return $ mconcat [idf, " = ", elems, "\n"]
                _ -> return $ idf <> " = " <> display val <> "\n"
        )
    showFnDefs = Map.foldMapWithKey
        (\(Identifier idf) (FnDef params _) ->
            mconcat ["fn ", idf, "(", commaSep params, ")\n"]
        )
    commaSep = T.intercalate ", " . fmap (\(Identifier idf) -> idf)
    displayArray :: Ix a => IOArray a Result -> IO Text
    displayArray arr = do
        elems <- getElems arr
        return $ "[" <> T.intercalate ", " (display <$> elems) <> "]"

-- * Interpreting

-- | Blocks evaluate to their outer expression's result or the first return 
-- statement's result.
evalBlock :: Block -> Interpreter Result
evalBlock (Block stmts outerExpr) = withinNewScope $ do
    mapM_ addItem       stmts
    mapM_ execStatement stmts
    returnVal <- gets returning
    case returnVal of
        Nothing  -> eval outerExpr
        -- Outer expression is not evaluated when returning early
        Just val -> pure val

-- | Statements change the program environment (declare new variables and change
-- their values, define new functions etc.) rather than return values (as
-- opposed to expressions).
execStatement :: Statement -> Interpreter ()
execStatement stmt =
    liftM2 (||) (isJust <$> gets returning) (isJust <$> gets breaking) >>= \case
    -- If function is returning or loop is exited with break, statements are 
    -- skipped until function and/or loop is exited. See 'evalBlock'
        True  -> pure ()
        False -> case stmt of
            StatementEmpty              -> pure ()
            StatementExpr expr          -> void $ eval expr
            -- Items should be added to environment using 'addItem' before executing
            StatementItem _             -> pure ()
            StatementLet idf _type expr -> eval expr >>= defineVar idf
            StatementReturn expr ->
                maybe (pure ResUnit) eval expr >>= setReturn
            StatementBreak expr -> eval expr >>= breakWith

-- | Add item definition to environment. Other types of statements are ignored.
addItem :: Statement -> Interpreter ()
addItem (StatementItem item) = defineItem item
addItem _                    = pure ()

-- | Break from with loop evaluating to given value. This will fail if breaking
-- is not allowed in current context.
breakWith :: Result -> Interpreter ()
breakWith result = do
    isAllowed <- NonEmpty.head <$> gets breakContext
    unless isAllowed $ fail "Break statement is only allowed in loops"
    modify' $ \env -> env { breaking = Just result }

-- | Evaluate an expression. There will always be a result and side effects may
-- be performed.
eval :: Expr -> Interpreter Result
eval = \case
    -- Literals
    Unit         -> pure ResUnit
    IntLit   i   -> pure $ ResInt i
    BoolLit  b   -> pure $ ResBool b
    Str      str -> pure $ ResStr str
    ArrayLit arr -> do
        elements <- traverse eval arr
        newArr <- liftIO $ newListArray (0, fromIntegral $ length arr) elements
        pure $ ResArr newArr

    -- Variables
    Var idf     -> lookupVar idf

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

    -- Boolean
    lhs :&& rhs -> binaryBoolOp (&&) lhs rhs
    lhs :|| rhs -> binaryBoolOp (||) lhs rhs
    Not expr    -> eval expr >>= \case
        ResBool b -> pure $ ResBool (not b)
        val       -> errUnexpectedType "bool" val

     -- Comparison
    lhs     :<  rhs -> comparisonOp (<) lhs rhs
    lhs     :>  rhs -> comparisonOp (>) lhs rhs
    lhs     :<= rhs -> comparisonOp (<=) lhs rhs
    lhs     :>= rhs -> comparisonOp (>=) lhs rhs

     -- Equality
    lhs     :== rhs -> equalityOp (==) lhs rhs
    lhs     :!= rhs -> equalityOp (/=) lhs rhs

    -- Variables and assignment
    Var idf :=  rhs -> eval rhs >>= assignVar idf
    ArrayAccess (Var idf) indexExpr := expr ->
        liftM2 (,) (lookupVar idf) (eval indexExpr) >>= \case
            (ResArr arr, ResInt i) -> do
                val <- eval expr
                liftIO $ writeArray arr i val
                pure val
            (indexed, index) -> errCannotIndex indexed index
    err := _ -> fail $ "Cannot assign to expression " <> show err

    -- Expressions with blocks
    IfExpr ((cond, conseq) : elseIfs) alt -> eval cond >>= \case
        ResBool b -> if b then evalBlock conseq else eval $ IfExpr elseIfs alt
        val       -> errUnexpectedType "bool" val
    IfExpr [] (Just alt) -> evalBlock alt
    IfExpr [] Nothing    -> pure ResUnit

    ExprBlock block      -> evalBlock block
    While predicate body -> withBreakAllowed True $ do
        runExceptT . forever $ do
            continue <- lift $ eval predicate >>= \case
                ResBool b -> pure b
                nonBool ->
                    fail
                        $  "Loop predicate must evaluate to boolean value, got "
                        <> (T.unpack . display) nonBool
            unless continue $ throwError () -- exit loop
            gets breaking >>= \case
                Nothing      -> void . lift $ evalBlock body
                Just ResUnit -> throwError () -- exit loop
                Just val ->
                    fail
                        .  T.unpack
                        $  "Cannot break while loop with a non-unit value: "
                        <> display val
        ResUnit <$ clearBreak
    Loop body -> withBreakAllowed True $ do
        result <- runExceptT . forever $ gets breaking >>= \case
            Nothing  -> void . lift $ evalBlock body
            Just val -> throwError val
        case result of
            Left val -> val <$ clearBreak
            Right () ->
                fail "Loop was exited without break. This shouldn't happen."

    -- Function calls and primitive functions
    FnCall (Identifier "debug") args -> do
        evaledArgs <- traverse eval args
        liftIO $ mapM_ print evaledArgs
        pure ResUnit
    FnCall (Identifier "debug_env") args -> do
        unless (null args) $ void $ errWrongArgumentCount
            (Identifier "debug_env")
            0
            (length args)
        printEnv
        pure ResUnit
    FnCall (Identifier "println") args -> traverse eval args >>= \case
        [ResStr str] -> do
            liftIO $ T.putStrLn str
            pure ResUnit
        [wrongArg] -> errUnexpectedType "str" wrongArg
        _          -> errWrongArgumentCount (Identifier "print") 1 (length args)
    FnCall (Identifier "i64_to_str") args -> case args of
        [arg] -> eval arg >>= \case
            i@(ResInt _) -> pure . ResStr $ display i
            err          -> errUnexpectedType "integer" err
        args -> errWrongArgumentCount (Identifier "i64_to_str") 1 (length args)
    FnCall (Identifier "str_to_i64") args -> case args of
        [arg] -> eval arg >>= \case
            ResStr str -> pure . ResInt . read $ T.unpack str
            err        -> errUnexpectedType "str" err
        args -> errWrongArgumentCount (Identifier "str_to_i64") 1 (length args)
    FnCall fnName args -> withBreakAllowed False $ evalFnCall fnName args

    -- Misc
    ArrayAccess exprIndexed exprIndex ->
        liftM2 (,) (eval exprIndexed) (eval exprIndex) >>= \case
            (ResArr arr, ResInt index) -> do
                bounds <- liftIO $ getBounds arr
                unless (inRange bounds index) . fail $ mconcat
                    ["Index ", show index, " out of bounds ", show bounds]
                liftIO $ readArray arr index
            (indexed, index) -> errCannotIndex indexed index
  where
    binaryIntOp op lhs rhs = evalToPair lhs rhs >>= \case
        (ResInt l, ResInt r) -> pure $ ResInt (l `op` r)
        (l       , r       ) -> typeMismatch l r
    binaryBoolOp op lhs rhs = evalToPair lhs rhs >>= \case
        (ResBool l, ResBool r) -> pure $ ResBool (l `op` r)
        (l        , r        ) -> typeMismatch l r
    comparisonOp op lhs rhs = evalToPair lhs rhs >>= \case
        (l@(ResInt  _), r@(ResInt _) ) -> pure $ ResBool (l `op` r)
        (l@(ResBool _), r@(ResBool _)) -> pure $ ResBool (l `op` r)
        (l@(ResStr  _), r@(ResStr _) ) -> pure $ ResBool (l `op` r)
        (l            , r            ) -> typeMismatch l r
    equalityOp op lhs rhs = evalToPair lhs rhs >>= \case
        (l@(ResBool _), r@(ResBool _)) -> pure $ ResBool (l `op` r)
        (l@(ResInt  _), r@(ResInt _) ) -> pure $ ResBool (l `op` r)
        (l@ResUnit    , r@ResUnit    ) -> pure $ ResBool (l `op` r)
        (l@(ResStr _) , r@(ResStr _) ) -> pure $ ResBool (l `op` r)
        (l, r) -> fail $ mconcat ["Cannot compare ", show l, " and ", show r]
    typeMismatch lhs rhs =
        fail $ concat ["Type mismatch: ", show lhs, ", ", show rhs]
    errUnexpectedType expected value =
        fail $ concat
            ["Unexpected type: expected ", expected, ", got ", show value]
    errCannotIndex indexed index =
        fail $ mconcat ["Cannot use ", show index, "to index ", show indexed]

    errWrongArgumentCount :: Identifier -> Int -> Int -> Interpreter Result
    errWrongArgumentCount (Identifier fnName) expected actual = fail $ mconcat
        [ "Function '"
        , T.unpack fnName
        , "' expected "
        , show expected
        , " arguments, got "
        , show actual
        ]

    -- Evaluate two expressions to a pair so they can be pattern matched easily
    evalToPair :: Expr -> Expr -> Interpreter (Result, Result)
    evalToPair a b = (,) <$> eval a <*> eval b

    evalFnCall :: Identifier -> [Expr] -> Interpreter Result
    evalFnCall fnName args = do
        (FnDef params body) <- lookupFn fnName
        evaledArgs          <- traverse eval args
        let paramCount = length params
            argCount   = length evaledArgs
            argDefs    = zip params evaledArgs
        when (paramCount /= argCount)
            $ void (errWrongArgumentCount fnName paramCount argCount)
        -- TODO: function params get their own scope which is probably 
        -- unnecessary and causes (probably very slight) overhead, but shouldn't
        -- change the semantics.
        withinNewScope $ do
            mapM_ (uncurry defineVar) argDefs -- add parameter values to scope
            evalBlock body <* clearReturn

-- | Run the main program of the crate. Fails if main is not defined.
runProgram :: [String] -> Crate -> IO ()
runProgram args crate = void $ flip execStateT emptyEnv $ do
    initProgram args crate
    eval (FnCall (Identifier "main") [])

-- | Initialize the program state
initProgram :: [String] -> Crate -> Interpreter ()
initProgram args (Crate items) = do
    mapM_ defineItem items
    -- Define argument count and argument values implicitly as global variables
    let argc = fromIntegral $ length args
    argv <- liftIO $ newListArray (0, argc - 1) (ResStr . T.pack <$> args)
    defineVar (Identifier "argc") (ResInt argc)
    defineVar (Identifier "argv") (ResArr argv)

-- | Run a very bare bones REPL constantly asking for statements to execute. 
-- Not much of a P currently since it doesn't print anything, but user can call
-- the primitive functions to inspect program state.
runRepl :: [String] -> IO Statement -> IO ()
runRepl args prompt = void $ flip execStateT emptyEnv $ do
    initProgram args (Crate [])
    forever $ do
        stmt <- liftIO prompt
        flip catchError (liftIO . print) $ do
            addItem stmt
            execStatement stmt
