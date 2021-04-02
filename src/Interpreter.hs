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
import           Data.Foldable                  ( traverse_ )
import           Data.Functor
import           Data.IORef
import           Data.List                      ( intercalate )
import           Data.Map.Strict                ( Map )

import           AST

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
type SymTable = [Map Identifier (IORef Result)]

-- | Environment of the interpreted program
type Env = IORef SymTable

-- | Creates a blank environment
emptyEnv :: IO Env
emptyEnv = newIORef [Map.empty]

-- | @findVarRef e i@ Find reference to value of the variable which identifier 
-- @i@ refers to (in environment @e@).
findVarRef :: Env -> Identifier -> IO (Maybe (IORef Result))
findVarRef envRef idf = do
    readIORef envRef <&> foldr lookVar Nothing
  where
    lookVar vars found = case idf `Map.lookup` vars of
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
defineVar envRef idf val = do
    valRef <- newIORef val
    modifyIORef' envRef (addVar valRef)
    pure envRef
  where
    addVar valRef (ctx : ctxs) = Map.insert idf valRef ctx : ctxs
    addVar valRef []           = [Map.insert idf valRef Map.empty]

-- | Assign value to variable
assignVar :: Env -> Identifier -> Result -> IO Env
assignVar envRef idf val = do
    findVarRef envRef idf >>= \case
        Just valRef -> modifyIORef' valRef (const val)
        Nothing ->
            let (Identifier var) = idf
            in  fail $ "Variable '" <> T.unpack var <> "' not found"
    pure envRef

-- | @withinNewScope env a@ Execute given action @a@ in a new scope added to 
-- the environment @env@. Once the action is complete, scope is exited 
-- automatically.
withinNewScope :: Env -> (Env -> IO b) -> IO b
withinNewScope env = bracket (beginScope env) exitScope
  where
    beginScope env = do
        modifyIORef' env (Map.empty :)
        pure env
    exitScope env = do
        -- Using tail here should be safe since a new scope is always added 
        -- before exiting
        modifyIORef' env tail
        pure env

-- | Print environment starting from the innermost context
printEnv :: Env -> IO ()
printEnv env = readIORef env >>= traverse showValVarPairs >>= putStr . separate
  where
    separate        = intercalate "---\n"
    showValVarPairs = Map.foldMapWithKey
        (\(Identifier idf) valRef -> do
            val <- readIORef valRef
            return $ T.unpack idf <> " = " <> show val <> "\n"
        )

-- | Blocks evaluate to their outer expression's result.
evalBlock :: Env -> Block -> IO (Env, Result)
evalBlock env = \case
    Block stmts outerExpr ->
        withinNewScope env (flip execStatements stmts >=> flip eval outerExpr)
    where execStatements = foldM execStatement

-- | Statements change the program environment (declare new variables and change
-- their values, define new functions etc.) rather than return values (as
-- opposed to expressions).
execStatement :: Env -> Statement -> IO Env
execStatement env = \case
    StatementEmpty              -> pure env
    StatementExpr expr          -> fst <$> eval env expr
    StatementItem _             -> fail "Items cannot be used yet" -- TODO:
    StatementLet idf _type expr -> do
        (env', result) <- eval env expr
        defineVar env' idf result

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
    IfExpr [] (Just alt) -> evalBlock env alt
    IfExpr [] Nothing    -> pure (env, ResUnit)
    ExprBlock block      -> evalBlock env block
    While condition body -> evalWhile env condition body
    Loop  body           -> evalWhile env (BoolLit True) body

    -- Misc
    Break _expr          -> fail "break not implemented"
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
