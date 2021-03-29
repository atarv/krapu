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
import           Data.IORef
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

-- | Lookup a variable's value. Fails if variable is not defined yet.
lookupVar :: Env -> Identifier -> IO Result
lookupVar envRef idf = do
    env <- readIORef envRef
    case foldr lookVar Nothing env of
        Just valRef -> readIORef valRef
        Nothing ->
            let (Identifier var) = idf
            in  fail $ "Variable '" <> T.unpack var <> "' not found"
  where
    lookVar vars found = case Map.lookup idf vars of
        Nothing -> found
        valRef  -> valRef

-- | Define a new variable and it's value. Note that there is no check for
--  existing values, because shadowing variables is allowed.
defineVar :: Env -> Identifier -> Result -> IO Env
defineVar envRef idf val = do
    valRef <- newIORef val
    modifyIORef' envRef (addVar valRef)
    pure envRef
  where
    addVar valRef (env : envs) = Map.insert idf valRef env : envs
    addVar valRef []           = [Map.insert idf valRef Map.empty]

-- | @withinNewScope env a@ Execute given action @a@ in a new scope added to 
-- the environment @env@. Once the action is complete, scope is exited 
-- automatically.
withinNewScope :: Env -> (Env -> IO b) -> IO b
withinNewScope env = bracket (beginScope env) exitScope
  where
    beginScope env = do
        modifyIORef env (Map.empty :)
        pure env
    exitScope env = do
        -- Using tail here should be safe since a new scope is always added 
        -- before exiting
        modifyIORef' env tail
        pure env

-- | Print environment starting from the innermost context
printEnv :: Env -> IO ()
printEnv env = do
    -- TODO: cleanup the implementation
    env' <- readIORef env
    forM_ env' $ \context ->
        let varValPairs = Map.toDescList context
        in
            do
                putStrLn "----"
                forM_ varValPairs $ \(Identifier i, valRef) ->
                    readIORef valRef >>= \val ->
                        putStrLn (T.unpack i <> " = " <> show val)

-- | Blocks evaluate to their outer expression's result, if they have one.
-- Otherwise they return a unit.
evalBlock :: Env -> Block -> IO (Env, Result)
evalBlock env = \case
    Block stmts -> (, ResUnit) <$> withinNewScope env (`evalStatements` stmts)
    BlockExpr stmts outerExpr ->
        withinNewScope env (flip evalStatements stmts >=> flip eval outerExpr)
    where evalStatements = foldM execStatement

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
    lhs :<  rhs            -> comparisonOp env (<) lhs rhs
    lhs :>  rhs            -> comparisonOp env (>) lhs rhs
    lhs :<= rhs            -> comparisonOp env (<=) lhs rhs
    lhs :>= rhs            -> comparisonOp env (>=) lhs rhs

    -- Equality
    lhs :== rhs            -> equalityOp env (==) lhs rhs
    lhs :!= rhs            -> equalityOp env (/=) lhs rhs

    -- Expressions with blocks
    IfExpr cond conseq alt -> eval env cond >>= \case
    -- TODO: begin new blocks by adding a new scope
        (env', ResBool b) -> if b
            then evalBlock env' conseq
            else maybe (pure (env', ResUnit)) (evalBlock env') alt
        (_, val) -> errUnexpectedType "bool" val
    ExprBlock block -> evalBlock env block
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
