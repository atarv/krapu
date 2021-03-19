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

import           Control.Monad
import           Data.List                      ( foldl' )
import           Data.Text                      ( Text )

import           AST

import qualified Data.Text                     as T


-- Took some inspiration from one of the course's example projects: vt-2016

-- | Result types are only those that expressions evaluate to
data Result
    = ResUnit
    | ResInt Integer
    | ResBool Bool
    deriving (Show, Eq, Ord)

-- | Blocks evaluate to their outer expression's result, if they have one.
-- Otherwise they return a unit.
evalBlock :: Block -> Either Text Result
evalBlock = \case
    Block stmts               -> evalStatements stmts >> Right ResUnit
    BlockExpr stmts outerExpr -> evalStatements stmts >> eval outerExpr
  where
    evalStatements = foldl' (const execStatement) -- Previous statement's result is discarded
                            (Right ()) -- Default to unit in case block contains no statements

-- | Statements change the program environment (declare new variables and change
-- their values, define new functions etc.) rather than return values (as
-- opposed to expressions).
execStatement :: Statement -> Either Text ()
execStatement = \case
    StatementEmpty     -> Right ()
    StatementExpr expr -> eval expr >> Right ()
    StatementItem _ -> Left "Items cannot be evaluated yet" -- TODO:

-- | Evaluate an expression. There will always be a result and side effects may
-- be performed.
eval :: Expr -> Either Text Result
eval = \case
    -- Literals
    Unit        -> Right ResUnit
    IntLit  i   -> Right $ ResInt i
    BoolLit b   -> Right $ ResBool b

    -- Arithmetic
    lhs :+ rhs  -> binaryIntOp (+) lhs rhs
    lhs :- rhs  -> binaryIntOp (-) lhs rhs
    lhs :* rhs  -> binaryIntOp (*) lhs rhs
    lhs :/ rhs  -> binaryIntOp div lhs rhs
    Negate expr -> eval expr >>= \case
        (ResInt i) -> Right $ ResInt (-i)
        val        -> errUnexpectedType "integer" val
    Plus expr -> eval expr >>= \case
        (ResInt i) -> Right $ ResInt i
        val        -> errUnexpectedType "integer" val

    -- Boolean
    lhs :&& rhs -> binaryBoolOp (&&) lhs rhs
    lhs :|| rhs -> binaryBoolOp (||) lhs rhs
    Not expr    -> eval expr >>= \case
        ResBool b -> Right $ ResBool (not b)
        val       -> errUnexpectedType "bool" val

    -- Comparison
    lhs :<  rhs            -> comparisonOp (<) lhs rhs
    lhs :>  rhs            -> comparisonOp (>) lhs rhs
    lhs :<= rhs            -> comparisonOp (<=) lhs rhs
    lhs :>= rhs            -> comparisonOp (>=) lhs rhs

    -- Equality
    lhs :== rhs            -> equalityOp (==) lhs rhs
    lhs :!= rhs            -> equalityOp (/=) lhs rhs

    -- Expressions with blocks
    IfExpr cond conseq alt -> eval cond >>= \case
        ResBool b ->
            if b then evalBlock conseq else maybe (Right ResUnit) evalBlock alt
        val -> errUnexpectedType "bool" val
    ExprBlock block -> evalBlock block
  where
    binaryIntOp op lhs rhs = evalToPair lhs rhs >>= \case
        (ResInt l, ResInt r) -> Right $ ResInt (l `op` r)
        (l       , r       ) -> typeMismatch l r
    binaryBoolOp op lhs rhs = evalToPair lhs rhs >>= \case
        (ResBool l, ResBool r) -> Right $ ResBool (l `op` r)
        (l        , r        ) -> typeMismatch l r
    comparisonOp op lhs rhs = evalToPair lhs rhs >>= \case
        (l@(ResInt _), r@(ResInt _)) -> Right $ ResBool (l `op` r)
        (l           , r           ) -> typeMismatch l r
    equalityOp op lhs rhs = evalToPair lhs rhs >>= \case
        (l@(ResBool _), r@(ResBool _)) -> Right $ ResBool (l `op` r)
        (l@(ResInt  _), r@(ResInt _) ) -> Right $ ResBool (l `op` r)
        (l            , r            ) -> typeMismatch l r
    typeMismatch lhs rhs =
        Left $ "Type mismatch: " <> T.pack (show lhs) <> ", " <> T.pack
            (show rhs)
    errUnexpectedType expected value =
        Left $ "Unexpected type: expected " <> expected <> ", got " <> T.pack
            (show value)
    -- Evaluate two expressions to a pair so they can be pattern matched easily
    evalToPair :: Expr -> Expr -> Either Text (Result, Result)
    evalToPair a b = liftM2 (,) (eval a) (eval b)
