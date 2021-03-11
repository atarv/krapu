{-|
Module         : AST
Description    : Abstract syntax tree of Krapu
Copyright      : (c) Aleksi Tarvainen, 2021
License        : BSD3
Maintainer     : a.aleksi.tarvainen@student.jyu.fi
-}
module AST where

data Expr
    = IntLit Integer -- ^ Integer literal
    | BoolLit Bool
    | UnaryOp UnaryOperator Expr
    | BinaryOp Operator Expr Expr
    deriving (Show, Eq)

-- | Binary operators
data Operator
    = Add -- ^ Addition
    | Sub -- ^ Subtraction
    | Mul -- ^ Multiplication
    | Div -- ^ Division
    | And -- ^ Lazy logical AND
    | Or -- ^ Lazy logical OR
    deriving (Show, Eq)

data UnaryOperator
    = Negate
    | Plus
    | Not -- ^ Logical NOT
    deriving (Show, Eq)
