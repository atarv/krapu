{-|
Module         : AST
Description    : Abstract syntax tree of Krapu language
Copyright      : (c) Aleksi Tarvainen, 2021
License        : BSD3
Maintainer     : a.aleksi.tarvainen@student.jyu.fi
-}
module AST where

data Expr
    = IntLit Integer
    | BoolLit Bool
    | UnaryOp UnaryOperator Expr
    | BinaryOp Operator Expr Expr
    deriving (Show, Eq)

-- | Binary operators
data Operator
    -- Arithmetic
    = Add -- ^ Addition
    | Sub -- ^ Subtraction
    | Mul -- ^ Multiplication
    | Div -- ^ Division
    -- Boolean
    | And -- ^ Lazy AND
    | Or  -- ^ Lazy OR
    -- Comparison
    | Greater
    | GreaterOrEqual
    | Lesser
    | LesserOrEqual
    | Equal
    | NotEqual
    deriving (Show, Eq)

data UnaryOperator
    = Negate
    | Plus
    | Not -- ^ Logical NOT
    deriving (Show, Eq)
