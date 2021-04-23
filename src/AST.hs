{-|
Module         : AST
Description    : Abstract syntax tree of Krapu language
Copyright      : (c) Aleksi Tarvainen, 2021
License        : BSD3
Maintainer     : a.aleksi.tarvainen@student.jyu.fi

Based on Rust reference <https://doc.rust-lang.org/reference/>, though a lot
of corners were cut.
-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE FlexibleInstances #-}

module AST where

import           Data.Text                      ( Text )

import           Fix                            

import qualified Data.Text as T


-- | Following terminology of Rust, a crate is a package that compiles to a 
-- single library or an executable. For now, there will be no modules in Krapu
-- so everything will be in a single namespace.
newtype Crate expr = Crate [Item expr]
    deriving (Show, Functor, Foldable, Traversable)

-- | Type represents type names
newtype Type = Type Text deriving (Show, Eq, Ord)

-- | Identifiers are the names of variables, constants, parameters etc.
newtype Identifier = Identifier Text deriving (Show, Eq, Ord)

-- | Item is a single component of a crate. Items shouldn't change during
-- runtime.
data Item expr = Function Identifier [Parameter] Type (Block expr)
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Function parameter
type Parameter = (Identifier, Type)

-- | Block groups statements together. It also forms a new scope for it's 
-- contents.
data Block expr
    -- | Outer expression will determine the return value of the block
    = Block [Statement expr] expr
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Single statement
data Statement expr
    = StatementEmpty
    | StatementItem (Item expr)
    | StatementExpr expr
    | StatementLet Identifier Type expr
    | StatementReturn expr
    | StatementBreak expr
    deriving (Show, Eq, Functor, Foldable, Traversable)

data ExprF expr
    -- Literals
    = UnitLiteral -- ^ Same as @()@ in Haskell and Rust
    | IntLiteral Integer
    | BoolLiteral Bool
    | StringLiteral Text
    | ArrayLiteral [expr]
    -- Variables
    | Variable Identifier
    -- Arithmetic
    | NegateExpression expr
    | PlusExpression expr
    | Add expr expr
    | Sub expr expr
    | Mul expr expr
    | Div expr expr
    -- Boolean
    | And expr expr
    | Or expr expr
    | BoolNot expr
    -- Comparison
    | GrT expr expr
    | GrTE expr expr
    | LsT expr expr
    | LsTE expr expr
    | Equal expr expr
    | NotEqual expr expr
    -- Expressions with block
    | IfExpression [(expr, Block expr)] (Maybe (Block expr))
    | ExpressionBlock (Block expr)
    | LoopExpression (Block expr)
    | WhileExpression expr (Block expr)
    -- Assignment
    | Assign expr expr
    -- Misc
    | FunctionCall Identifier [expr]
    | ArrayAccessExpression expr expr
    deriving (Show, Eq, Functor, Foldable, Traversable)

pattern Unit = Fix UnitLiteral
pattern IntLit i = Fix (IntLiteral i)
pattern BoolLit b = Fix (BoolLiteral b)
pattern Str str = Fix (StringLiteral str)
pattern ArrayLit arr = Fix (ArrayLiteral arr)
-- Variables
pattern Var idf = Fix (Variable idf)
-- Arithmetic
pattern Negate expr = Fix (NegateExpression expr)
pattern Plus expr = Fix (PlusExpression expr)
pattern lhs :+ rhs = Fix (Add lhs rhs)
pattern lhs :- rhs = Fix (Sub lhs rhs)
pattern lhs :* rhs = Fix (Mul lhs rhs)
pattern lhs :/ rhs = Fix (Div lhs rhs)
-- Boolean
pattern lhs :&& rhs = Fix (And lhs rhs)
pattern lhs :|| rhs = Fix (Or lhs rhs)
pattern Not expr = Fix (BoolNot expr)
-- Comparison
pattern lhs :> rhs = Fix (GrT lhs rhs)
pattern lhs :>= rhs = Fix (GrTE lhs rhs)
pattern lhs :< rhs = Fix (LsT lhs rhs)
pattern lhs :<= rhs = Fix (LsTE lhs rhs)
pattern lhs :== rhs = Fix (Equal lhs rhs)
pattern lhs :!= rhs = Fix (NotEqual lhs rhs)
-- Expressions with block
pattern IfExpr ifs altBlock = Fix (IfExpression ifs altBlock)
pattern ExprBlock block = Fix (ExpressionBlock block)
pattern Loop body = Fix (LoopExpression body)
pattern While pred body = Fix (WhileExpression pred body)
-- Assignment
pattern lhs := rhs = Fix (Assign lhs rhs)
-- Misc
pattern FnCall idf args = Fix (FunctionCall idf args)
pattern ArrayAccess indexed index = Fix (ArrayAccessExpression indexed index)

instance Show (Fix ExprF) where
    show = cata alg
        where 
            alg UnitLiteral = parens "UnitLiteral"
            alg (IntLiteral i) = parens $ "IntLiteral " <> show i
            alg (BoolLiteral b) = parens $ "BoolLiteral " <> show b
            alg (StringLiteral str) = parens $ "StringLiteral " <> T.unpack str
            alg (ArrayLiteral args) = parens $ "ArrayLiteral " <> show args
            -- Variables
            alg (Variable idf) = parens $ "Variable " <> parens (show idf)
            -- Arithmetic
            alg (NegateExpression expr) = 
                parens $ "NegatedExpression " <> show expr
            alg (PlusExpression expr) = parens $ "PlusExpression" <> show expr
            alg (Add lhs rhs) = parens $ "Add " <> show lhs <> " " <> show rhs
            alg (Sub lhs rhs) = parens $ "Sub " <> show lhs <> " " <> show rhs
            alg (Mul lhs rhs) = parens $ "Mul " <> show lhs <> " " <> show rhs
            alg (Div lhs rhs) = parens $ "Div " <> show lhs <> " " <> show rhs
            -- Boolean
            alg (And lhs rhs) = parens $ "And " <> show lhs <> " " <> show rhs
            alg (Or lhs rhs) = parens $ "Or " <> show lhs <> " " <> show rhs
            alg (BoolNot expr) = parens $ "BoolNot " <> show expr
            -- Comparison
            alg (GrT lhs rhs) = parens $ "GrT " <> show lhs <> " " <> show rhs 
            alg (GrTE lhs rhs) = parens $ "GrTE " <> show lhs <> " " <> show rhs
            alg (LsT lhs rhs) = parens $ "LsT " <> show lhs <> " " <> show rhs
            alg (LsTE lhs rhs) = parens $ "LsTE " <> show lhs <> " " <> show rhs 
            alg (Equal lhs rhs) = 
                parens $ "Equal " <> show lhs <> " " <> show rhs 
            alg (NotEqual lhs rhs) = 
                parens $ "NotEqual " <> show lhs <> " " <> show rhs 
            -- Expressions with block
            alg (IfExpression ifs altBlock) = 
                parens $ "IfExpression " <> show ifs <> " " <> show altBlock
            alg (ExpressionBlock block) = 
                parens $ "ExpressionBlock " <> show block
            alg (LoopExpression body) = parens $ "LoopExpression " <> show body
            alg (WhileExpression pred body) = 
                parens $ "WhileExpression " <> show pred <> " " <> show body
            -- Assignment
            alg (Assign lhs rhs) = 
                parens $ "Assign " <> show lhs <> " " <> show rhs
            -- Misc
            alg (FunctionCall idf args) = 
                parens $ "FunctionCall " <> show idf <> " " <> show args
            alg (ArrayAccessExpression indexed index) = 
                parens 
                    $ "ArrayAccessExpression " 
                    <> show indexed 
                    <> " " 
                    <> show index
            parens s = "(" <> s <> ")"

type Expr = Fix ExprF

