{-|
Module         : AST
Description    : Abstract syntax tree of Krapu language
Copyright      : (c) Aleksi Tarvainen, 2021
License        : BSD3
Maintainer     : a.aleksi.tarvainen@student.jyu.fi

Based on Rust reference <https://doc.rust-lang.org/reference/>, though a lot
of corners were cut.
-}
module AST where

import           Data.Text                      ( Text )


-- | Following terminology of Rust, a crate is a package that compiles to a 
-- single library or an executable. For now, there will be no modules in Krapu
-- so everything will be in a single namespace.
newtype Crate = Crate [Item] deriving Show

-- | Type represents type names
newtype Type = Type Text deriving (Show, Eq, Ord)

-- | Identifiers are the names of variables, constants, parameters etc.
newtype Identifier = Identifier Text deriving (Show, Eq, Ord)

-- | Item is a single component of a crate. Items shouldn't change during
-- runtime.
data Item = Function Identifier [Parameter] Type Block deriving (Show, Eq)

-- | Function parameter
type Parameter = (Identifier, Type)

-- | Block groups statements together. It also forms a new scope for it's 
-- contents.
data Block
    = Block [Statement]
    -- | Outer expression will determine the return value of the block
    | BlockExpr [Statement] Expr
    deriving (Show, Eq)

-- | Single statement
data Statement
    = StatementEmpty
    | StatementItem Item
    | StatementExpr Expr
    | StatementLet Identifier Type Expr
    deriving (Show, Eq)

-- | Expressions always produce a value and may perform side effects
data Expr
    -- Literals
    = Unit -- ^ Same as @()@ in Haskell and Rust
    | IntLit Integer
    | BoolLit Bool
    -- Variables
    | Var Identifier
    -- Arithmetic
    | Negate Expr
    | Plus Expr
    | Expr :+ Expr
    | Expr :- Expr
    | Expr :* Expr
    | Expr :/ Expr
    -- Boolean
    | Expr :&& Expr
    | Expr :|| Expr
    | Not Expr
    -- Comparison
    | Expr :> Expr
    | Expr :>= Expr
    | Expr :< Expr
    | Expr :<= Expr
    | Expr :== Expr
    | Expr :!= Expr
    -- Expressions with block
    | IfExpr Expr Block (Maybe Block)
    | ExprBlock Block
    deriving (Show, Eq)
