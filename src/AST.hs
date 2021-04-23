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
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module AST where

import           Data.Functor.Foldable.TH       ( makeBaseFunctor )
import           Data.Text                      ( Text )
import           Data.List.NonEmpty             ( NonEmpty )


-- | Following terminology of Rust, a crate is a package that compiles to a 
-- single library or an executable. For now, there will be no modules in Krapu
-- so everything will be in a single namespace.
newtype Crate = Crate [Item] deriving Show

-- | Type represents type names
newtype TypeName = TypeName Text deriving (Show, Eq, Ord)

-- | Identifiers are the names of variables, constants, parameters etc.
newtype Identifier = Identifier Text deriving (Show, Eq, Ord)

-- | Item is a single component of a crate. Items shouldn't change during
-- runtime.
data Item = Function Identifier [Parameter] TypeName Block deriving (Show, Eq)

-- | Function parameter
type Parameter = (Identifier, TypeName)

-- | Block groups statements together. It also forms a new scope for it's 
-- contents.
data Block
    -- | Outer expression will determine the return value of the block
    = Block [Statement] Expr
    deriving (Show, Eq)

-- | Single statement
data Statement
    = StatementEmpty
    | StatementItem Item
    | StatementExpr Expr
    | StatementLet Identifier TypeName Expr
    | StatementReturn Expr
    | StatementBreak Expr
    deriving (Show, Eq)

data Expr
    -- Literals
    = Unit -- ^ Same as @()@ in Haskell and Rust
    | IntLit Integer
    | BoolLit Bool
    | Str Text
    | ArrayLit [Expr]
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
    | IfExpr [(Expr, Block)] (Maybe Block)
    | ExprBlock Block
    | Loop Block
    | While Expr Block
    -- Assignment
    | Expr := Expr
    -- Misc
    | FnCall Identifier [Expr]
    | ArrayAccess Expr Expr
    deriving (Show, Eq)

makeBaseFunctor ''Expr
makeBaseFunctor ''Statement
