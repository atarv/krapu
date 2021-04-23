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

module AST where

import           Data.Text                      ( Text )

import           Fix                            ( Fix )


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
    = Unit -- ^ Same as @()@ in Haskell and Rust
    | IntLit Integer
    | BoolLit Bool
    | Str Text
    | ArrayLit [expr]
    -- Variables
    | Var Identifier
    -- Arithmetic
    | Negate expr
    | Plus expr
    | expr :+ expr
    | expr :- expr
    | expr :* expr
    | expr :/ expr
    -- Boolean
    | expr :&& expr
    | expr :|| expr
    | Not expr
    -- Comparison
    | expr :> expr
    | expr :>= expr
    | expr :< expr
    | expr :<= expr
    | expr :== expr
    | expr :!= expr
    -- Expressions with block
    | IfExpr [(expr, Block expr)] (Maybe (Block expr))
    | ExprBlock (Block expr)
    | Loop (Block expr)
    | While expr (Block expr)
    -- Assignment
    | expr := expr
    -- Misc
    | FnCall Identifier [expr]
    | ArrayAccess expr expr
    deriving (Show, Eq, Functor, Foldable, Traversable)

type Expr = Fix ExprF
