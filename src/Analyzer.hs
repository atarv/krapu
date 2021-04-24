{-|
Module         : Analysis
Description    : Static analysis (type checking etc.) for Krapu
Copyright      : (c) Aleksi Tarvainen, 2021
License        : BSD3
Maintainer     : aleksi@atarv.dev
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Analyzer where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Map.Strict                ( Map )
import           Data.Maybe
import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , (<|)
                                                )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )

import           AST

import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as T

-- | Data types of Krapu
data Type
    = TypeUnit
    | TypeI64
    | TypeStr
    | TypeBool
    | TypeArr Type
    deriving (Eq, Ord, Show)

-- | Display type in the way user should see it
display :: Type -> String
display = \case
    TypeUnit  -> "Unit"
    TypeI64   -> "I64"
    TypeBool  -> "Bool"
    TypeArr t -> show t <> "[]"

type Types = Map TypeName Type
type FnTypes = Map Identifier ([Type], Type)
type VarTypes = Map Identifier Type
data TypeEnv = TypeEnv { fnTypes :: FnTypes, varTypes :: VarTypes }

type TypeError = String
type Analyzer = StateT TypeEnv (Except TypeError)

-- | Instances of @Inferable@ can have their type inferred
class Inferable a where
    -- | Infer node's type
    infer :: a -> Analyzer Type

instance Inferable Expr where
    infer = inferExpr

instance Inferable Block where
    infer (Block _stmts outerExpr) = infer outerExpr

primitiveTypes :: Types
primitiveTypes = Map.fromList $ fmap
    (\t -> (TypeName . T.pack . display $ t, t))
    [TypeUnit, TypeI64, TypeBool, TypeStr]

initialEnv :: TypeEnv
initialEnv = TypeEnv fnTypes varTypes
  where
    fnTypes = Map.fromList
        [ (Identifier "debug_env" , ([], TypeUnit))
        , (Identifier "println"   , ([TypeStr], TypeUnit))
        , (Identifier "i64_to_str", ([TypeI64], TypeStr))
        , (Identifier "str_to_i64", ([TypeStr], TypeI64))
        ]
    varTypes = Map.fromList
        [(Identifier "argc", TypeI64), (Identifier "argv", TypeArr TypeStr)]

lookupVarType :: Identifier -> Analyzer Type
lookupVarType idf@(Identifier name) = do
    vars <- gets varTypes
    case Map.lookup idf vars of
        Nothing -> throwError $ "Variable '" <> T.unpack name <> "' not found"
        Just t  -> pure t

lookupFnType :: Identifier -> Analyzer ([Type], Type)
lookupFnType idf@(Identifier name) = do
    fns <- gets fnTypes
    case Map.lookup idf fns of
        Nothing -> throwError $ "Function '" <> T.unpack name <> "' not found"
        Just t  -> pure t

errWrongType :: Show a => Type -> Type -> a -> Analyzer b
errWrongType expected actual expr = throwError $ mconcat
    [ "Expected type "
    , display expected
    , " but actual was "
    , display actual
    , " from expression "
    , show expr
    ]

inferExpr :: Expr -> Analyzer Type
inferExpr = \case
    -- Literals
    Unit                    -> pure TypeUnit
    BoolLit  _              -> pure TypeBool
    IntLit   _              -> pure TypeI64
    Str      _              -> pure TypeStr
    ArrayLit []             -> throwError "Cannot infer type of an empty array"
    ArrayLit (elem : elems) -> do
        elemType <- inferExpr elem
        mapM_ (check elemType) elems
        pure elemType
    -- Variables
    Var    idf  -> lookupVarType idf
    -- Arithmetic
    Negate expr -> inferExpr expr >>= \case
        TypeI64 -> pure TypeI64
        wrong   -> errWrongType TypeI64 wrong expr
    Plus expr -> inferExpr expr >>= \case
        TypeI64 -> pure TypeI64
        wrong   -> errWrongType TypeI64 wrong expr
    lhs :+  rhs         -> inferNumericBinExpr lhs rhs
    lhs :-  rhs         -> inferNumericBinExpr lhs rhs
    lhs :*  rhs         -> inferNumericBinExpr lhs rhs
    lhs :/  rhs         -> inferNumericBinExpr lhs rhs
    -- Boolean 
    lhs :&& rhs         -> inferBoolBinExpr lhs rhs
    lhs :|| rhs         -> inferBoolBinExpr lhs rhs
    Not expr            -> inferExpr expr
    -- Comparison
    lhs    :>  rhs      -> inferComparableBinExpr lhs rhs
    lhs    :<  rhs      -> inferComparableBinExpr lhs rhs
    lhs    :>= rhs      -> inferComparableBinExpr lhs rhs
    lhs    :<= rhs      -> inferComparableBinExpr lhs rhs
    lhs    :== rhs      -> inferEqBinExpr lhs rhs
    lhs    :!= rhs      -> inferEqBinExpr lhs rhs
    -- Expressions with block
    IfExpr ifs altBlock -> do
        let (conditions, block : blocks) = unzip ifs
        -- Every condition should have boolean type
        mapM_ (check TypeBool) conditions
        firstType <- infer block
        -- Every block should return same type
        case altBlock of
            Nothing -> TypeUnit <$ forM_
                (block : blocks)
                (\blck -> do
                    blockType <- infer blck
                    unless (blockType == TypeUnit)
                        $ throwError
                        $ "If expression must return unit, if else branch is \
                          \not defined. "
                        <> show blck
                )
            Just alt -> foldM checkTypesEqual firstType (blocks ++ [alt])
      where
        checkTypesEqual :: Type -> Block -> Analyzer Type
        checkTypesEqual prevType nextBlock = do
            nextType <- infer nextBlock
            when (prevType /= nextType)
                $ errWrongType prevType nextType nextBlock
            pure nextType
    ExprBlock block  -> infer block
    Loop      block  -> undefined -- TODO: infer break expressions' types
    While pred block -> do
        check TypeBool pred
        undefined -- TODO: infer break expressions' types
    -- Assignment
    lhs := rhs -> case lhs of
        Var var -> do
            varType <- lookupVarType var
            check varType rhs
            pure varType
        ArrayAccess (Var var) indexExpr -> do
            check TypeI64 indexExpr
            lookupVarType var >>= \case
                TypeArr t -> do
                    check t rhs
                    pure t
                nonArrayType ->
                    throwError
                        $  "Cannot index variable of type "
                        <> display nonArrayType
        ArrayAccess indexed indexExpr ->
            throwError $ "Cannot assign to expression " <> show indexed
    -- Function calls
    FnCall (Identifier "debug") _ ->
        -- Any number of arguments with any types may be debugged
        pure TypeUnit
    FnCall idf args -> do
        (paramTypes, retType) <- lookupFnType idf
        let paramCount = length paramTypes
            argCount   = length args
        when (argCount /= paramCount) $ throwError $ mconcat
            [ "Expected "
            , show paramCount
            , " arguments, got "
            , show argCount
            , " when calling "
            , show idf
            ]
        zipWithM_ check paramTypes args
        pure retType
    -- Misc
    ArrayAccess indexedExpr indexExpr -> do
        inferExpr indexedExpr >>= \case
            TypeArr t -> do
                check TypeI64 indexExpr
                pure t
            nonIndexable -> throwError $ mconcat
                [ "Cannot index a non-array type "
                , display nonIndexable
                , ", "
                , show indexedExpr
                ]

inferNumericBinExpr :: Expr -> Expr -> Analyzer Type
inferNumericBinExpr = inferBinExpr (Set.fromList [TypeI64])

inferBoolBinExpr :: Expr -> Expr -> Analyzer Type
inferBoolBinExpr = inferBinExpr (Set.fromList [TypeBool])

inferComparableBinExpr :: Expr -> Expr -> Analyzer Type
inferComparableBinExpr =
    inferBinExpr (Set.fromList [TypeBool, TypeI64, TypeStr])

inferEqBinExpr :: Expr -> Expr -> Analyzer Type
inferEqBinExpr =
    inferBinExpr (Set.fromList [TypeBool, TypeI64, TypeStr, TypeUnit])

inferBinExpr :: Set Type -> Expr -> Expr -> Analyzer Type
inferBinExpr allowedTypes lhs rhs = do
    lhsType <- inferExpr lhs
    if lhsType `Set.member` allowedTypes
        then do
            check lhsType rhs
            pure lhsType
        else throwError $ "Wrong type of expression " <> show lhs

-- | @check t node@ checks that @node@ belonging to AST has given type @t@ 
-- throwing an error if it is not.
check :: (Inferable a, Show a) => Type -> a -> Analyzer ()
check expected expr = do
    actual <- infer expr
    when (expected /= actual) $ errWrongType expected actual expr

-- | TODO: is this useful?
analyzeExpr :: Expr -> IO ()
analyzeExpr expr = do
    let typ = evalStateT (inferExpr expr) initialEnv
    print typ
