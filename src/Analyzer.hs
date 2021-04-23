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

data Type
    = TypeUnit
    | TypeI64
    | TypeStr
    | TypeBool
    | TypeArr Type
    deriving (Eq, Ord)

instance Show Type where
    show = \case
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

primitiveTypes :: Types
primitiveTypes = Map.fromList $ fmap
    (\t -> (TypeName . T.pack . show $ t, t))
    [TypeUnit, TypeI64, TypeBool, TypeStr]

-- TODO: Add primitive function types
initialEnv :: TypeEnv
initialEnv = TypeEnv Map.empty $ Map.fromList
    [(Identifier "argc", TypeI64), (Identifier "argv", TypeArr TypeStr)]

lookupVarType :: Identifier -> Analyzer Type
lookupVarType idf@(Identifier name) = do
    vars <- gets varTypes
    case Map.lookup idf vars of
        Nothing -> throwError $ "Variable '" <> T.unpack name <> "' not found"
        Just t  -> pure t

errWrongType :: Show a => Type -> Type -> a -> Analyzer b
errWrongType expected actual expr = throwError $ mconcat
    [ "Expected type "
    , show expected
    , " but actual was "
    , show actual
    , " from expression "
    , show expr
    ]

inferBlock :: Block -> Analyzer Type
inferBlock (Block _stmts outerExpr) = inferExpr outerExpr

inferExpr :: Expr -> Analyzer Type
inferExpr = \case
    -- Literals
    Unit         -> pure TypeUnit
    BoolLit _    -> pure TypeBool
    IntLit  _    -> pure TypeI64
    Str     _    -> pure TypeStr
    -- TODO: arrays
    -- Variables
    Var     idf  -> lookupVarType idf
    -- Arithmetic
    Negate  expr -> inferExpr expr >>= \case
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
        mapM_ (checkExpr TypeBool) conditions
        firstType <- inferBlock block
        let allBlocks = maybe blocks ((blocks ++) . (: [])) altBlock
        -- Every block should return same type
        foldM checkTypesEqual firstType allBlocks
      where
        checkTypesEqual :: Type -> Block -> Analyzer Type
        checkTypesEqual prevType nextBlock = do
            nextType <- inferBlock nextBlock
            when (prevType /= nextType)
                $ errWrongType prevType nextType nextBlock
            pure nextType
    ExprBlock block  -> inferBlock block
    Loop      block  -> undefined -- TODO: infer break expressions' types
    While pred block -> do
        checkExpr TypeBool pred
        undefined -- TODO: infer break expressions' types
    -- Assignment
    lhs := rhs -> case lhs of
        Var var -> do
            varType <- lookupVarType var
            checkExpr varType rhs
            pure varType
        ArrayAccess (Var var) indexExpr -> do
            checkExpr TypeI64 indexExpr
            lookupVarType var >>= \case
                TypeArr t -> do
                    checkExpr t rhs
                    pure t
                nonArrayType ->
                    throwError
                        $  "Cannot index variable of type "
                        <> show nonArrayType
        ArrayAccess indexed indexExpr ->
            throwError $ "Cannot assign to expression " <> show indexed
    -- TODO: FnCall
    ArrayAccess indexedExpr indexExpr -> do
        inferExpr indexedExpr >>= \case
            TypeArr t -> do
                checkExpr TypeI64 indexExpr
                pure t
            nonIndexable -> throwError $ mconcat
                [ "Cannot index a non-array type "
                , show nonIndexable
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
            checkExpr lhsType rhs
            pure lhsType
        else throwError $ "Wrong type of expression " <> show lhs


-- | @checkExpr t expr@ checks that expression @expr@ is of given type @t@ 
-- throwing an error if it is not.
checkExpr :: Type -> Expr -> Analyzer ()
checkExpr expected expr = do
    actual <- inferExpr expr
    when (expected /= actual) $ errWrongType expected actual expr

-- | TODO: is this useful?
analyzeExpr :: Expr -> IO ()
analyzeExpr expr = do
    let typ = evalStateT (inferExpr expr) initialEnv
    print typ
