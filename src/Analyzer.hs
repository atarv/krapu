{-|
Module         : Analyzer
Description    : Static analysis (type checking etc.) for Krapu
Copyright      : (c) Aleksi Tarvainen, 2021
License        : BSD3
Maintainer     : a.aleksi.tarvainen@student.jyu.fi
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Analyzer where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Functor.Foldable          ( cata )
import           Data.Map.Strict                ( Map )
import           Data.Maybe
import           Data.List                      ( intercalate )
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
    TypeStr   -> "Str"
    TypeArr t -> display t <> "[]"

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

getType :: TypeName -> Analyzer Type
getType (TypeName name) = do
    let elem     = TypeName $ T.takeWhile (not . isBracket) name
        brackets = T.takeWhileEnd isBracket name
        arrLevel = T.length brackets `div` 2
    case Map.lookup elem primitiveTypes of
        Nothing -> throwError $ "Could not find type " <> T.unpack name
        Just t  -> pure $ foldr ($) t $ replicate arrLevel TypeArr
    where isBracket c = c == '[' || c == ']'

-- | Lookup variable's type from analyzer's environment
lookupVarType :: Identifier -> Analyzer Type
lookupVarType idf@(Identifier name) = do
    vars <- gets varTypes
    case Map.lookup idf vars of
        Nothing -> throwError $ "Variable '" <> T.unpack name <> "' not found"
        Just t  -> pure t

-- | Lookup function's parameter's types and it's return type from analyzer's 
-- environment
lookupFnType :: Identifier -> Analyzer ([Type], Type)
lookupFnType idf@(Identifier name) = do
    fns <- gets fnTypes
    case Map.lookup idf fns of
        Nothing -> throwError $ "Function '" <> T.unpack name <> "' not found"
        Just t  -> pure t

declareVar :: Identifier -> Type -> Analyzer ()
declareVar idf t =
    modify' $ \env -> env { varTypes = Map.insert idf t $ varTypes env }

-- | TODO: check if already declared?
declareFn :: Identifier -> [Type] -> Type -> Analyzer ()
declareFn idf paramTypes retType = modify' $ \env ->
    env { fnTypes = Map.insert idf (paramTypes, retType) $ fnTypes env }

errWrongType :: Show a => Type -> Type -> a -> Analyzer b
errWrongType expected actual expr = throwError $ mconcat
    [ "Expected type "
    , display expected
    , " but actual was "
    , display actual
    , " from expression "
    , show expr
    ]

-- | Infer the type of an expression
-- 
-- >>> flip evalStateT initialEnv $ inferExpr (IntLit 0 :< IntLit 2)
-- ExceptT (Identity (Right TypeBool))
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
        pure $ TypeArr elemType
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
    ExprBlock block -> infer block
    Loop      block -> case pickBreakExprs block of
        []     -> pure TypeUnit -- In this case the loop is endless
        breaks -> do
            expectedBrkType <- infer $ head breaks
            forM_ (tail breaks) $ \break -> do
                brkType <- infer break
                unless (brkType == expectedBrkType) $ throwError $ mconcat
                    [ "Expected break expression with type "
                    , show expectedBrkType
                    , ", actual "
                    , show brkType
                    , " in "
                    , show block
                    ]
            pure expectedBrkType
    While pred block -> do
        check TypeBool pred
        case pickBreakExprs block of
            []     -> pure TypeUnit
            -- Breaking from while loop is permitted only with unit value
            breaks -> TypeUnit <$ mapM_ (check TypeUnit) breaks
    -- Assignment
    lhs := rhs -> case lhs of
        Var var -> do
            varType <- lookupVarType var
            check varType rhs
            pure varType
        ArrayAccess (Var var) indexExpr -> do
            check TypeI64 indexExpr
            lookupVarType var >>= \case
                TypeArr t -> check t rhs >> pure t
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
inferComparableBinExpr lhs rhs =
    TypeBool <$ inferBinExpr (Set.fromList [TypeBool, TypeI64, TypeStr]) lhs rhs

inferEqBinExpr :: Expr -> Expr -> Analyzer Type
inferEqBinExpr lhs rhs =
    TypeBool
        <$ inferBinExpr
               (Set.fromList [TypeBool, TypeI64, TypeStr, TypeUnit])
               lhs
               rhs

-- | @inferBinExpr allowedTypes e1 e2@ checks that both expressions @e1@ and 
-- @e2@ share a type that is in the set @allowedTypes@. Returns expressions' 
-- inferred type.
inferBinExpr :: Set Type -> Expr -> Expr -> Analyzer Type
inferBinExpr allowedTypes lhs rhs = do
    lhsType <- inferExpr lhs
    if lhsType `Set.member` allowedTypes
        then do
            check lhsType rhs
            pure lhsType
        else throwError $ mconcat
            [ "Excpected "
            , show lhs
            , " to be one of "
            , intercalate ", " (show <$> Set.toAscList allowedTypes)
            ]

pickBreakExprs :: Block -> [Expr]
pickBreakExprs (Block stmts _) = mconcat $ fmap (cata alg) stmts
  where
    alg :: StatementF [Expr] -> [Expr]
    alg (StatementBreakF expr) =
        -- Take into account both what the block evaluates to and the possible
        -- break statements inside it
        expr : mconcat (pickBreakExprs <$> foldMap pickNonLoopBlocks [expr])
    alg (StatementExprF expr) =
        mconcat $ pickBreakExprs <$> foldMap pickNonLoopBlocks [expr]
    alg (StatementLetF _ _ expr) =
        mconcat $ pickBreakExprs <$> foldMap pickNonLoopBlocks [expr]
    alg (StatementReturnF expr) =
        mconcat $ pickBreakExprs <$> foldMap pickNonLoopBlocks [expr]
    alg _ = []

pickNonLoopBlocks :: Expr -> [Block]
pickNonLoopBlocks = cata alg
  where
    alg :: ExprF [Block] -> [Block]
    alg (IfExprF ifs altBlock) = case altBlock of
        Nothing ->
            let (conditions, conseqs) = unzip ifs
            in  mconcat conditions ++ conseqs
        Just alt ->
            let (conditions, conseqs) = unzip ifs
            in  alt : mconcat conditions ++ conseqs
    alg (ExprBlockF block      ) = [block]
    alg (NegateF    expr       ) = expr
    alg (PlusF      expr       ) = expr
    alg (NotF       expr       ) = expr
    alg (ArrayLitF  expr       ) = mconcat expr
    alg (lhs          :+$  rhs ) = lhs ++ rhs
    alg (lhs          :-$  rhs ) = lhs ++ rhs
    alg (lhs          :/$  rhs ) = lhs ++ rhs
    alg (lhs          :*$  rhs ) = lhs ++ rhs
    alg (lhs          :==$ rhs ) = lhs ++ rhs
    alg (lhs          :!=$ rhs ) = lhs ++ rhs
    alg (lhs          :=$  rhs ) = lhs ++ rhs
    alg (lhs          :||$ rhs ) = lhs ++ rhs
    alg (lhs          :&&$ rhs ) = lhs ++ rhs
    alg (lhs          :>$  rhs ) = lhs ++ rhs
    alg (lhs          :>=$ rhs ) = lhs ++ rhs
    alg (lhs          :<$  rhs ) = lhs ++ rhs
    alg (lhs          :<=$ rhs ) = lhs ++ rhs
    alg (ArrayAccessF idxd idx ) = idxd ++ idx
    alg (FnCallF      _    args) = mconcat args
    alg _                        = []

-- TODO: move this example block to tests or something
ex :: Block
ex = Block
    [ StatementEmpty
    , StatementBreak Unit
    , StatementExpr $ IfExpr
        [ ( BoolLit True
          , Block
              [ StatementBreak (IntLit 1)
              , StatementLet
                  (Identifier "foo")
                  (Just $ TypeName "I64")
                  (ExprBlock $ Block [StatementBreak $ BoolLit False] Unit)
              , StatementBreak $ ExprBlock $ Block
                  [StatementBreak $ IntLit 42, StatementBreak Unit]
                  Unit
              ]
              Unit
          )
        ]
        Nothing
    ]
    Unit

-- | @check t node@ checks that @node@ belonging to AST has given type @t@ 
-- throwing an error if it is not.
check :: (Inferable a, Show a) => Type -> a -> Analyzer ()
check expected expr = do
    actual <- infer expr
    when (expected /= actual) $ errWrongType expected actual expr

declareItem :: Item -> Analyzer ()
declareItem = \case
    Function fnName params retType _body -> do
        paramTypes <- traverse getType (snd <$> params)
        retType'   <- getType retType
        declareFn fnName paramTypes retType'

checkItem :: Item -> Analyzer ()
checkItem = \case
    Function fnName params retType body -> do
        retType' <- getType retType
        check retType' body

checkStatement :: Statement -> Analyzer ()
checkStatement = \case
    StatementEmpty                 -> pure ()
    StatementExpr   expr           -> void $ infer expr
    StatementBreak  expr           -> void $ infer expr
    StatementReturn expr           -> void $ infer expr
    StatementItem   _              -> pure ()
    StatementLet idf typeName expr -> case typeName of
        Nothing    -> infer expr >>= declareVar idf
        Just tName -> do
            t <- getType tName
            declareVar idf t
            check t expr

checkCrate :: Crate -> Analyzer ()
checkCrate (Crate items) = do
    mapM_ declareItem items
    mapM_ checkItem   items

analyzeCrate :: Crate -> Either Text Crate
analyzeCrate crate = do
    runExcept $ withExceptT T.pack $ evalStateT (checkCrate crate) initialEnv
    pure crate

-- | TODO: is this useful?
analyzeExpr :: Expr -> IO ()
analyzeExpr expr = do
    let typ = evalStateT (inferExpr expr) initialEnv
    print typ
