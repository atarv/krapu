{-|
Module         : Analyzer
Description    : Static analysis (type checking etc.) for Krapu
Copyright      : (c) Aleksi Tarvainen, 2021
License        : BSD3
Maintainer     : a.aleksi.tarvainen@student.jyu.fi
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}

module Analyzer where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Functor.Foldable          ( cata )
import           Data.Map.Strict                ( Map )
import           Data.List.NonEmpty             ( NonEmpty(..) )
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

data AnalyzerException
    = TypeMismatch Type (NonEmpty Type) String
    | TypeNameNotFound TypeName
    | VarNotFound Identifier
    | FnNotFound Identifier
    | NonUnitIfExpr Type Block
    | CannotIndex Type Type Expr
    | CannotAssign Type Expr
    | InvalidArgCount Identifier Int Int
    | CannotInferEmptyArray
    deriving Eq

instance Show AnalyzerException where
    show = \case
        TypeMismatch actual (expected :| []) expr -> mconcat
            [ "Expected type "
            , display expected
            , ", but actual was "
            , display actual
            , " in "
            , expr
            ]
        TypeMismatch actual expected expr -> mconcat
            [ "Expected type to be one of ["
            , concat $ NonEmpty.intersperse ", " $ fmap display expected
            , "], but actual was "
            , display actual
            , " in "
            , expr
            ]
        TypeNameNotFound (TypeName name) ->
            "Could not find definition of type " <> T.unpack name
        FnNotFound (Identifier name) ->
            mconcat
                [ "Could not find function '"
                , T.unpack name
                , "' in current context"
                ]
        VarNotFound (Identifier name) ->
            mconcat
                [ "Could not find variable '"
                , T.unpack name
                , "' in currenct context"
                ]
        NonUnitIfExpr typ block -> mconcat
            [ "If else branch of if-expression is not defined,"
            , " branches must have type Unit. Got "
            , display typ
            , " in "
            , show block
            ]
        CannotIndex indexedType indexType expr -> mconcat
            [ "Cannot index "
            , display indexedType
            , " with "
            , display indexType
            , " in "
            , show expr
            ]
        CannotAssign typ expr ->
            "Cannot assign to " <> display typ <> " in " <> show expr
        InvalidArgCount (Identifier name) params args -> mconcat
            [ "Function '"
            , T.unpack name
            , "' expects "
            , show params
            , " arguments, but "
            , show args
            , " were supplied"
            ]
        CannotInferEmptyArray -> "Cannot infer type of an empty array"

type Analyzer = StateT TypeEnv (Except AnalyzerException)

-- | Instances of @Inferable@ can have their type inferred
class Inferable a where
    -- | Infer node's type
    infer :: a -> Analyzer Type

instance Inferable Expr where
    infer = inferExpr

instance Inferable Block where
    infer (Block stmts outerExpr) = do
        mapM_ checkItemStatement stmts
        mapM_ checkStatement     stmts
        infer outerExpr

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
getType tn@(TypeName name) = do
    let elem     = TypeName $ T.takeWhile (not . isBracket) name
        brackets = T.takeWhileEnd isBracket name
        arrLevel = T.length brackets `div` 2
    case Map.lookup elem primitiveTypes of
        Nothing    -> throwError $ TypeNameNotFound tn
        Just type' -> pure $ foldr ($) type' $ replicate arrLevel TypeArr
    where isBracket c = c == '[' || c == ']'

-- | Lookup variable's type from analyzer's environment
lookupVarType :: Identifier -> Analyzer Type
lookupVarType idf = do
    vars <- gets varTypes
    case Map.lookup idf vars of
        Nothing -> throwError $ VarNotFound idf
        Just t  -> pure t

-- | Lookup function's parameter's types and it's return type from analyzer's 
-- environment
lookupFnType :: Identifier -> Analyzer ([Type], Type)
lookupFnType idf = do
    fns <- gets fnTypes
    case Map.lookup idf fns of
        Nothing -> throwError $ FnNotFound idf
        Just t  -> pure t

declareVar :: Identifier -> Type -> Analyzer ()
declareVar idf t =
    modify' $ \env -> env { varTypes = Map.insert idf t $ varTypes env }

-- | TODO: check if already declared?
declareFn :: Identifier -> [Type] -> Type -> Analyzer ()
declareFn idf paramTypes retType = modify' $ \env ->
    env { fnTypes = Map.insert idf (paramTypes, retType) $ fnTypes env }

errTypeMismatch :: Show a => Type -> Type -> a -> Analyzer b
errTypeMismatch expected actual expr =
    throwError $ TypeMismatch actual (expected :| []) (show expr)

-- | Run given action in a new scope
withinNewScope :: Analyzer a -> Analyzer a
withinNewScope action = do
    -- There are no variables to keep track of so implementing scopes like this 
    -- might be enough
    priorState <- get
    result     <- action
    put priorState
    pure result

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
    ArrayLit []             -> throwError CannotInferEmptyArray
    ArrayLit (elem : elems) -> do
        elemType <- inferExpr elem
        mapM_ (check elemType) elems
        pure $ TypeArr elemType
    -- Variables
    Var    idf  -> lookupVarType idf
    -- Arithmetic
    Negate expr -> inferExpr expr >>= \case
        TypeI64 -> pure TypeI64
        wrong   -> errTypeMismatch TypeI64 wrong expr
    Plus expr -> inferExpr expr >>= \case
        TypeI64 -> pure TypeI64
        wrong   -> errTypeMismatch TypeI64 wrong expr
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
                        $ throwError (NonUnitIfExpr blockType blck)
                )
            Just alt -> foldM checkTypesEqual firstType (blocks ++ [alt])
      where
        checkTypesEqual :: Type -> Block -> Analyzer Type
        checkTypesEqual prevType nextBlock = do
            nextType <- infer nextBlock
            when (prevType /= nextType)
                $ errTypeMismatch prevType nextType nextBlock
            pure nextType
    ExprBlock block -> infer block
    Loop      block -> case pickBreakExprs block of
        []     -> pure TypeUnit -- In this case the loop is endless
        breaks -> do
            expectedBrkType <- infer $ head breaks
            forM_ (tail breaks) $ \break -> do
                brkType <- infer break
                unless (brkType == expectedBrkType)
                    $ errTypeMismatch expectedBrkType brkType block
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
                    throwError $ CannotIndex nonArrayType TypeI64 indexExpr
        ArrayAccess indexed _indexExpr -> do
            t <- infer indexed
            throwError $ CannotAssign t indexed
        unindexable -> do
            t <- infer unindexable
            throwError $ CannotIndex t TypeI64 unindexable
    -- Function calls
    FnCall (Identifier "debug") _ ->
        -- Any number of arguments with any types may be debugged
        pure TypeUnit
    FnCall idf args -> do
        (paramTypes, retType) <- lookupFnType idf
        let paramCount = length paramTypes
            argCount   = length args
        when (argCount /= paramCount) . throwError $ InvalidArgCount
            idf
            paramCount
            argCount
        zipWithM_ check paramTypes args
        pure retType
    -- Misc
    ArrayAccess indexedExpr indexExpr -> do
        inferExpr indexedExpr >>= \case
            TypeArr t -> do
                check TypeI64 indexExpr
                pure t
            nonIndexable -> do
                t <- infer indexExpr
                throwError $ CannotIndex nonIndexable t indexedExpr

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
        else throwError $ TypeMismatch
            lhsType
            (NonEmpty.fromList $ Set.toAscList allowedTypes)
            (show lhs)

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
    when (expected /= actual) $ errTypeMismatch expected actual expr

declareItem :: Item -> Analyzer ()
declareItem = \case
    Function fnName params retType _body -> do
        paramTypes <- traverse getType (snd <$> params)
        retType'   <- getType retType
        declareFn fnName paramTypes retType'

checkItem :: Item -> Analyzer ()
checkItem = \case
    Function fnName params retType (Block stmts outerExpr) -> do
        let (paramNames, typeNames) = unzip params
        paramTypes <- mapM getType typeNames
        retType'   <- getType retType
        -- declare before checking to support recursive calls
        declareFn fnName paramTypes retType'
        withinNewScope $ do
            mapM_ (uncurry declareVar) $ zip paramNames paramTypes
            mapM_ checkItemStatement stmts
            mapM_ checkStatement     stmts
            -- TODO: check that return statements are of same type
            check retType' outerExpr

-- | Item statements have to checked before other statements because function
-- hoisting is allowed and the declarations have to be available
checkItemStatement :: Statement -> Analyzer ()
checkItemStatement = \case
    StatementItem item -> checkItem item
    _                  -> pure ()

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
analyzeCrate crate =
    crate <$ first (T.pack . show) (runAnalyzer (checkCrate crate))

runAnalyzerIO :: Analyzer a -> IO a
runAnalyzerIO = either (fail . show) pure . runAnalyzer

runAnalyzer :: Analyzer a -> Either AnalyzerException a
runAnalyzer analyzer = runExcept $ evalStateT analyzer initialEnv
