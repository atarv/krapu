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

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Functor.Foldable          ( cata
                                                , embed
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , (<|)
                                                )
import           Data.Map.Strict                ( Map )
import           Data.Maybe
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )

import           AST
import           Helpers                        ( onHeadOf
                                                , lookupMaps
                                                )

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
type FnTypes = NonEmpty (Map Identifier ([Type], Type))
type VarTypes = Map Identifier Type
data TypeEnv = TypeEnv
    { fnTypes :: FnTypes -- ^ Type declarations of functions
    , varTypes :: VarTypes -- ^ Variables' types
    , returnType :: Type -- ^ Return type that is allowed in current context
    , hasReturn :: Bool -- ^ True if current function body contains a return 
                        -- statement, false otherwise.
    }

-- | Exceptions that might arise when performing analysis on AST
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
    | DuplicateFnDeclaration Identifier
    | NoReturn Identifier Type
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
        DuplicateFnDeclaration (Identifier name) ->
            "Duplicate declaration of function '" <> T.unpack name <> "'"
        NoReturn (Identifier name) t -> mconcat
            [ "Function '"
            , T.unpack name
            , "' has no return statement nor outer expression of type "
            , display t
            ]

type Analyzer = StateT TypeEnv (Except AnalyzerException)

-- | Instances of @Inferable@ can have their type inferred
class Inferable a where
    -- | Infer node's type
    infer :: a -> Analyzer Type

instance Inferable Expr where
    infer = inferExpr

instance Inferable Block where
    infer (Block stmts outerExpr) = do
        analyzeStatements stmts
        maybe (pure TypeUnit) infer outerExpr

primitiveTypes :: Types
primitiveTypes = Map.fromList $ fmap
    (\t -> (TypeName . T.pack . display $ t, t))
    [TypeUnit, TypeI64, TypeBool, TypeStr]

-- | Initial environment of the analyzer
initialEnv :: TypeEnv
initialEnv = TypeEnv (fnTypes :| []) varTypes TypeUnit False
  where
    fnTypes = Map.fromList
        [ (Identifier "debug_env" , ([], TypeUnit))
        , (Identifier "println"   , ([TypeStr], TypeUnit))
        , (Identifier "i64_to_str", ([TypeI64], TypeStr))
        , (Identifier "str_to_i64", ([TypeStr], TypeI64))
        ]
    varTypes = Map.fromList
        [(Identifier "argc", TypeI64), (Identifier "argv", TypeArr TypeStr)]

-- | Get the actual type that given typename represents. Throws if typename is 
-- not found or otherwise invalid.
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
    case lookupMaps idf fns of
        Nothing -> throwError $ FnNotFound idf
        Just t  -> pure t

-- | Add variable's type to type environment
declareVar :: Identifier -> Type -> Analyzer ()
declareVar idf t =
    modify' $ \env -> env { varTypes = Map.insert idf t $ varTypes env }

-- | Add function to type environment. Throws if function with same name would 
-- be declared another time in the same context.
declareFn :: Identifier -> [Type] -> Type -> Analyzer ()
declareFn idf paramTypes retType = do
    functions <- gets fnTypes
    case Map.lookup idf $ NonEmpty.head functions of
        Nothing -> modify' $ \env -> env
            { fnTypes = Map.insert idf (paramTypes, retType)
                            `onHeadOf` fnTypes env
            }
        Just _ -> throwError $ DuplicateFnDeclaration idf

-- | Throw an where two types mismatch. Use @TypeMismatch@ constructor if 
-- there's more than one expected type
errTypeMismatch :: Show a => Type -> Type -> a -> Analyzer b
errTypeMismatch expected actual expr =
    throwError $ TypeMismatch actual (expected :| []) (show expr)

-- | Run given action in a new scope
withinNewScope :: Analyzer a -> Analyzer a
withinNewScope action = do
    priorState <- get
    modify' $ \env -> env { fnTypes = Map.empty <| fnTypes env }
    result <- action
    put priorState
    pure result

withReturnType :: Type -> Analyzer a -> Analyzer a
withReturnType t analyzer = do
    oldRetType   <- gets returnType
    oldHasReturn <- gets hasReturn
    modify' $ \env -> env { returnType = t, hasReturn = False }
    result <- analyzer
    modify' $ \env -> env { returnType = oldRetType, hasReturn = oldHasReturn }
    pure result

-- * Type checking & inference

-- | Infer the type of an expression
-- 
-- >>> flip evalStateT initialEnv $ inferExpr (IntLit 0 :< IntLit 2)
-- ExceptT (Identity (Right TypeBool))
inferExpr :: Expr -> Analyzer Type
inferExpr = \case
    -- Literals
    Unit                  -> pure TypeUnit
    BoolLit _             -> pure TypeBool
    IntLit  _             -> pure TypeI64
    Str     _             -> pure TypeStr
    arrLit@(ArrayLit arr) -> do
        elemTypes <- catMaybes <$> forM
            arr
            (\elem -> (Just <$> inferExpr elem)
                -- In this case empty arrays are of the same type as other 
                -- arrays, so the exception can be suppressed
                `catchError` \CannotInferEmptyArray -> pure Nothing
            )
        when (null elemTypes) $ throwError CannotInferEmptyArray
        -- Check that all elements' types match
        TypeArr <$> foldM
            (\expected t -> do
                unless (expected == t) $ errTypeMismatch expected t arrLit
                pure t
            )
            (head elemTypes) -- head and tail are safe since elemTypes is surely
            (tail elemTypes) -- non-empty here

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
        breaks -> do -- TODO: better handling of breaks
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
        ArrayAccess (Var var) index -> do
            check TypeI64 index
            lookupVarType var >>= \case
                TypeArr t -> check t rhs >> pure t
                nonArrayType ->
                    throwError $ CannotIndex nonArrayType TypeI64 index
        ArrayAccess indexed _indexExpr -> do
            t <- infer indexed
            throwError $ CannotAssign t indexed
        unindexable -> do
            t <- infer unindexable
            throwError $ CannotAssign t unindexable
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
    ArrayAccess indexed index -> inferExpr indexed >>= \case
        TypeArr t -> do
            check TypeI64 index
            pure t
        nonIndexable -> do
            t <- infer index
            throwError $ CannotIndex nonIndexable t indexed

-- | Infer the type of a numeric binary expression
inferNumericBinExpr :: Expr -> Expr -> Analyzer Type
inferNumericBinExpr = inferBinExpr (Set.fromList [TypeI64])

-- | Infer type of boolean binary expression
inferBoolBinExpr :: Expr -> Expr -> Analyzer Type
inferBoolBinExpr = inferBinExpr (Set.fromList [TypeBool])

-- | Infer the type of binary expression of two comparable expressions
inferComparableBinExpr :: Expr -> Expr -> Analyzer Type
inferComparableBinExpr lhs rhs =
    TypeBool <$ inferBinExpr (Set.fromList [TypeBool, TypeI64, TypeStr]) lhs rhs

-- | Infer the type of binary expression of two expressions that can be checked
-- for equality
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

-- | Pick break expressions that given block contains
pickBreakExprs :: Block -> [Expr]
pickBreakExprs (Block stmts outerExpr) = case outerExpr of
    Just (ExprBlock block) ->
        mconcat (fmap (cata alg) stmts) ++ pickBreakExprs block
    _ -> mconcat $ fmap (cata alg) stmts
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

-- | Pick blocks from expression that are not loop bodies
pickNonLoopBlocks :: Expr -> [Block]
pickNonLoopBlocks = cata alg
  where
    alg :: ExprF [Block] -> [Block]
    alg (IfExprF ifs altBlock) =
        let (conditions, conseqs) = unzip ifs
        in  case altBlock of
                Nothing  -> mconcat conditions ++ conseqs
                Just alt -> alt : mconcat conditions ++ conseqs
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

-- | @check t node@ checks that @node@ belonging to AST has given type @t@ 
-- throwing an error if it is not.
check :: (Inferable a, Show a) => Type -> a -> Analyzer ()
check expected expr = do
    actual <- infer expr
    when (expected /= actual) $ errTypeMismatch expected actual expr

-- | Add item to type environment
declareItem :: Item -> Analyzer ()
declareItem = \case
    Function fnName params retType _body -> do
        paramTypes <- traverse getType (snd <$> params)
        retType'   <- getType retType
        declareFn fnName paramTypes retType'

-- | Check item's validity
checkItem :: Item -> Analyzer ()
checkItem = \case
    Function fnName params retTypeName (Block stmts outerExpr) -> do
        let (paramNames, typeNames) = unzip params
        paramTypes <- mapM getType typeNames
        retType    <- getType retTypeName
        -- Items should be declare before checking to support recursive calls!
        withReturnType retType $ withinNewScope $ do
            mapM_ (uncurry declareVar) $ zip paramNames paramTypes
            analyzeStatements stmts
            case outerExpr of
                Nothing -> do
                    hasReturn' <- gets hasReturn
                    when (not hasReturn' && retType /= TypeUnit)
                        $ throwError (NoReturn fnName retType)
                Just expr -> check retType expr

-- | Perform analysis of statements in the order it's expected
analyzeStatements :: Foldable t => t Statement -> Analyzer ()
analyzeStatements stmts = do
    mapM_ declareItemStatement stmts
    mapM_ checkItemStatement   stmts
    mapM_ checkStatement       stmts

-- | Add item contained in statement to type environment
declareItemStatement :: Statement -> Analyzer ()
declareItemStatement = \case
    StatementItem item -> declareItem item
    _                  -> pure ()

-- | Item statements have to checked before other statements because function
-- hoisting is allowed and the declarations have to be available
checkItemStatement :: Statement -> Analyzer ()
checkItemStatement = \case
    StatementItem item -> checkItem item
    _                  -> pure ()

-- | Check statement's validity
checkStatement :: Statement -> Analyzer ()
checkStatement = \case
    StatementEmpty       -> pure ()
    StatementExpr   expr -> void $ infer expr
    StatementBreak  expr -> void $ infer expr
    StatementReturn expr -> do
        expected <- gets returnType
        check expected expr
        modify' $ \env -> env { hasReturn = True }
    StatementItem _                -> pure ()
    StatementLet idf typeName expr -> case typeName of
        Nothing    -> infer expr >>= declareVar idf
        Just tName -> do
            t <- getType tName
            declareVar idf t
            check t expr

-- | Check a whole crate
checkCrate :: Crate -> Analyzer ()
checkCrate (Crate items) = do
    mapM_ declareItem items
    mapM_ checkItem   items

-- | Performa analysis on a crate
analyzeCrate :: Crate -> Either Text Crate
analyzeCrate crate =
    crate <$ first (T.pack . show) (runAnalyzer (checkCrate crate))

-- | Run given analyzer in IO monad
runAnalyzerIO :: Analyzer a -> IO a
runAnalyzerIO = either (fail . show) pure . runAnalyzer

runAnalyzer :: Analyzer a -> Either AnalyzerException a
runAnalyzer analyzer = runExcept $ evalStateT analyzer initialEnv

-- * Optimization

-- | Evaluate expressions that consist solely of constants
--
-- >>> evalConstExpr (BoolLit True :&& BoolLit False :== Var (Identifier "q"))
-- BoolLit False :== Var (Identifier "q")
evalConstExpr :: Expr -> Expr
evalConstExpr = cata alg
  where
    alg :: ExprF Expr -> Expr
    alg (IntLit a :+$ IntLit b   ) = IntLit (a + b)
    alg (PlusF expr              ) = expr
    alg (IntLit  a :-$  IntLit  b) = IntLit (a - b)
    alg (IntLit  a :/$  IntLit  b) = IntLit (a `div` b)
    alg (IntLit  a :*$  IntLit  b) = IntLit (a * b)
    alg (IntLit  a :==$ IntLit  b) = BoolLit (a == b)
    alg (Str     a :==$ Str     b) = BoolLit (a == b)
    alg (BoolLit a :==$ BoolLit b) = BoolLit (a == b)
    alg (Unit      :==$ Unit     ) = BoolLit True
    alg (IntLit  a :!=$ IntLit  b) = BoolLit (a /= b)
    alg (Str     a :!=$ Str     b) = BoolLit (a /= b)
    alg (BoolLit a :!=$ BoolLit b) = BoolLit (a /= b)
    alg (Unit      :!=$ Unit     ) = BoolLit False
    alg (BoolLit a :||$ BoolLit b) = BoolLit (a || b)
    alg (BoolLit a :&&$ BoolLit b) = BoolLit (a && b)
    alg (IntLit  a :>$  IntLit  b) = BoolLit (a > b)
    alg (Str     a :>$  Str     b) = BoolLit (a > b)
    alg (BoolLit a :>$  BoolLit b) = BoolLit (a > b)
    alg (IntLit  a :>=$ IntLit  b) = BoolLit (a >= b)
    alg (Str     a :>=$ Str     b) = BoolLit (a >= b)
    alg (BoolLit a :>=$ BoolLit b) = BoolLit (a >= b)
    alg (IntLit  a :<$  IntLit  b) = BoolLit (a < b)
    alg (Str     a :<$  Str     b) = BoolLit (a < b)
    alg (BoolLit a :<$  BoolLit b) = BoolLit (a < b)
    alg (IntLit  a :<=$ IntLit  b) = BoolLit (a <= b)
    alg (Str     a :<=$ Str     b) = BoolLit (a <= b)
    alg (BoolLit a :<=$ BoolLit b) = BoolLit (a <= b)
    alg expr                       = embed expr

-- | Perform optimizations on a single statment
optimizeStatement :: Statement -> Statement
optimizeStatement = \case
    StatementItem item          -> StatementItem $ optimizeItem item
    StatementLet idf type' expr -> StatementLet idf type' (evalConstExpr expr)
    StatementExpr   expr        -> StatementExpr $ evalConstExpr expr
    StatementReturn expr        -> StatementReturn $ evalConstExpr expr
    StatementBreak  expr        -> StatementBreak $ evalConstExpr expr
    stmt                        -> stmt

-- | Perform optimizations on an item
optimizeItem :: Item -> Item
optimizeItem = \case
    Function idf params retType (Block stmts outerExpr) ->
        Function idf params retType
            $ Block (optimizeStatement <$> stmts) (evalConstExpr <$> outerExpr)

-- | Perform optimizations on a crate
optimizeCrate :: Crate -> Crate
optimizeCrate (Crate items) = Crate (optimizeItem <$> items)
