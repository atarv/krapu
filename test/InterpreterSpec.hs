{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module InterpreterSpec (spec) where

import           Control.Monad.State.Strict
import           Data.IORef
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           AST
import           Interpreter

import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T


exampleEnv :: IO Env
exampleEnv = do
    foo  <- newIORef $ ResInt 1
    bar  <- newIORef $ ResInt 42
    baz  <- newIORef $ ResBool False
    foo2 <- newIORef $ ResBool True
    let exampleSyms = NonEmpty.fromList
            [ Map.fromList [(Identifier "foo", foo), (Identifier "bar", bar)]
            , Map.fromList [(Identifier "baz", baz), (Identifier "foo", foo2)]
            ]
    pure $ emptyEnv { symTable = exampleSyms, fnDefs = exampleFnDefs }

exampleFnDefs :: FnDefs
exampleFnDefs = Map.fromList [min] :| []
  where
    min =
        ( Identifier "min"
        , FnDef
            [Identifier "a", Identifier "b"]
            (Block
                []
                (IfExpr
                    [ ( Var (Identifier "a") :<= Var (Identifier "b")
                      , Block [] (Var (Identifier "a"))
                      )
                    ]
                    (Just (Block [] (Var (Identifier "b"))))
                )
            )
        )

usingEnv :: IO Env -> Interpreter b -> IO b
usingEnv ioEnv interpret = do
    env <- ioEnv
    evalStateT interpret env

usingExampleEnv :: Interpreter b -> IO b
usingExampleEnv = usingEnv exampleEnv

spec :: Spec
spec = do
    describe "Arithmetic operations" $ do
        prop "are handled" $ \(a, b, c, d, e) ->
            let testCase = flip evalStateT emptyEnv $ eval
                    (  (IntLit a :+ (IntLit b :/ Negate (IntLit c)))
                    :- (IntLit d :* Plus (IntLit e))
                    )
            in  if (c :: Integer) == 0 -- Avoid division by zero
                    then True `shouldBe` True -- FIXME: check for ArithException
                    else testCase
                        `shouldReturn` ResInt (a + b `div` (-c) - d * e)

    describe "If expressions" $ do
        -- env <- runIO emptyEnv
        prop "condition is handled correctly" $ \(cond, conseq, alt) -> do
            result <- flip evalStateT emptyEnv $ eval
                (IfExpr [(BoolLit cond, Block [] (BoolLit conseq))]
                        (Just (Block [] (BoolLit alt)))
                )
            result `shouldBe` ResBool (if cond then conseq else alt)
        prop "else if branches are handled"
            $ \(condIf, condElseIf, conseqIf, conseqElseIf, alt) -> do
                  result <- flip evalStateT emptyEnv $ eval
                      (IfExpr
                          [ (BoolLit condIf, Block [] (BoolLit conseqIf))
                          , ( BoolLit condElseIf
                            , Block [] (BoolLit conseqElseIf)
                            )
                          ]
                          (Just $ Block [] (BoolLit alt))
                      )
                  result
                      `shouldBe` ResBool
                                     (if condIf
                                         then conseqIf
                                         else if condElseIf
                                             then conseqElseIf
                                             else alt
                                     )

    describe "Let statement" $ do
        let testCase testEnv = do
                usingEnv testEnv $ do
                    execStatement
                        (StatementLet (Identifier "foo")
                                      (Just $ TypeName "I64")
                                      (IntLit 22 :+ IntLit 20)
                        )
                    lookupVar (Identifier "foo")
        it "should add a variable definition to environment"
            $              testCase (pure emptyEnv)
            `shouldReturn` ResInt 42
        it "should allow shadowing an existing variable"
            $              testCase exampleEnv
            `shouldReturn` ResInt 42

    describe "Variable lookup" $ do
        (foo, baz) <- runIO $ usingExampleEnv $ do
            (,) <$> eval (Var $ Identifier "foo") <*> eval
                (Var $ Identifier "baz")

        it "works for the simplest case (current context)" $ do
            foo `shouldBe` ResInt 1
        it "looks upwards the context hierarchy" $ do
            baz `shouldBe` ResBool False
        it "fails if variable does not exist"
            $ let noVar =
                      usingExampleEnv $ eval (Var $ Identifier "doesNotExist")
              in  noVar `shouldThrow` anyException
        it "looks in outer context(s) if variable is not found on current" $ do
            let testOuterContext = usingExampleEnv $ evalBlock
                    (Block
                        [ StatementLet (Identifier "x")
                                       (Just $ TypeName "I64")
                                       (IntLit 2)
                        ]
                        (ExprBlock $ Block [] (Var $ Identifier "x"))
                    )
            testOuterContext `shouldReturn` ResInt 2
        it "should fail if trying to access that was in an exited scope"
            $ let accessInnerScope = usingExampleEnv $ evalBlock
                      (Block
                          [ StatementExpr $ ExprBlock $ Block
                                [ StatementLet (Identifier "x")
                                               (Just $ TypeName "I64")
                                               (IntLit 2)
                                ]
                                Unit
                          ]
                          (ExprBlock $ Block [] (Var $ Identifier "x"))
                      )
              in  accessInnerScope `shouldThrow` anyException

    describe "Assignment expression" $ do
        it "sets variable's value" $ do
            let assignment = usingExampleEnv $ evalBlock
                    (Block
                        [StatementExpr (Var (Identifier "foo") := IntLit 75)]
                        (Var (Identifier "foo"))
                    )
            assignment `shouldReturn` ResInt 75
        it "can be chained and it's right associative" $ do
            let chainedAssignment = usingExampleEnv $ evalBlock
                    (Block
                        [ StatementExpr
                          $  Var (Identifier "bar")
                          := (Var (Identifier "foo") := IntLit 0)
                        ]
                        (Var (Identifier "bar"))
                    )
            chainedAssignment `shouldReturn` ResInt 0

    describe "While loop" $ do
        it "executes loop body until loop predicate is false" $ do
            let
                lookupFoo = usingExampleEnv $ do
                    eval
                        (While
                            (Var (Identifier "foo") :< IntLit 10)
                            (Block
                                [ StatementExpr
                                  $  Var (Identifier "foo")
                                  := (Var (Identifier "foo") :+ IntLit 4)
                                ]
                                Unit
                            )
                        )
                    lookupVar (Identifier "foo")
            lookupFoo `shouldReturn` ResInt 13

    describe "Function call" $ do
        it "succeeds when supplied correct number of arguments" $ do
            let
                fnCall =
                    usingExampleEnv $ eval
                        (FnCall (Identifier "min") [IntLit (-2), IntLit 0])
            fnCall `shouldReturn` ResInt (-2)
        it "fails with inccorrect number of arguments"
            $ let
                  fnCall =
                      usingExampleEnv
                          $ eval
                                (FnCall (Identifier "min")
                                        [IntLit (-2), IntLit 2, IntLit 3]
                                )
              in  fnCall `shouldThrow` anyException
        it "fails if called functions does not exist in current environment"
            $ let fnCallNotFound =
                      usingExampleEnv $ eval (FnCall (Identifier "föö") [])
              in  fnCallNotFound `shouldThrow` anyException
        it "works with early return"
            $ let
                  earlyRet = usingExampleEnv $ do
                      defineItem
                          (Function
                              (Identifier "f")
                              []
                              (TypeName "I64")
                              (Block
                                  [ StatementReturn (IntLit 1)
                                  , StatementReturn (IntLit 2)
                                  ]
                                  (IntLit 3)
                              )
                          )
                      eval (FnCall (Identifier "f") [])
              in  earlyRet `shouldReturn` ResInt 1

    describe "Primitive functions" $ do
        prop "str_to_i64(i64_to_str(x)) == x" $ \x ->
            let
                idempotent = usingEnv (pure emptyEnv) $ eval
                    (FnCall (Identifier "str_to_i64")
                            [FnCall (Identifier "i64_to_str") [IntLit x]]
                    )
            in  idempotent `shouldReturn` ResInt x
        prop "i64_to_str(str_to_i64(x)) == x" $ \x ->
            let idempotent = usingEnv (pure emptyEnv) $ eval
                    (FnCall
                        (Identifier "i64_to_str")
                        [ FnCall (Identifier "str_to_i64")
                                 [Str (T.pack $ show (x :: Integer))]
                        ]
                    )
            in  idempotent `shouldReturn` ResStr (T.pack $ show x)

    describe "Break statement" $ do
        it "should terminate the loop and return with its expression's value"
            $ let
                  breakLoop = usingEnv (pure emptyEnv) $ do
                      execStatement
                          (StatementLet
                              (Identifier "l")
                              (Just $ TypeName "I64")
                              (Loop (Block [StatementBreak $ IntLit 1] Unit))
                          )
                      lookupVar (Identifier "l")
              in  breakLoop `shouldReturn` ResInt 1
        it "should throw an error if used outside of a loop (i.e. function)"
            $ let errBreak = usingEnv (pure emptyEnv) $ do
                      execStatement
                          (StatementItem $ Function
                              (Identifier "brk")
                              []
                              (TypeName "Unit")
                              (Block [StatementBreak Unit] Unit)
                          )
                      eval (FnCall (Identifier "brk") [])
              in  errBreak `shouldThrow` anyException
