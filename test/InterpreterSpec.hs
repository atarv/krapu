{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module InterpreterSpec (spec) where

import           Data.IORef
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           AST
import           Interpreter

import qualified Data.Map.Strict               as Map


exampleEnv :: IO Env
exampleEnv = do
    foo  <- newIORef $ ResInt 1
    bar  <- newIORef $ ResInt 42
    baz  <- newIORef $ ResBool False
    foo2 <- newIORef $ ResBool True
    Env
        <$> newIORef
                [ Map.fromList
                    [(Identifier "foo", foo), (Identifier "bar", bar)]
                , Map.fromList
                    [(Identifier "baz", baz), (Identifier "foo", foo2)]
                ]
        <*> pure exampleFnDefs

exampleFnDefs :: FnDefs
exampleFnDefs = [Map.fromList [min]]
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

spec :: Spec
spec = do
    describe "Arithmetic operations" $ do
        env <- runIO emptyEnv
        prop "are handled" $ \(a, b, c, d, e) ->
            let testCase = eval
                    env
                    (  (IntLit a :+ (IntLit b :/ Negate (IntLit c)))
                    :- (IntLit d :* Plus (IntLit e))
                    )
            in  if (c :: Integer) == 0 -- Avoid division by zero
                    then True `shouldBe` True -- FIXME: check for ArithException
                    else do
                        (_, result) <- testCase
                        result `shouldBe` ResInt (a + b `div` (-c) - d * e)

    describe "If expressions" $ do
        env <- runIO emptyEnv
        prop "condition is handled correctly" $ \(cond, conseq, alt) -> do
            (_, result) <- eval
                env
                (IfExpr [(BoolLit cond, Block [] (BoolLit conseq))]
                        (Just (Block [] (BoolLit alt)))
                )
            result `shouldBe` ResBool (if cond then conseq else alt)
        prop "else if branches are handled"
            $ \(condIf, condElseIf, conseqIf, conseqElseIf, alt) -> do
                  (_, result) <- eval
                      env
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
                env  <- testEnv
                env' <- execStatement
                    env
                    (StatementLet (Identifier "foo")
                                  (Type "I64")
                                  (IntLit 22 :+ IntLit 20)
                    )
                lookupVar env' (Identifier "foo")
        it "should add a variable definition to environment"
            $              testCase emptyEnv
            `shouldReturn` ResInt 42
        it "should allow shadowing an existing variable"
            $              testCase exampleEnv
            `shouldReturn` ResInt 42

    describe "Variable lookup" $ do
        (foo, baz) <- runIO $ do
            env         <- exampleEnv
            (env', foo) <- eval env (Var $ Identifier "foo")
            (_   , baz) <- eval env' (Var $ Identifier "baz")
            pure (foo, baz)

        it "works for the simplest case (current context)" $ do
            foo `shouldBe` ResInt 1
        it "looks upwards the context hierarchy" $ do
            baz `shouldBe` ResBool False
        it "fails if variable does not exist"
            $ let noVar = do
                      env <- exampleEnv
                      eval env (Var $ Identifier "doesNotExist")
              in  noVar `shouldThrow` anyException
        it "looks in outer context(s) if variable is not found on current" $ do
            let testOuterContext = fmap snd $ exampleEnv >>= flip
                    evalBlock
                    (Block
                        [StatementLet (Identifier "x") (Type "I64") (IntLit 2)]
                        (ExprBlock $ Block [] (Var $ Identifier "x"))
                    )
            testOuterContext `shouldReturn` ResInt 2
        it "variables defined in exited scopes shouldn't be available" $ do
            let accessInnerScope = fmap snd $ exampleEnv >>= flip
                    evalBlock
                    (Block
                        [ StatementExpr $ ExprBlock $ Block
                              [ StatementLet (Identifier "x")
                                             (Type "I64")
                                             (IntLit 2)
                              ]
                              Unit
                        ]
                        (ExprBlock $ Block [] (Var $ Identifier "x"))
                    )
            accessInnerScope `shouldThrow` anyException

    describe "Assignment expression" $ do
        it "sets variable's value" $ do
            let assignment = fmap snd $ exampleEnv >>= flip
                    evalBlock
                    (Block
                        [StatementExpr (Var (Identifier "foo") := IntLit 75)]
                        (Var (Identifier "foo"))
                    )
            assignment `shouldReturn` ResInt 75
        it "can be chained and it's right associative" $ do
            let chainedAssignment = fmap snd $ exampleEnv >>= flip
                    evalBlock
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
            let envAfterLoop = fmap fst $ exampleEnv >>= flip
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
                lookupFoo = envAfterLoop >>= flip lookupVar (Identifier "foo")
            lookupFoo `shouldReturn` ResInt 13

    describe "Function calls" $ do
        it "succeed when supplied correct number of arguments" $ do
            let fnCall = fmap snd $ exampleEnv >>= flip
                    eval
                    (FnCall (Identifier "min") [IntLit (-2), IntLit 0])
            fnCall `shouldReturn` ResInt (-2)
        it "fail with inccorrect number of arguments" $ do
            let fnCall = fmap snd $ exampleEnv >>= flip
                    eval
                    (FnCall (Identifier "min") [IntLit (-2), IntLit 2, IntLit 3]
                    )
            fnCall `shouldThrow` anyException
