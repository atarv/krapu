
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module InterpreterSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck

import           AST
import           Interpreter


spec :: Spec
spec = do
    describe "Arithmetic operations" $ do
        prop "are handled" $ \(a, b, c, d, e) ->
            let testCase = eval
                    (  (IntLit a :+ (IntLit b :/ Negate (IntLit c)))
                    :- (IntLit d :* Plus (IntLit e))
                    )
            in  if (c :: Integer) == 0 -- Avoid division by zero
                    then True `shouldBe` True -- FIXME: check for ArithException
                    else shouldBe (pure (ResInt (a + b `div` (-c) - d * e)))
                                  testCase

    describe "If expressions" $ do
        prop "condition is handled correctly" $ \(cond, conseq, alt) ->
            eval
                    (IfExpr (BoolLit cond)
                            (BlockExpr [] (BoolLit conseq))
                            (Just (BlockExpr [] (BoolLit alt)))
                    )
                `shouldBe` pure (ResBool $ if cond then conseq else alt)
