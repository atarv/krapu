module AnalyzerSpec
    ( spec
    )
where

{-# LANGUAGE OverloadedStrings #-}

import           Data.List.NonEmpty
import           Data.Text                      ( Text )
import           Test.Hspec

import           Analyzer
import           AST

import qualified Data.Text                     as T

spec :: Spec
spec = do -- pure () -- TODO: write tests
    describe "Type inference on expressions" $ do
        it "works with comparison"
            $          runAnalyzer (infer (IntLit 2 :< IntLit 3))
            `shouldBe` Right TypeBool
        it "fails if types are wrong"
            $          runAnalyzer (infer (IntLit 2 :> BoolLit True))
            `shouldBe` Left
                           (TypeMismatch TypeBool (TypeI64 :| []) "BoolLit True"
                           )
    describe "Type check" $ do
        it "checks for expected type"
            $          runAnalyzer (check TypeBool (Unit :== Unit))
            `shouldBe` Right ()
        it "fails if not of expected type"
            $          runAnalyzer (check TypeI64 Unit)
            `shouldBe` Left (TypeMismatch TypeUnit (TypeI64 :| []) "Unit")
