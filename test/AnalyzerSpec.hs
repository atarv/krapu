module AnalyzerSpec (spec) where

{-# LANGUAGE OverloadedStrings #-}

import           Data.List.NonEmpty
import           Data.Text                      ( Text )
import           Test.Hspec

import           Analyzer
import           AST

import qualified Data.Text                     as T

spec :: Spec
spec = do -- pure () -- TODO: write tests
    describe "Type inference on simple expressions" $ do
        it "works with comparison"
            $          runAnalyzer (infer (IntLit 2 :< IntLit 3))
            `shouldBe` Right TypeBool
        it "fails if types are wrong"
            $          runAnalyzer (infer (IntLit 2 :> BoolLit True))
            `shouldBe` Left
                           (TypeMismatch TypeBool (TypeI64 :| []) "BoolLit True"
                           )
    describe "check" $ do
        it "checks for expected type"
            $          runAnalyzer (check TypeBool (Unit :== Unit))
            `shouldBe` Right ()
        it "fails if not of expected type"
            $          runAnalyzer (check TypeI64 Unit)
            `shouldBe` Left (TypeMismatch TypeUnit (TypeI64 :| []) "Unit")

    describe "Type inference on arrays" $ do
        it "array type can be inferred"
            $          runAnalyzer (infer $ ArrayLit [IntLit 1, IntLit 2])
            `shouldBe` Right (TypeArr TypeI64)
        it "array type cannot be inferred if array is empty"
            $          runAnalyzer (infer $ ArrayLit [])
            `shouldBe` Left CannotInferEmptyArray
        it "array of empty arrays cannot be inferred either"
            $          runAnalyzer (infer $ ArrayLit [ArrayLit [], ArrayLit []])
            `shouldBe` Left CannotInferEmptyArray
        it "type inferred on access" $ do
            runAnalyzer
                    (infer $ ArrayAccess
                        (ArrayLit [BoolLit True, BoolLit False])
                        (IntLit 0)
                    )
                `shouldBe` Right TypeBool
        it "type inferred on access (nested array)" $ do
            runAnalyzer
                    (infer $ ArrayAccess
                        (ArrayLit
                            [ ArrayLit []
                            , ArrayLit [BoolLit True, BoolLit False]
                            ]
                        )
                        (IntLit 0)
                    )
                `shouldBe` Right (TypeArr TypeBool)
        it "throws if element types don't match"
            $          runAnalyzer
                           (infer $ ArrayLit
                               [ArrayLit [IntLit 0], ArrayLit [BoolLit False]]
                           )
            `shouldBe` Left
                           (TypeMismatch
                               (TypeArr TypeBool)
                               (TypeArr TypeI64 :| [])
                               "ArrayLit [ArrayLit [IntLit 0],ArrayLit [BoolLit False]]"
                           )
