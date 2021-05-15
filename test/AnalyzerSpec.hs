{-# LANGUAGE OverloadedStrings #-}

module AnalyzerSpec
    ( spec
    )
where

import           Data.List.NonEmpty
import           Data.Text                      ( Text )
import           Test.Hspec

import           Analyzer
import           AST

import qualified Data.Text                     as T


spec :: Spec
spec = do
    describe "Type inference on simple expressions" $ do
        it "works with comparison"
            $          runAnalyzer (infer (IntLit 2 :< IntLit 3))
            `shouldBe` Right TypeBool
        it "throws if types are wrong"
            $          runAnalyzer (infer (IntLit 2 :> BoolLit True))
            `shouldBe` Left
                           (TypeMismatch TypeBool (TypeI64 :| []) "BoolLit True"
                           )

    describe "check" $ do
        it "checks for expected type"
            $          runAnalyzer (check TypeBool (Unit :== Unit))
            `shouldBe` Right ()
        it "throws if not of expected type"
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

    describe "Let statement checking & inference" $ do
        it
                "type is inferred from right hand side of assignment\
            \ if not explicit"
            $ let
                  testCase = do
                      checkStatement
                          (StatementLet (Identifier "foo") Nothing (IntLit 1))
                      infer $ Var (Identifier "foo")
              in  runAnalyzer testCase `shouldBe` Right TypeI64

        it "throws if explicit type doesn't match right hand side"
            $          runAnalyzer
                           (do
                               checkStatement $ StatementLet
                                   (Identifier "bar")
                                   (Just . TypeName $ "Bool")
                                   (IntLit 0)
                           )
            `shouldBe` Left (TypeMismatch TypeI64 (TypeBool :| []) "IntLit 0")

    describe "If expressions" $ do
        it "work if every branch has the same type"
            $          runAnalyzer
                           (infer $ IfExpr
                               [ (BoolLit False, Block [] (Just . Str $ "asdf"))
                               , (BoolLit True , Block [] (Just . Str $ "qwer"))
                               ]
                               (Just $ Block [] (Just . Str $ "zxcv"))
                           )
            `shouldBe` Right TypeStr
        it "throws if one branch has differing type"
            $          runAnalyzer
                           (infer $ IfExpr
                               [ (BoolLit False, Block [] (Just . Str $ "asdf"))
                               , (BoolLit True , Block [] (Just Unit))
                               ]
                               (Just $ Block [] (Just . Str $ "zxcv"))
                           )
            `shouldBe` Left
                           (TypeMismatch TypeUnit
                                         (TypeStr :| [])
                                         "Block [] (Just Unit)"
                           )
        it "if there's no else branch, other branches must have type Unit"
            $          runAnalyzer
                           (infer $ IfExpr
                               [ (BoolLit False, Block [] (Just Unit))
                               , (BoolLit True , Block [] (Just Unit))
                               ]
                               Nothing
                           )
            `shouldBe` Right TypeUnit
        it "throws if no else branch and any other branch has non-Unit type"
            $ let wrongBlock = Block [] (Just $ BoolLit False)
              in  runAnalyzer
                          (infer $ IfExpr
                              [ (BoolLit False, Block [] (Just Unit))
                              , (BoolLit True , wrongBlock)
                              ]
                              Nothing
                          )
                      `shouldBe` Left (NonUnitIfExpr TypeBool wrongBlock)
        it "throws if conditional is non-Bool"
            $          runAnalyzer
                           (infer $ IfExpr [(IntLit 1, Block [] (Just $ IntLit 2))]
                                           Nothing
                           )
            `shouldBe` Left (TypeMismatch TypeI64 (TypeBool :| []) "IntLit 1")

    describe "Assignment" $ do
        it "should throw if variable has different type than right hand side"
            $ let
                  errAssignment = runAnalyzer $ do
                      checkStatement
                          (StatementLet (Identifier "foo")
                                        Nothing
                                        (BoolLit True)
                          )
                      infer $ Var (Identifier "foo") := IntLit 0
              in
                  errAssignment `shouldBe` Left
                      (TypeMismatch TypeI64 (TypeBool :| []) "IntLit 0")
        it "to array constant should throw"
            $          runAnalyzer (infer $ ArrayLit [IntLit 0] := IntLit 1)
            `shouldBe` Left
                           (CannotAssign (TypeArr TypeI64) (ArrayLit [IntLit 0])
                           )
        it "should throw if assigned item's type differs from array elements'"
            $ let
                  assignedTypeErr = runAnalyzer $ do
                      checkStatement
                          (StatementLet (Identifier "arr")
                                        Nothing
                                        (ArrayLit [IntLit 0, IntLit 1])
                          )
                      infer
                          $  ArrayAccess (Var (Identifier "arr")) (IntLit 1)

                          := BoolLit True
              in  assignedTypeErr `shouldBe` Left
                      (TypeMismatch TypeBool (TypeI64 :| []) "BoolLit True")

    describe "Function declarations" $ do
        it "throws if return value's type differs from declared return type"
            $          runAnalyzer
                           (checkItem $ Function (Identifier "f")
                                                 []
                                                 (TypeName "I64")
                                                 (Block [] (Just $ BoolLit False))
                           )
            `shouldBe` Left
                           (TypeMismatch TypeBool
                                         (TypeI64 :| [])
                                         "BoolLit False"
                           )
        it "throws if function body contains a type mismatch"
            $          runAnalyzer
                           (checkItem $ Function
                               (Identifier "f")
                               [(Identifier "x", TypeName "I64")]
                               (TypeName "I64")
                               (Block
                                   [StatementExpr $ BoolLit True :+ Var (Identifier "x")]
                                   (Just $ IntLit 1)
                               )
                           )
            `shouldBe` Left
                           (TypeMismatch TypeBool (TypeI64 :| []) "BoolLit True"
                           )
        it "throws if trying to declare function with same name twice"
            $ let declareTwice = runAnalyzer $ do
                      declareItem $ Function (Identifier "f")
                                             []
                                             (TypeName "I64")
                                             (Block [] (Just $ IntLit 1))
                      declareItem $ Function
                          (Identifier "f")
                          []
                          (TypeName "Bool")
                          (Block [] (Just $ BoolLit True))
              in  declareTwice
                      `shouldBe` Left (DuplicateFnDeclaration (Identifier "f"))
        it "throws if return statement's inferred type differs from return type"
            $          runAnalyzer
                           (checkItem $ Function
                               (Identifier "f")
                               [(Identifier "x", TypeName "I64")]
                               (TypeName "I64")
                               (Block [StatementReturn Unit] (Just $ IntLit 1))
                           )
            `shouldBe` Left (TypeMismatch TypeUnit (TypeI64 :| []) "Unit")
        it
                "throws if function has no return statement nor outer \
                \expression and return type is other than Unit"
            $          runAnalyzer
                           (checkItem $ Function (Identifier "f")
                                                 []
                                                 (TypeName "Bool")
                                                 (Block [StatementEmpty] Nothing)
                           )
            `shouldBe` Left (NoReturn (Identifier "f") TypeBool)
        -- FIXME: should probably think of a more sophisticated way of analyzing
        -- control flow to fix this test. GCC doesn't catch similiar cases
        -- without flags, so this might suffice.
        -- it "throws if one branch of function has return but another doesn't"
        --     $             runAnalyzer
        --                       (checkItem $ Function
        --                           (Identifier "f")
        --                           []
        --                           (TypeName "Bool")
        --                           (Block
        --                               [ StatementExpr $ IfExpr
        --                                   [ ( BoolLit False
        --                                     , Block [StatementReturn $ BoolLit False]
        --                                             Nothing
        --                                     )
        --                                   ]
        --                                   Nothing
        --                               , StatementEmpty
        --                               ]
        --                               Nothing
        --                           )
        --                       )
        --     `shouldNotBe` Right ()

    describe "Function calls" $ do
        it "throws if argument count doesn't match parameter count"
            $          runAnalyzer
                           (infer $ FnCall (Identifier "i64_to_str") [IntLit 1, IntLit 2]
                           )
            `shouldBe` Left (InvalidArgCount (Identifier "i64_to_str") 1 2)

    describe "Loop-expressions" $ do
        it "type is inferred based on break expressions"
            $          runAnalyzer
                           (infer $ Loop
                               (Block
                                   [ StatementExpr $ IfExpr
                                         [ ( IntLit 1 :> IntLit 2
                                           , Block [StatementBreak $ Str "broke"] Nothing
                                           )
                                         ]
                                         Nothing
                                   ]
                                   (Just Unit)
                               )
                           )
            `shouldBe` Right TypeStr
        it "throws if conflicting types in break expressions"
            $          runAnalyzer
                           (infer $ Loop
                               (Block
                                   [ StatementExpr $ IfExpr
                                         [ ( IntLit 1 :> IntLit 2
                                           , Block [StatementBreak $ BoolLit True]
                                                   Nothing
                                           )
                                         ]
                                         Nothing
                                   ]
                                   (Just $ ExprBlock $ Block
                                       [ StatementExpr $ IfExpr
                                             [ ( BoolLit False
                                               , Block [StatementBreak Unit] Nothing
                                               )
                                             ]
                                             Nothing
                                       ]
                                       Nothing
                                   )
                               )
                           )
            `shouldBe` Left
                           (TypeMismatch
                               TypeUnit
                               (TypeBool :| [])
                               "Block [StatementExpr (IfExpr [(IntLit 1 :> IntLit 2,Block [StatementBreak (BoolLit True)] Nothing)] Nothing)] (Just (ExprBlock (Block [StatementExpr (IfExpr [(BoolLit False,Block [StatementBreak Unit] Nothing)] Nothing)] Nothing)))"
                           )

    describe "While-loops" $ do
        it "throws if loop condition is non-boolean"
            $          runAnalyzer (infer $ While (IntLit 1) (Block [] Nothing))
            `shouldBe` Left (TypeMismatch TypeI64 (TypeBool :| []) "IntLit 1")
        it "are inferred to be of type Unit"
            $          runAnalyzer
                           (infer $ While
                               (BoolLit True)
                               (Block
                                   [StatementLet (Identifier "z") Nothing (IntLit 1)]
                                   (Just $ IntLit 2)
                               )
                           )
            `shouldBe` Right TypeUnit
        it "throws when breaking from them with non-Unit values"
            $          runAnalyzer
                           (infer $ While
                               (BoolLit True)
                               (Block
                                   [ StatementLet (Identifier "z") Nothing (IntLit 1)
                                   , StatementBreak $ Str "fail"
                                   ]
                                   Nothing
                               )
                           )
            `shouldBe` Left
                           (TypeMismatch TypeStr (TypeUnit :| []) "Str \"fail\""
                           )

    describe "Constant expression evaluation" $ do
        it
                "evaluates expressions consistincg of constants and leaves the \
                \ other untouched"
            $          evalConstExpr
                           (   (IntLit 1 :+ IntLit 2)
                           :-  ((IntLit 4 :* IntLit 3) :/ IntLit 4)
                           :== (Var (Identifier "x") :+ Var (Identifier "y"))
                           )
            `shouldBe` (   IntLit 0
                       :== (Var (Identifier "x") :+ Var (Identifier "y"))
                       )
