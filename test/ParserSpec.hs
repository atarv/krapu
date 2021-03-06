{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Data.Char
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Numeric
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Test.Hspec.QuickCheck
import           Text.Megaparsec

import           AST
import           Parser

intLiteralTest :: Text -> Integer -> Expectation
intLiteralTest input value =
    parse integerLiteral "" input `shouldParse` IntLit value

spec :: Spec
spec = do
    describe "Integer literal parser" $ do
        context "parses correctly" $ do
            prop "base-ten integers"
                $ \n -> intLiteralTest (pack $ show (n :: Integer)) n
            prop "binary numbers" $ \n ->
                let x = fromIntegral (n :: Word) -- limit to non-negative values
                in  intLiteralTest
                        (pack $ "0b" <> showIntAtBase 2 intToDigit x "")
                        x
            prop "octal numbers" $ \n ->
                let x = fromIntegral (n :: Word)
                in  intLiteralTest (pack $ "0o" <> showOct n "") x
            prop "hexadecimal numbers" $ \n ->
                let x = fromIntegral (n :: Word)
                in  intLiteralTest (pack $ "0x" <> showHex n "") x
        context "should fail if" $ do
            it "number is not specified"
                $              parse integerLiteral ""
                `shouldFailOn` "0x"
            it "there is whitespace between the base prefix and literal value"
                $              parse integerLiteral ""
                `shouldFailOn` "0x ff"

    describe "Logical operators" $ do
        it "&& should have higher precedence than ||"
            $             parse expression "" "false || !false && true"
            `shouldParse` (   BoolLit False
                          :|| (Not (BoolLit False) :&& BoolLit True)
                          )

    describe "Comparison operators" $ do
        it
                "equal (and not equal) should have lesser precedence than other\ 
                \ comparison operators"
            $             parse expression "" "true == 1 < 2"
            `shouldParse` (BoolLit True :== (IntLit 1 :< IntLit 2))

    describe "if expressions" $ do
        it "should be possible to parse without else case"
            $             parse ifExpr "" "if 1 < 2 { 3; }"
            `shouldParse` IfExpr
                              [ ( IntLit 1 :< IntLit 2
                                , Block [StatementExpr $ IntLit 3] Nothing
                                )
                              ]
                              Nothing
        it "can be used in conditions (as expressions)"
            $ parse ifExpr
                    ""
                    "if if 1 > 2 { false } else { true } { 3 } else { 4 }"
            `shouldParse` IfExpr
                              [ ( IfExpr
                                    [ ( IntLit 1 :> IntLit 2
                                      , Block [] $ Just (BoolLit False)
                                      )
                                    ]
                                    (Just $ Block [] $ Just (BoolLit True))
                                , Block [] $ Just (IntLit 3)
                                )
                              ]
                              (Just (Block [] $ Just (IntLit 4)))
        it "can have else if branches"
            $             parse ifExpr "" "if -2 < i { 1 } else if i == 0 { 2 }"
            `shouldParse` IfExpr
                              [ ( Negate (IntLit 2) :< Var (Identifier "i")
                                , Block [] $ Just (IntLit 1)
                                )
                              , ( Var (Identifier "i") :== IntLit 0
                                , Block [] $ Just (IntLit 2)
                                )
                              ]
                              Nothing
        it "comments are allowed between and after else and if keywords" $ do
            let
                withComments
                    = "if false /* always */\n\
                    \{ 1 }\n\
                    \/**/ else /* comment */ if /* condition */ 1 < 2\n\
                    \{ 2 }"
            parse ifExpr "" withComments
                `shouldParse` IfExpr
                                  [ (BoolLit False, Block [] $ Just (IntLit 1))
                                  , ( IntLit 1 :< IntLit 2
                                    , Block [] $ Just (IntLit 2)
                                    )
                                  ]
                                  Nothing

    describe "block expressions" $ do
        it "can be nested and used as a return value"
            $             parse blockExpr "" "{ ; { 1 } }"
            `shouldParse` ExprBlock
                              (Block [StatementEmpty] $ Just
                                  (ExprBlock (Block [] $ Just (IntLit 1)))
                              )
        it
                "declaring unit excplicitly as outer expression results in\
                \ different AST"
            $             parse blockExpr "" "{ () }"
            `shouldParse` ExprBlock (Block [] (Just Unit))

    describe "block statements" $ do
        it "end in semicolon to separate them from block expressions"
            $             parse statement "" "{ ; { } };"
            `shouldParse` StatementExpr
                              (ExprBlock $ Block [StatementEmpty] $ Just
                                  (ExprBlock $ Block [] Nothing)
                              )

    describe "let statements" $ do
        it "can be parsed"
            $             parse statement "" "let xyzzy: I64 = 4321 ;"
            `shouldParse` StatementLet (Identifier "xyzzy")
                                       (Just $ TypeName "I64")
                                       (IntLit 4321)
        it "declaring type is optional"
            $             parse statement "" "let x = 123 ;"
            `shouldParse` StatementLet (Identifier "x") Nothing (IntLit 123)

    describe "variables" $ do
        it "which begin with a keyword name"
            $             parse variable "" "iffe"
            `shouldParse` Var (Identifier "iffe")

    describe "function declarations" $ do
        it "can contain more function definitions"
            $             parse item "" "fn f() { fn g() { 2 } g() }"
            `shouldParse` Function
                              (Identifier "f")
                              []
                              (TypeName "Unit")
                              ( Block
                                      [ StatementItem $ Function
                                            (Identifier "g")
                                            []
                                            (TypeName "Unit")
                                            (Block [] $ Just (IntLit 2))
                                      ]
                              $ Just (FnCall (Identifier "g") [])
                              )

    describe "function calls" $ do
        it "can have empty param lists"
            $             parse functionCall "" "f()"
            `shouldParse` FnCall (Identifier "f") []
        it "omitting argument list causes parser to fail"
            $              parse functionCall ""
            `shouldFailOn` "f"
        it "trailing commas are not allowed on argument list"
            $              parse functionCall ""
            `shouldFailOn` "f(1,2,)"

    describe "Array literals" $ do
        it "empty array" $ parse expression "" "[]" `shouldParse` ArrayLit []
        it "some items"
            $ parse expression "" "[1,2, /* can haz comments */ 1 + 2]"
            `shouldParse` ArrayLit [IntLit 1, IntLit 2, IntLit 1 :+ IntLit 2]

    describe "Array access" $ do
        it "is parsed correctly with array literals"
            $             parse expression "" "[1,2,3][2]"
            `shouldParse` ArrayAccess
                              (ArrayLit [IntLit 1, IntLit 2, IntLit 3])
                              (IntLit 2)
        it "on variable"
            $             parse expression "" "foo[1]"
            `shouldParse` ArrayAccess (Var (Identifier "foo")) (IntLit 1)
