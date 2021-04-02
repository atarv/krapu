{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

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
            `shouldParse` IfExpr (IntLit 1 :< IntLit 2)
                                 (Block [StatementExpr $ IntLit 3] Unit)
                                 Nothing
        it "can be used in conditions (as expressions)"
            $ parse ifExpr
                    ""
                    "if if 1 > 2 { false } else { true } { 3 } else { 4 }"
            `shouldParse` IfExpr
                              (IfExpr (IntLit 1 :> IntLit 2)
                                      (Block [] (BoolLit False))
                                      (Just (Block [] (BoolLit True)))
                              )
                              (Block [] (IntLit 3))
                              (Just (Block [] (IntLit 4)))

    describe "block expressions" $ do
        it "can be nested"
            $             parse blockExpr "" "{ ; { 1 } }"
            `shouldParse` ExprBlock
                              (Block [StatementEmpty]
                                     (ExprBlock (Block [] (IntLit 1)))
                              )

    describe "let statements" $ do
        it "can be parsed"
            $             parse statement "" "let xyzzy: I64 = 4321 ;"
            `shouldParse` StatementLet (Identifier "xyzzy")
                                       (Type "I64")
                                       (IntLit 4321)
