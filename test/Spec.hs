{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Main (main) where

import           Test.Hspec.QuickCheck

import           Data.Char
import           Data.Word                      ( Word )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Numeric
import           Text.Megaparsec
import           Parser
import           AST

intLiteralTest :: Text -> Integer -> Expectation
intLiteralTest input value =
    parse integerLiteral "" input `shouldParse` IntLit value

main :: IO ()
main = hspec $ do
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

    describe "Logical operators" $ do
        it "&& should have higher precedence than ||"
            $             parse expression "" "false || !false && true"
            `shouldParse` BinaryOp
                              Or
                              (BoolLit False)
                              (BinaryOp And
                                        (UnaryOp Not (BoolLit False))
                                        (BoolLit True)
                              )

    describe "Comparison operators" $ do
        it "equal and not equal should have lesser precedence"
            $             parse expression "" "true == 1 < 2"
            `shouldParse` BinaryOp Equal
                                   (BoolLit True)
                                   (BinaryOp Lesser (IntLit 1) (IntLit 2))
