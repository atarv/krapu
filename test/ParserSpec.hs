{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module ParserSpec (spec) where

import           Data.Char
import           Data.Text                      ( Text , pack)
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

    describe "Logical operators" $ do
        it "&& should have higher precedence than ||"
            $             parse expression "" "false || !false && true"
            `shouldParse` (   BoolLit False
                          :|| ((Not (BoolLit False)) :&& BoolLit True)
                          )

    describe "Comparison operators" $ do
        it
                "equal (and not equal) should have lesser precedence than other\ 
                \ comparison operators"
            $             parse expression "" "true == 1 < 2"
            `shouldParse` (BoolLit True :== (IntLit 1 :< IntLit 2))

    describe "if expressions" $ do
        it "should be possible to parse without else case"
            $             parse ifExpr "" "if 1 < 2 { 3 }"
            `shouldParse` IfExpr (IntLit 1 :< IntLit 2)
                                 (BlockExpr [] (IntLit 3))
                                 Nothing
        it "can be nested"
            $ parse ifExpr
                    ""
                    "if if 1 > 2 { false } else { true } { 3 } else { 4 }"
            `shouldParse` IfExpr
                              (IfExpr (IntLit 1 :> IntLit 2)
                                      (BlockExpr [] (BoolLit False))
                                      (Just (BlockExpr [] (BoolLit True)))
                              )
                              (BlockExpr [] (IntLit 3))
                              (Just (BlockExpr [] (IntLit 4)))
