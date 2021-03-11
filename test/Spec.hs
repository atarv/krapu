{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Main (main) where

import           Control.Applicative     hiding ( some )
import           Data.Text                      ( Text )
import           Data.Void
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Parser
import           AST

main :: IO ()
main = hspec $ describe "Integer literal parser parses correctly" $ do
    it "base-ten integers"
        $             parse integerLiteral "" "-1234567890"
        `shouldParse` IntLit (-1234567890)
    it "binary numbers"
        $             parse integerLiteral "" "0b0101"
        `shouldParse` IntLit 5
    it "octal numbers"
        $             parse integerLiteral "" "0o755"
        `shouldParse` IntLit 493
    it "hexadecimal numbers"
        $             parse integerLiteral "" "0xfab0"
        `shouldParse` IntLit 64176
