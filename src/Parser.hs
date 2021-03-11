{-|
Module         : Parser
Description    : Parser is responsible for generating the syntax tree out of 
                 the program source file. Using parser combinators this kinda 
                 does the lexing part too.
Copyright      : (c) Aleksi Tarvainen, 2021
License        : BSD3
Maintainer     : a.aleksi.tarvainen@student.jyu.fi
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Parser where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex
import qualified Control.Monad.Combinators.Expr
                                               as Expr
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           AST

type Parser
    = Parsec Void -- Custom error component
                  Text -- Input stream type

-- | Discards any whitespace and comments
spaceConsumer :: Parser ()
spaceConsumer =
    Lex.space space1 (Lex.skipLineComment "//") (Lex.skipBlockComment "/*" "*/")

-- | @lexeme@ makes given parser combinator discard any comments and 
-- whitespace after consuming input
lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceConsumer

-- | Helper for parsing symbols and keywords of the source language
symbol :: Text -> Parser Text
symbol = Lex.symbol spaceConsumer

-- | Use given parser between parentheses
betweenParens :: Parser a -> Parser a
betweenParens = between (symbol "(") (symbol ")")

booleanLiteral :: Parser Expr
booleanLiteral =
    (BoolLit False <$ symbol "false") <|> (BoolLit True <$ symbol "true")

-- | Parse integer literal. Supports different bases 
-- (decimal, binary, octal, hexadecimal).
integerLiteral :: Parser Expr
integerLiteral = fmap IntLit $ lexeme $ do
    optional (char '0') >>= \case
        Just _ -> oneOf ("box" :: [Char]) >>= \case
            'b' -> Lex.binary
            'o' -> Lex.octal
            'x' -> Lex.hexadecimal
            _ -> fail "This shouldn't happen because all the cases are handled"
        Nothing -> Lex.signed spaceConsumer (lexeme Lex.decimal)

-- Usage of @Expr.makeExprParser@ is based on examples from Megaparsec 
-- documentation and Joseph Morag's MicroC tutorial
-- <https://blog.josephmorag.com/posts/mcc1/>

-- | Specifies operators that can be used in expressions and their precedence
operatorTable :: [[Expr.Operator Parser Expr]]
operatorTable =
    -- Higher precedence
    [ [unary "-" Negate, unary "+" Plus, unary "!" Not]
    , [binary "*" Mul, binary "/" Div]
    , [binary "+" Add, binary "-" Sub]
    , [binary "&&" And]
    , [binary "||" Or]
    ] -- Lower precedence
  where
    -- | Defines an unary operator
    unary :: Text -> UnaryOperator -> Expr.Operator Parser Expr
    unary name unOp = Expr.Prefix $ UnaryOp unOp <$ symbol name

    -- | Defines an infix left associative binary operator
    binary :: Text -> Operator -> Expr.Operator Parser Expr
    binary name binop = Expr.InfixL $ BinaryOp binop <$ symbol name

-- | Parse a literal expression, which directly describes a value.
literal :: Parser Expr
literal = integerLiteral <|> booleanLiteral

-- | Parses terms that can be used in expressions
term :: Parser Expr
term = betweenParens expression <|> literal <?> "term"

-- | Parses an expression
expression :: Parser Expr
expression = Expr.makeExprParser term operatorTable <?> "expression"
