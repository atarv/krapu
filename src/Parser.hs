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

import           Data.Bifunctor                 ( bimap )
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           AST

import qualified Text.Megaparsec.Char.Lexer    as Lex
import qualified Control.Monad.Combinators.Expr
                                               as Expr
import qualified Data.Text                     as T


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

betweenBraces :: Parser a -> Parser a
betweenBraces = between (symbol "{") (symbol "}")

booleanLiteral :: Parser Expr
booleanLiteral =
    (BoolLit False <$ symbol "false") <|> (BoolLit True <$ symbol "true")

-- | Parse integer literal. Supports different bases 
-- (decimal, binary, octal, hexadecimal).
integerLiteral :: Parser Expr
integerLiteral = fmap IntLit . lexeme . label "integer literal" $ do
    nonDefaultBase <|> base10
  where
    base10         = Lex.signed spaceConsumer (lexeme Lex.decimal)
    nonDefaultBase = choice (chunk <$> ["0b", "0o", "0x"]) >>= \case
        "0b" -> Lex.binary
        "0o" -> Lex.octal
        "0x" -> Lex.hexadecimal
        _ ->
            fail "This shouldn't happen since parser should have failed already"

-- Usage of @Expr.makeExprParser@ is based on examples from Megaparsec 
-- documentation and Joseph Morag's MicroC tutorial
-- <https://blog.josephmorag.com/posts/mcc1/>

-- | Specifies operators that can be used in expressions and their precedence
operatorTable :: [[Expr.Operator Parser Expr]]
operatorTable =
    -- Higher precedence
    [ [unary "-" Negate, unary "+" Plus, unary "!" Not]
    , [binary "*" (:*), binary "/" (:/)]
    , [binary "+" (:+), binary "-" (:-)]
    , [ binary ">"  (:>)  -- Rust disallows comparison operator chaining, but
      , binary "<"  (:<)  -- to make things simple let's follow C instead
      , binary ">=" (:>=)
      , binary "<=" (:<=)
      ]
    , [binary "==" (:==), binary "!=" (:!=)]
    , [binary "&&" (:&&)]
    , [binary "||" (:||)]
    ] -- Lower precedence
  where
    -- | Defines an unary operator
    unary name unOp = Expr.Prefix $ unOp <$ symbol name

    -- | Defines an infix left associative binary operator
    binary name binOp = Expr.InfixL $ binOp <$ symbol name

-- | Parse a literal expression, which directly describes a value.
literal :: Parser Expr
literal = integerLiteral <|> booleanLiteral

-- | Parses terms that can be used in expressions
term :: Parser Expr
term = betweenParens expression <|> ifExpr <|> blockExpr <|> literal <?> "term"

-- | Parses an expression
expression :: Parser Expr
expression = Expr.makeExprParser term operatorTable <?> "expression"

-- Parse an if-else-expression, else is optional.
-- TODO: add support for else-if
ifExpr :: Parser Expr
ifExpr = do
    _           <- symbol "if"
    condition   <- expression
    consequent  <- block
    alternative <- optional $ symbol "else" >> block
    pure $ IfExpr condition consequent alternative

blockExpr :: Parser Expr
blockExpr = ExprBlock <$> block

-- | Parse a type identifier (starts with upper case letter)
type_ :: Parser Type
type_ = lexeme . label "type" $ do
    initial <- upperChar
    rest    <- many alphaNumChar
    pure . Type $ T.singleton initial <> T.pack rest

-- | Parse return type of a function
returnType :: Parser Type
returnType = lexeme . label "return type" $ symbol "->" >> type_

-- | Parse an identifier of other language constructs than types
identifier :: Parser Identifier
identifier = lexeme . label "identifier" $ do
    initial <- lowerChar <|> single '_'
    rest    <- many alphaNumChar
    pure . Identifier $ T.singleton initial <> T.pack rest

-- | Parse a single function parameter
functionParam :: Parser Parameter
functionParam = do
    name <- identifier
    _    <- symbol ":"
    (,) name <$> type_

-- | Parse a statement
statement :: Parser Statement
statement =
    try emptyStatement
        <|> try itemStatement
        <|> try letStatement
        <|> try statementExpr
  where
    emptyStatement = StatementEmpty <$ symbol ";"
    itemStatement  = StatementItem <$> item
    statementExpr  = StatementExpr <$> expression <* symbol ";"
    letStatement =
        StatementLet
            <$> (symbol "let" *> identifier)
            <*> (symbol ":" *> type_)
            <*> (symbol "=" *> expression)
            <*  symbol ";"

-- | Parse a block (a bunch of statements enclosed in braces). It may have an 
-- outer expression, which is used as block's return value.
block :: Parser Block
block = betweenBraces $ do
    stmts <- many statement
    -- If given, outer expression is used as block's return value
    maybe (Block stmts) (BlockExpr stmts) <$> optional expression

-- | Parse a function declaration. Return type may be omitted.
functionDeclaration :: Parser Item
functionDeclaration = do
    _      <- symbol "fn"
    name   <- identifier
    params <-
        betweenParens (functionParam `sepBy` symbol ",") <?> "parameter list"
    -- default to Unit if return type is not defined
    retType <- fromMaybe (Type "Unit") <$> optional returnType
    Function name params retType <$> block

-- | Parse an item. Items are components of a crate.
item :: Parser Item
item = functionDeclaration

-- | Parse a whole crate. Empty crates are not allowed.
crate :: Parser Crate
crate = Crate <$> some item <* eof

-- | First argument is source file name and second is it's content. @Left@
-- contains parser error message(s) and @Right@ contains the AST as text
parseProgram :: String -> Text -> Either Text Text
parseProgram src content =
    bimap (T.pack . errorBundlePretty) (T.pack . show) $ parse crate src content
