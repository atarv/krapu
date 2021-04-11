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

import           Data.Bifunctor                 ( bimap
                                                , first
                                                )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           AST

import qualified Control.Monad.Combinators.Expr
                                               as Expr
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Text.Megaparsec.Char.Lexer    as Lex


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

betweenBrackets :: Parser a -> Parser a
betweenBrackets = between (symbol "[") (symbol "]")

-- | Reserved words of the language. These cannot be used as identifiers.
reservedWords :: Set Text
reservedWords =
    Set.fromList ["else", "fn", "if", "let", "while", "break", "loop"]

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
    , [ binary ">=" (:>=)
      , binary "<=" (:<=)
      , binary ">"  (:>)  -- Rust disallows comparison operator chaining, but
      , binary "<"  (:<)  -- to make things simple let's follow C instead
      ]
    , [binary "==" (:==), binary "!=" (:!=)]
    , [binary "&&" (:&&)]
    , [binary "||" (:||)]
    , [binaryR "=" (:=)]
    ] -- Lower precedence
  where
    -- | Defines an unary operator
    unary name unOp = Expr.Prefix $ unOp <$ symbol name
    -- | Defines an infix left associative binary operator
    binary name binOp = Expr.InfixL $ binOp <$ symbol name
    -- | Defines an infix right associative binary operator
    binaryR name binOp = Expr.InfixR $ binOp <$ symbol name

-- | Parse a literal expression, which directly describes a value.
literal :: Parser Expr
literal =
    integerLiteral
        <|> try booleanLiteral
        <|> unitLiteral
        <|> strLiteral
        <|> arrayLiteral

-- | Parse literal unit, @()@
unitLiteral :: Parser Expr
unitLiteral = Unit <$ symbol "()"

-- | Parse literal string. Escaped characters are supported.
strLiteral :: Parser Expr
strLiteral = Str . T.pack <$ char '"' <*> Lex.charLiteral `manyTill` char '"'

-- | Parse a variable access
variable :: Parser Expr
variable = Var <$> identifier

-- | Parse an array literal
--
-- >>> parse arrayLiteral "" "[1, 2 /* */, 1 + 2]"
-- Right (ArrayLit [IntLit 1,IntLit 2,IntLit 1 :+ IntLit 2])
arrayLiteral :: Parser Expr
arrayLiteral = ArrayLit <$> betweenBrackets (expression `sepBy` symbol ",")

-- | Parses terms that can be used in expressions
term :: Parser Expr
term = do
    -- This parser had to be written in this kind of unfortunate way because of 
    -- left recursion introduced by array access
    first <-
        betweenParens expression
        <|> ifExpr
        <|> blockExpr
        <|> whileLoop
        <|> loop
        <|> literal
        <|> try functionCall
        <|> variable
        <?> "term"
    optional (betweenBrackets expression) >>= \case
        Nothing    -> pure first
        Just index -> pure $ ArrayAccess first index

-- | Parses an expression
expression :: Parser Expr
expression = Expr.makeExprParser term operatorTable <?> "expression"

-- | Parse an if-else-expression, else is optional.
ifExpr :: Parser Expr
ifExpr = do
    IfExpr
        <$> ((:) <$> ifBranch <*> many (try elseIfBranch))
        <*> optional elseBranch
  where
    ifBranch     = (,) <$ symbol "if" <*> expression <*> block
    elseIfBranch = (,) <$ symbol "else" <* symbol "if" <*> expression <*> block
    elseBranch   = symbol "else" >> block

blockExpr :: Parser Expr
blockExpr = ExprBlock <$> block

loop :: Parser Expr
loop = Loop <$ symbol "loop" <*> block

whileLoop :: Parser Expr
whileLoop = While <$ symbol "while" <*> expression <*> block

breakExpr :: Parser Expr
breakExpr = Break <$ symbol "break" <*> option Unit expression

-- | Parse a type identifier (starts with upper case letter)
type_ :: Parser Type
type_ = lexeme . label "type" $ do
    initial <- T.singleton <$> upperChar
    rest    <- T.pack <$> many alphaNumChar
    pure . Type $ initial <> rest

-- | Parse an identifier of other language constructs than types
identifier :: Parser Identifier
identifier = lexeme . label "identifier" $ do
    initial <- lowerChar <|> single '_'
    rest    <- many (alphaNumChar <|> single '_')
    let idf = T.singleton initial <> T.pack rest
    if idf `Set.notMember` reservedWords
        then pure $ Identifier idf
        else fail "keywords cannot be used as identifiers"

-- | Parse a single function parameter
functionParam :: Parser Parameter
functionParam = (,) <$> identifier <* symbol ":" <*> type_

-- | Parse a statement
statement :: Parser Statement
statement =
    try emptyStatement
        <|> try itemStatement
        <|> try letStatement
        <|> try returnStatement
        <|> try statementExpr
  where
    emptyStatement = StatementEmpty <$ symbol ";"
    itemStatement  = StatementItem <$> item
    statementExpr  = StatementExpr <$> expression <* symbol ";"
    returnStatement =
        StatementReturn <$ symbol "return" <*> optional expression <* symbol ";"
    letStatement =
        StatementLet
            <$> (symbol "let" *> identifier)
            <*> (symbol ":" *> type_)
            <*> (symbol "=" *> expression)
            <*  symbol ";"

-- | Parse a block (a bunch of statements enclosed in braces). It may have an 
-- outer expression, which is used as block's return value.
block :: Parser Block
block = betweenBraces $ Block <$> many statement <*> option Unit expression

-- | Parse a function call
functionCall :: Parser Expr
functionCall = do
    FnCall <$> identifier <*> argList
    where argList = betweenParens (expression `sepBy` symbol ",")

-- | Parse a function declaration. Return type may be omitted.
functionDeclaration :: Parser Item
functionDeclaration =
    Function
        <$  symbol "fn"
        <*> identifier
        <*> paramList
        <*> returnType
        <*> block
  where
    returnType =
        label "return type" $ option (Type "Unit") $ symbol "->" >> type_
    paramList =
        betweenParens (functionParam `sepBy` symbol ",") <?> "parameter list"

-- | Parse an item. Items are components of a crate.
item :: Parser Item
item = functionDeclaration

-- | Parse a whole crate. Empty crates are not allowed.
crate :: Parser Crate
crate = between spaceConsumer eof $ Crate <$> some item

-- | Parse a crate. Parse error is converted to text.
parseCrate :: String -> Text -> Either Text Crate
parseCrate src content =
    first (T.pack . errorBundlePretty) $ parse crate src content

-- | First argument is source file name and second is it's content. @Left@
-- contains parser error message(s) and @Right@ contains the AST as text
parseProgram :: String -> Text -> Either Text Text
parseProgram src content =
    bimap (T.pack . errorBundlePretty) (T.pack . show) $ parse crate src content
