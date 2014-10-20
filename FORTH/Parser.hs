module FORTH.Parser (parseForth, Expr(..)) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Control.Applicative ((<$>))
import Control.Monad
import Data.Char
import Data.Maybe
import qualified Text.Parsec.Token as Token

data Expr = Num Int
          | Str String
          | Tok String
          | VarDef String
          | WordDef String [Expr]
          | WordDefAsm String String
          deriving (Show,Eq)

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
    where style = emptyDef {
              Token.commentStart = "(",
              Token.commentEnd = ")",
              Token.caseSensitive = False
          }

decimal       = Token.decimal lexer
lexeme        = Token.lexeme lexer

-- Parsec's hex parser looks for 'x00' not '0x00' so I prefix with 0
hexadecimal   = char '0' >> Token.hexadecimal lexer
whiteSpace    = Token.whiteSpace lexer

identChar = letter <|> oneOf "_+-*/=<>@!."
identifier = many1 (digit <|> identChar)

integer = do
    sign <- option '+' (oneOf "+-")
    num <- try hexadecimal <|> try decimal <|> try charNum
    notFollowedBy identChar
    return $ case sign of
               '+' -> num
               '-' -> -num
    where charNum = do
              char '\''
              c <- anyChar
              char '\''
              return . toInteger . fromEnum $ c

stringEscape :: Parser Char
stringEscape = char '\'' >> oneOf "\\\""

stringLiteral :: Parser String
stringLiteral = do
    char '"'
    str <- many (stringEscape <|> noneOf "\"")
    char '"'
    return str

stringNoCase :: String -> Parser String
stringNoCase "" = return ""
stringNoCase (s:str) = do
    l <- satisfy (\c -> toUpper c == toUpper s)
    r <- stringNoCase str
    return $ l:r

-- Parse language
num :: Parser Expr
num = Num . fromInteger <$> integer

fstring :: Parser Expr
fstring = Str <$> stringLiteral

tok :: Parser Expr
tok = Tok <$> identifier

varDef :: Parser Expr
varDef = do
    try (stringNoCase "variable") <|> stringNoCase "var"
    whiteSpace
    VarDef <$> identifier

wordDef :: Parser Expr
wordDef = do
    stringNoCase "word"
    whiteSpace
    nm <- identifier
    whiteSpace >> char '{'
    whiteSpace
    body <- many (lexeme forthExpr)
    char '}'
    return $ WordDef nm body

wordDefAsm :: Parser Expr
wordDefAsm = do
    stringNoCase "asmword"
    whiteSpace
    nm <- identifier
    whiteSpace >> char '{'
    body <- many $ noneOf "}"
    char '}'
    return $ WordDefAsm nm body

forthExpr :: Parser Expr
forthExpr = try num <|> try fstring <|> try varDef <|> tok

topLevelExpr :: Parser Expr
topLevelExpr = try num <|> try fstring <|> try varDef <|> try wordDef <|> try wordDefAsm <|> tok

parseForth :: String -> Either String [Expr]
parseForth s =
    case parse (whiteSpace >> many1 (lexeme topLevelExpr)) "" s of
      Left err -> Left (show err)
      Right x -> Right x
