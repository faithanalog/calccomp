module C.Parser (parseExpr) where

import C.Expr
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (javaStyle)
import qualified Text.Parsec.Token as Token
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import System.Environment
import Numeric



-- Lexer

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = javaStyle {
          Token.opStart = oneOf "+-*/=",
          Token.reservedNames = ["int"]
        }

integer :: Parser Integer
integer = Token.integer lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

semi :: Parser String
semi = Token.semi lexer

comma :: Parser String
comma = Token.comma lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

operator :: Parser String
operator = Token.operator lexer

identifier :: Parser String
identifier = Token.identifier lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

varTypeQualifier :: Parser DataType
varTypeQualifier = do
    tq <- choice [
                try $ symbol "uint8",
                try $ symbol "uint16"
            ]
    -- tq <- symbol "uint" >>= (symbol "8" <|> symbol "16")
    return $ typeOf tq
    where typeOf "uint8" = Uint8
          typeOf "uint16" = Uint16

typeQualifier :: Parser DataType
typeQualifier = varTypeQualifier <|> do
    tq <- symbol "void"
    return Void


-- Parser

num :: Parser Expr
num = do
    x <- integer
    return $ Num (fromInteger x)

cstring :: Parser Expr
cstring = String `fmap` stringLiteral

funcDef :: Parser Expr
funcDef = do
    dataType <- typeQualifier
    name <- identifier
    args <- parens $ varDef `sepBy` comma
    body <- braces $ many statement
    return $ FuncDef name dataType args body

varDef :: Parser Expr
varDef = do
    dataType <- varTypeQualifier
    name <- identifier
    return $ VarDef name dataType

var :: Parser Expr
var = do
    name <- identifier
    return $ Var name

asm :: Parser Expr
asm = do
    symbol "asm"
    body <- braces $ many $ noneOf "{}"
    return $ Asm body

varAssign :: Parser Expr
varAssign = do
    v <- var
    symbol "="
    val <- expr
    return $ assignExpr v val

mathOp :: Parser (Expr -> Expr -> Expr)
mathOp = do
    op <- operator
    return $ case op of
                "+" -> addExpr
                "-" -> subExpr
                "*" -> mulExpr
                "/" -> divExpr

binOp :: Parser Expr
binOp = chainl1 (try funcCall <|> try var <|> num <|> parens expr) mathOp

funcCall :: Parser Expr
funcCall = do
    name <- identifier
    args <- parens $ expr `sepBy` comma
    return $ FuncCall name args

statement :: Parser Expr
statement = try asm <|> do
    xpr <- expr
    semi
    return xpr

expr :: Parser Expr
expr = try varDef
    <|> try varAssign
    <|> try funcCall
    <|> try binOp
    <|> try var
    <|> num
    <|> cstring
    <|> parens expr

topLevelStatement :: Parser Expr
topLevelStatement = try funcDef
            <|> do var <- varDef
                   semi
                   return var


parseExpr :: String -> [Expr]
parseExpr t =
  case parse (many1 $ whiteSpace >> topLevelStatement) "stdin" t of
    Left err -> error (show err)
    Right ast -> ast
