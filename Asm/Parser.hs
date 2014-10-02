module Asm.Parser (parseExpr) where

import Asm.Expr
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import System.Environment
import Numeric



-- Lexer

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = emptyDef {
          Token.reservedOpNames = ["+","*","-","="],
          Token.reservedNames = [".org", ".db", ".dw"],
          Token.commentLine = ";",
          Token.identStart = letter <|> char '_',
          Token.identLetter = alphaNum <|> char '_'
        }

decimal :: Parser Integer
decimal = Token.decimal lexer

binary :: Parser Integer
binary = do
    char '%'
    str <- many $ oneOf "01"
    return $ foldl (\l r -> l * 2 + (if r == '0' then 0 else 1)) 0 str

hexadecimal :: Parser Integer
hexadecimal = do
    char '$'
    str <- many hexDigit
    return $ fst . head . readHex $ str

integer :: Parser Integer
integer = do
    sign <- option '+' (oneOf "+-")
    num <- decimal <|> binary <|> hexadecimal
    return $ if sign == '+' then num else -num

symbol :: String -> Parser String
symbol = Token.symbol lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

-- Parser
asmlabel :: Parser Expr
asmlabel = do
    name <- identifier
    char ':'
    return $ Label name

instr :: Parser Expr
instr = do
    ident <- identifier
    args <- commaSep argExpr
    return $ Instr ident args

num :: Parser Expr
num = Num `fmap` integer

labelref :: Parser Expr
labelref = Label `fmap` identifier

asmstring :: Parser Expr
asmstring = String `fmap` stringLiteral

register :: Parser Expr
register = do
    regName <- choice [
              symbol "a",
              symbol "b",
              symbol "c",
              symbol "d",
              symbol "e",
              symbol "h",
              symbol "l",
              symbol "ixh",
              symbol "ixl",
              symbol "iyh",
              symbol "iyl",
              symbol "af",
              symbol "bc",
              symbol "de",
              symbol "hl",
              symbol "ix",
              symbol "iy",
              symbol "r",
              symbol "i",
              symbol "sp",
              symbol "af'"
           ]
    return $ Reg regName

indirectRegister :: Parser Expr
indirectRegister = do
    char '('
    regName <- choice [
                symbol "hl",
                symbol "de",
                symbol "bc",
                symbol "ix",
                symbol "iy"
            ]
    char ')'
    return $ Reg $ '(':regName ++ ")"

directive :: Parser Expr
directive = do
    oneOf "#."
    ident <- identifier
    args <- many (noneOf "\n") >>= commaSep (choice [num, asmstring, labelref])
    return $ Directive ident args

argExpr :: Parser Expr
argExpr = register
       <|> try indirectRegister
       <|> liftM Label identifier
       <|> liftM Num integer

expr :: Parser Expr
expr = directive <|> try asmlabel <|> instr

parseExpr :: String -> [Expr]
parseExpr t =
  case parse (many1 expr) "stdin" t of
    Left err -> error (show err)
    Right ast -> ast
