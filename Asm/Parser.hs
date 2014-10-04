module Asm.Parser (parseExpr, printTree) where

import Asm.Expr
import Text.Parsec hiding (space, spaces)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import System.Environment
import Numeric
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Char (ord, toUpper)


-- Lexer

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = emptyDef {
          Token.opStart = oneOf "+-*/=<>",
          Token.opLetter = oneOf "<>=",
          Token.reservedNames = [".org", ".db", ".dw"],
          Token.commentLine = ";",
          Token.identStart = letter <|> char '_',
          Token.identLetter = alphaNum <|> char '_',
          Token.caseSensitive = False
        }

parseBin :: String -> Integer
parseBin = foldl (\l r -> l * 2 + (if r == '1' then 1 else 0)) 0

binPrefix :: Parser Integer
binPrefix = do
    char '%'
    str <- many1 $ oneOf "01"
    return $ parseBin str

binSuffix :: Parser Integer
binSuffix = do
    str <- many1 $ oneOf "01"
    oneOf "Bb"
    return $ parseBin str

binary :: Parser Integer
binary = binPrefix <|> binSuffix

hexPrefix :: Parser Integer
hexPrefix = do
    char '$'
    str <- many1 hexDigit
    return $ fst . head . readHex $ str

hexSuffix :: Parser Integer
hexSuffix = do
    str <- many1 hexDigit
    oneOf "Hh"
    return $ fst . head . readHex $ str

hexadecimal :: Parser Integer
hexadecimal = hexPrefix <|> hexSuffix

decimal :: Parser Integer
decimal = do
    str <- many1 digit
    return $ read str

charNum :: Parser Integer
charNum = do
    char '\''
    c <- anyChar
    char '\''
    return $ toInteger (ord c)

integer :: Parser Integer
integer = lexeme $ do
    sign <- option '+' (oneOf "+-")
    num <- try binary <|> try hexadecimal <|> try decimal <|> try charNum
    notFollowedBy lblChar
    return $ if sign == '+' then num else -num

space :: Parser Char
space = char ' '

spaces :: Parser String
spaces = many1 space

comment :: Parser String
comment = do
    char ';'
    many (try $ noneOf "\n")

whiteSpace :: Parser ()
whiteSpace = skipMany (spaces <|> comment)

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whiteSpace
    return x


stringNoCase :: String -> Parser String
stringNoCase "" = return ""
stringNoCase (s:str) = do
    l <- satisfy (\c -> toUpper c == toUpper s)
    r <- stringNoCase str
    return $ l:r

symbol :: String -> Parser String
symbol = lexeme . stringNoCase

parens :: Parser a -> Parser a
parens p = do
    symbol "("
    x <- p
    symbol ")"
    return x

braces :: Parser a -> Parser a
braces p = do
    symbol "{"
    x <- p
    symbol "}"
    return x

semi :: Parser String
semi = symbol ";"

comma :: Parser String
comma = symbol ","

operator :: Parser String
operator = lexeme $ do
    op <- oneOf "+-*/><%"
    return [op]

identifier :: Parser String
identifier = lexeme $ do
    x <- letter <|> char '_'
    xs <- many (alphaNum <|> char '_')
    return $ x:xs

commaSep :: Parser a -> Parser [a]
commaSep p = lexeme $ p `sepBy` comma

semiSep :: Parser a -> Parser [a]
semiSep p = lexeme $ p `sepBy` semi

stringEscape :: Parser Char
stringEscape = char '\'' >> oneOf "\\\""

stringLiteral :: Parser String
stringLiteral = lexeme $ do
    char '"'
    str <- many (stringEscape <|> noneOf "\"")
    char '"'
    return str

singleton :: Parser a -> Parser [a]
singleton p = do
    x <- p
    return [x]

lblChar :: Parser Char
lblChar = alphaNum <|> char '_'

lblIdentifier :: Parser String
lblIdentifier = lexeme $ many1 lblChar

-- Parser
asmlabel :: Parser Expr
asmlabel = lexeme $ do
    name <- lblIdentifier
    char ':'
    return $ LabelDef name

condition :: Parser Expr
condition = do
    condName <- choice [
                try $ symbol "z",
                try $ symbol "nz",
                try $ symbol "c",
                try $ symbol "nc",
                try $ symbol "po",
                try $ symbol "pe",
                try $ symbol "p",
                try $ symbol "m"
            ]
    return $ Cond $ getCond condName

instr :: Parser Expr
instr = do
    ident <- identifier
    instr <- case maybeInstr ident of
            Nothing -> parserZero
            Just x -> return x
    let jpCond = do
            cond <- option [] $ try $ do
                x <- condition
                symbol ","
                return [x]
            arg <- argExpr
            return $ cond ++ [arg]
    args <- case instr of
            CALL -> jpCond
            JR -> jpCond
            JP -> jpCond
            RET -> option [] $ try $ singleton condition
            instr -> case numArgs instr of
                0 -> return []
                1 -> singleton argExpr
                2 -> do
                    xpr1 <- argExpr
                    symbol ","
                    xpr2 <- argExpr
                    return [xpr1, xpr2]
    return $ Instr instr args

num :: Parser Expr
num = Num `fmap` integer

labelref :: Parser Expr
labelref = Label `fmap` (lblIdentifier <|> do x <- char '$'; return [x])

constAssign :: Parser Expr
constAssign = do
    name <- lblIdentifier
    symbol "="
    val <- argExpr
    return $ Directive "define" [Label name, val]

asmstring :: Parser Expr
asmstring = String `fmap` stringLiteral

register :: Parser Expr
register = do
    ident <- identifier
    reg <- case maybeReg ident of
                Nothing -> parserZero
                Just r -> return r
    return $ Reg reg

regIndirect :: Parser Expr
regIndirect = do
    regName <- parens $ choice [
                symbol "hl",
                symbol "de",
                symbol "bc",
                try $ symbol "ix",
                symbol "iy",
                symbol "c"
            ]
    return $ RegIndir $ getReg regName

regIndex :: Parser Expr
regIndex = do
    (regName, offs) <- parens $ do
        reg <- choice [try $ symbol "ix", symbol "iy"]
        offs <- labelref <|> num
        return (reg, offs)
    return $ RegIndex (getReg regName) offs

addrIndirect :: Parser Expr
addrIndirect = do
    addr <- parens mathExpr
    return $ AddrIndir addr

directive :: Parser Expr
directive = do
    oneOf "#."
    ident <- identifier
    args <- commaSep directiveArg
    return $ Directive ident args

mathOp :: Parser (Expr -> Expr -> Expr)
mathOp = do
    op <- operator
    return $ case op of
                "+" -> Binop Add
                "-" -> Binop Sub
                "*" -> Binop Mul
                "/" -> Binop Div
                "<" -> Binop Lt
                ">" -> Binop Gt
                "%" -> Binop Mod
                "<<" -> Binop LShift
                ">>" -> Binop RShift

parensExpr :: Parser Expr -> Parser Expr
parensExpr p = do
    xpr <- parens p
    return $ Parens xpr

binOp :: Parser Expr
binOp = chainl1 (try num <|> try labelref <|> try (parensExpr mathExpr)) mathOp

binOp' :: Parser Expr
binOp' = do
    lft <- labelref
    op <- mathOp
    rt <- labelref
    return $ op lft rt

mathExpr :: Parser Expr
mathExpr = try binOp
        <|> try num
        <|> labelref

directiveArg :: Parser Expr
directiveArg = try mathExpr
            <|> asmstring
            <|> parens directiveArg

argExpr :: Parser Expr
argExpr = try register
       <|> try regIndirect
       <|> try regIndex
       <|> try binOp


statement :: Parser Expr
statement = try directive <|> try constAssign <|> try asmlabel <|> instr

parseStatements :: Parser [Expr]
parseStatements = do
    let parseLine = whiteSpace >> optionMaybe (try statement)
    stmnts <- parseLine `sepBy` many1 newline
    return $ catMaybes stmnts

indirPass :: [Expr] -> [Expr]
indirPass = map conv
    where conv (Instr i args) = Instr i (map convParens args)
          conv x = x
          convParens (Parens xpr) = AddrIndir xpr
          convParens x = x

parseExpr :: String -> [Expr]
parseExpr t =
  case parse parseStatements "stdin" t of
    Left err -> error (show err)
    Right ast -> indirPass ast

printTree :: [Expr] -> String
printTree xprs = unlines (map show xprs)
