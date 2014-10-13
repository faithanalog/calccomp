module Asm.Parser (parseExpr, printTree, parseText) where

import Asm.Expr
import Text.Parsec hiding (space, spaces, hexDigit)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import System.Environment
import Numeric
import Data.List (intercalate, isPrefixOf)
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

hexDigit :: Parser Char
hexDigit = oneOf "abcdefABCDEF0123456789"

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
    num <- try hexadecimal <|> try binary <|> try decimal <|> try charNum
    notFollowedBy lblChar
    return $ if sign == '+' then num else -num

space :: Parser Char
space = char ' '

spaces :: Parser String
spaces = many1 space

tabs :: Parser String
tabs = many1 tab

comment :: Parser String
comment = do
    char ';'
    many (try $ noneOf "\n")

whiteSpace :: Parser ()
whiteSpace = skipMany (spaces <|> tabs <|> comment)

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
    end <- case op of
        '>' -> option "" (try (string ">"))
        '<' -> option "" (try (string "<"))
        _ -> return ""
    return $ op:end

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
num = do
    x <- integer
    return $ Literal (Num $ fromIntegral x)

labelref :: Parser Expr
labelref = do
    ident <- lblIdentifier <|> do x <- char '$'; return [x]
    return $ Literal (Label ident)

constAssign :: Parser Expr
constAssign = do
    name <- lblIdentifier
    symbol "=" <|> (optional (char '.') >> optional (char '.') >> symbol "equ")
    val <- argExpr
    return $ DefineDirective name val

asmstring :: Parser Expr
asmstring = String `fmap` stringLiteral

register :: Parser Expr
register = do
    ident <- identifier
    reg <- case maybeReg ident of
                Nothing -> parserZero
                Just r -> return r
    return $ if regIs8Bit reg then Reg8 reg
        else case reg of
            IX -> Reg16Index IX
            IY -> Reg16Index IY
            _ -> Reg16 reg

regIndirect :: Parser Expr
regIndirect = do
    regName <- parens $ choice [
                symbol "hl",
                symbol "de",
                symbol "bc",
                try $ symbol "ix",
                symbol "iy",
                symbol "sp",
                symbol "c"
            ]
    return $ case regName of
        "ix" -> RegIndex IX (Literal $ Num 0)
        "iy" -> RegIndex IY (Literal $ Num 0)
        "hl" -> Reg8 HL'
        _ -> RegIndir $ getReg regName

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
    optional (oneOf "#.")
    ident <- identifier
    args <- commaSep directiveArg
    return $ case ident of
        "define" -> let (Literal (Label name)) = head args in
            DefineDirective name $ if length args > 1 then args !! 1 else Literal (Num 1)
        _ -> Directive ident args

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

-- Convert top level 'Parens' exprs to AddrIndir exprs. Consider merging with 'removeParens' pass
indirPass :: [Expr] -> [Expr]
indirPass = map conv
    where conv (Instr i args) = Instr i (map convParens args)
          conv x = x
          convParens (Parens xpr) = AddrIndir xpr
          convParens x = x

-- Remove 'Parens' exprs, as we no longer need them
removeParens :: [Expr] -> [Expr]
removeParens = map conv
    where conv (Parens xpr) = conv xpr
          conv (Binop op l r) = Binop op (conv l) (conv r)
          conv (Directive str xs) = Directive str (removeParens xs)
          conv (DefineDirective str xpr) = DefineDirective str (conv xpr)
          conv (RegIndex r xpr) = RegIndex r (conv xpr)
          conv (Instr i xs) = Instr i (removeParens xs)
          conv xpr = xpr

parseExpr :: String -> [Expr]
parseExpr t =
  case parse parseStatements "stdin" t of
    Left err -> error (show err)
    Right ast -> (removeParens . indirPass) ast

printTree :: [Expr] -> String
printTree xprs = unlines (map show xprs)

-- Parse file with includes
parseText :: String -> [Expr]
parseText = parseExpr
