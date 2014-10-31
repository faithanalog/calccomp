{-# LANGUAGE DoAndIfThenElse #-}
module Asm.Preprocess (preprocess, preprocessFile, readWithIncludes) where
import Control.Monad
import Data.List (isPrefixOf, intercalate)
import Data.List.Utils (replace, split)
import Data.Char (toUpper)
import qualified Data.ByteString.Lazy as B
import Data.Word

import Text.Parsec hiding (space, spaces, Line)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

-- TODO: Maybe make #defines actually work for find/replace

data Directive = Directive String [String] deriving (Eq)
instance Show Directive where
    show (Directive "endif" _) = ""
    show (Directive dir args) = ('.':dir) ++ (' ':unwords args)

data Line = Dir Directive | Line String deriving (Eq,Show)

comment :: Parser String
comment = do
    char ';'
    many (try $ noneOf "\n")

whiteSpace :: Parser ()
whiteSpace = skipMany (many1 (char ' ') <|> many1 tab <|> comment)

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whiteSpace
    return x

symbol :: Parser String
symbol = lexeme $ do
    h <- letter
    t <- many1 alphaNum
    return (h:t)

commaSep :: Parser a -> Parser [a]
commaSep p = lexeme $ p `sepBy` (lexeme $ char ',')

directive :: Parser Directive
directive = do
    char '#' <|> char '.'
    dir <- symbol
    args <- many1 (noneOf " ") `sepBy` many1 (char ' ' <|> tab)
    return $ Directive dir args

stringNoCase :: String -> Parser String
stringNoCase "" = return ""
stringNoCase (s:str) = do
    l <- satisfy (\c -> toUpper c == toUpper s)
    r <- stringNoCase str
    return $ l:r

parseLine :: String -> Line
parseLine ln = case parse directive "" ln of
    Left _ -> Line ln
    Right x -> Dir x

isDirective :: String -> Line -> Bool
isDirective x (Dir (Directive d _)) = x == d
isDirective _ _ = False

-- Gets all directives of a certain type in a file
directives :: String -> String -> [Directive]
directives text dir = [d | Dir d <- matches]
    where matches = filter (isDirective dir) . map parseLine . lines $ text

binaryInc :: String -> IO String
binaryInc str = do
    contents <- B.readFile str
    let bytes = B.unpack contents
    return $ ".db " ++ intercalate "," (map show bytes)

readWithIncludes :: FilePath -> IO String
readWithIncludes file = do
    text <- readFile file
    unlines `fmap` mapM procLine (lines text)
    where fname f = if head f == '"' then init . tail $ f else f
          procLine l = do
              let line = parseLine l
              case line of
                  Line ln -> return $ ln
                  Dir (Directive "include" (f:_)) ->
                      if file == (fname f) then return "" --No recursive includes
                      else readWithIncludes (fname f)
                  Dir (Directive "incbin" (f:_)) -> binaryInc (fname f)
                  Dir d -> return $ show d

processDefines :: String -> String
processDefines text = unlines $ transform [] (map parseLine $ lines text)
    where transform defs [] = []
    	  transform defs (Dir (Directive "define" (a:_)):lns) = transform (a:defs) lns
          transform defs (Dir (Directive "undef" (a:_)):lns) = transform (filter (/=a) defs) lns
          transform defs (Dir (Directive "ifdef" (a:_)):lns) = transform defs $
              if a `elem` defs then lns
              else
                  tail $ dropWhile (not . isDirective "endif") lns
          transform defs (Line ln:lns) = ln : transform defs lns
          transform defs (Dir d@(Directive _ _):lns) = transform defs (Line (show d):lns)

-- Hard coded bcall replacement
data OSCall = BCall String | BJump String
instance Show OSCall where
    show (BCall lbl) = "rst 28h\n.dw " ++ lbl
    show (BJump lbl) = "call 50h\n.dw " ++ lbl

processBCalls :: String -> String
processBCalls text = unlines (map convCalls $ lines text)
    where convCalls ln = case parse (whiteSpace >> osCall) "" ln of
              Left _ -> ln
              Right x -> show x
          osCall = do
              oneOf "bB"
              optional $ char '_'
              t <- stringNoCase "call" <|> stringNoCase "jump"
              oneOf "( "
              lbl <- many1 (alphaNum <|> char '_')
              return $ case map toUpper t of
                  "CALL" -> BCall lbl
                  "JUMP" -> BJump lbl


-- Replace x \ y with x(newline)y
processNewlines :: String -> String
processNewlines = replace "\\" "\n"

-- Preprocess some asm code, which means parsing BCalls, handling #ifdefs,
-- and changing \ to \n
preprocess :: String -> String
preprocess = processNewlines . processDefines . processBCalls

-- Reads a file, processing includes and applying the
-- pre-processor to the resulting contents
preprocessFile :: FilePath -> IO String
preprocessFile fname = do
    text <- readWithIncludes fname
    return $ preprocess text
