module Asm.Preprocess (preprocess) where
import Control.Monad
import Data.List (isPrefixOf, intercalate)
import Data.List.Utils (replace, split)
import Data.Char (toUpper)

import Text.Parsec hiding (space, spaces, Line)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

-- TODO: Get #defines off the crack and make it so ..equ isn't valid


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

directive :: Parser Directive
directive = do
    char '#' <|> char '.'
    dir <- symbol
    args <- many1 (noneOf " ") `sepBy` many1 (char ' ' <|> tab)
    return $ Directive dir args

directiveType :: String -> Parser Directive
directiveType x = do
    dir@(Directive t _) <- directive
    if t == x then
        return dir
    else
        parserZero

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
isDirective x (Dir (Directive dir _)) = x == dir
isDirective _ _ = False

-- Gets all directives of a certain type in a file
directives :: String -> String -> [Directive]
directives text dir = [d | Dir d@(Directive t _) <- map parseLine $ lines text, t == dir]

readWithIncludes :: String -> IO String
readWithIncludes file = do
    text <- readFile file
    let incs = directives text "include"
    files <- forM incs $ \(Directive _ (f:_)) ->
        let fname = if head f == '"' then init . tail $ f else f in
            if file == fname then return "" else readWithIncludes fname
    return $ unlines (files ++ [text])

substitute :: [(String,String)] -> String -> String
substitute _ x = x
-- substitute [] str = str
-- substitute ((old,new):defs) str = substitute defs (replace old new str)

processDefines :: String -> String
processDefines text = unlines $ transform [] (map parseLine $ lines text)
    where transform defs [] = []
    	  transform defs (Dir (Directive "define" (a:b)):lns) = transform ((a,unwords b):defs) lns
          transform defs (Dir (Directive "undef" (a:_)):lns) = transform [x | x@(o,_) <- defs, o /= a] lns
          transform defs (Dir (Directive "ifdef" (a:_)):lns) = transform defs $
              if any (\(x,_) -> x == a) defs then
                  lns
              else
                  tail $ dropWhile (not . isDirective "endif") lns
          transform defs (Line ln:lns) = substitute defs ln : transform defs lns
          transform defs (Dir d@(Directive _ _):lns) = transform defs (Line (show d):lns)

-- Hard coded bcall replacement LOL
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

preprocess :: String -> IO String
preprocess fname = do
    text <- readWithIncludes fname
    let out = processNewlines . processDefines . processBCalls $ text
    return out
