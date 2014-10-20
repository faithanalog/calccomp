{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Asm.QQ (asm, asmf) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.Meta.Parse as P
import Data.Generics
import Asm.Preprocess
import Asm.Parser
import Asm.Expr

-- Quotes assembly code
asm :: QuasiQuoter
asm = QuasiQuoter {
    quoteExp = quoteAsmExp,
    quotePat = notImplemented,
    quoteType = notImplemented,
    quoteDec = notImplemented
}

-- Reads an assembly file and handles includes as well
asmf :: QuasiQuoter
asmf = QuasiQuoter {
    quoteExp = quoteAsmFile,
    quotePat = notImplemented,
    quoteType = notImplemented,
    quoteDec = notImplemented
}

notImplemented = fail "Feature not implemented"

quoteAsmExp :: String -> Q Exp
quoteAsmExp x = do
    file <- loc_filename `fmap` location
    let tree = parseText file . preprocess $ x
    case tree of
        Left err -> fail err
        Right x -> exprsToExp x

quoteAsmFile :: String -> Q Exp
quoteAsmFile fname = do
    contents <- runIO $ preprocessFile fname
    let tree = parseText fname contents
    case tree of
        Left err -> fail err
        Right x -> exprsToExp x


exprsToExp :: [Expr] -> Q Exp
exprsToExp = listE . map conv
    where conv (AntiQuote s) = aqExpr s
          conv (AntiQuoteStr s) = aqStr s
          conv x = dataToExpQ (const Nothing `extQ` convArg) x

-- All valid expression results that we can feed to the parser
-- String is special cased so string results will be parsed as code
class (Show a) => ExpShow a where
    expShow :: a -> String
    expShow = show

instance ExpShow String where
    expShow = id

instance ExpShow Int
instance ExpShow Integer
instance ExpShow Char where
    expShow x = ['\'', x, '\'']

-- Antiquoter
aqExpr :: String -> Q Exp
aqExpr = parseExp [|stmnt . expShow|]

aqStr :: String -> Q Exp
aqStr = parseExp [|String . expShow|]

convArg :: Expr -> Maybe (Q Exp)
convArg (AntiQuoteStr s) = Just $ aqStr s
convArg (AntiQuote s) = Just $ parseExp [|arg . expShow|] s
convArg _ = Nothing

parseExp f s = case P.parseExp s of
                 Left err -> fail err
                 Right x -> f `appE` return x

parseOrErr :: (String -> Either String Expr) -> String -> Expr
parseOrErr f x = case f x of
                  Left e -> error e
                  Right xpr -> xpr

stmnt = parseOrErr parseStatement
arg = parseOrErr parseArg


