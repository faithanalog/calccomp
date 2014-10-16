{-# LANGUAGE TemplateHaskell #-}
module Asm.QQ (asm, asmf) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Generics
import Asm.Preprocess
import Asm.Parser
import Asm.Expr

-- Quotes assembly code
asm :: QuasiQuoter
asm = QuasiQuoter {
    quoteExp = quoteAsmExp,
    quotePat = \_ -> fail "illegal raw string QuasiQuote \
                          \(allowed as expression only, used as a pattern)",

    quoteType = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a type)",

    quoteDec = \_ -> fail "illegal raw string QuasiQuote \
                          \(allowed as expression only, used as a declaration)"
}

-- Reads an assembly file and handles includes as well
asmf :: QuasiQuoter
asmf = QuasiQuoter {
    quoteExp = quoteAsmFile,
    quotePat = \_ -> fail "illegal raw string QuasiQuote \
                          \(allowed as expression only, used as a pattern)",

    quoteType = \_ -> fail "illegal raw string QuasiQuote \
                           \(allowed as expression only, used as a type)",

    quoteDec = \_ -> fail "illegal raw string QuasiQuote \
                          \(allowed as expression only, used as a declaration)"
}

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
exprsToExp = dataToExpQ (const Nothing `extQ` antiExprExp)

maybeParse x = case parseStatement x of
    Left e -> error e
    Right xpr -> xpr

antiExprExp :: Expr -> Maybe (Q Exp)
antiExprExp (AntiQuoteNum v) = Just $ appE [|Literal . Num  |] (varE $ mkName v)
antiExprExp (AntiQuoteLbl v) = Just $ appE [|Literal . Label|] (varE $ mkName v)
antiExprExp (AntiQuoteStr v) = Just $ appE [|String         |] (varE $ mkName v)
antiExprExp _ = Nothing
