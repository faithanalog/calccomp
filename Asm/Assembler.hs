{-# LANGUAGE DoAndIfThenElse #-}
module Asm.Assembler (
    assemble,
    makeProgVar,
    makeFile,
    assembleText,
    assembleFile,
    VarType(..)) where

import Asm.Expr
import Asm.Parser
import Asm.Preprocess
import Asm.InstrSize
import Data.Bits
import Control.Monad
import Data.Monoid
import Data.ByteString.Lazy.Builder
import Data.Char
import qualified Asm.InstrBytes as IB
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as B

type Labels = Map.Map String Int
type MemLocs = [(Int,Int)]

data VarType = Prog | EditLockedProg | AppVar

-- Evaluates a numeric constant
evalExpr :: Labels -> Expr -> Either String Int
evalExpr lbls (Literal (Num x)) = Right x
evalExpr lbls (Literal (Label l)) = case Map.lookup (map toUpper l) lbls of
                                        Nothing -> Left $ "Label not found: " ++ l
                                        Just x -> Right x
evalExpr lbls (Binop op l r) = do
    left <- evalExpr lbls l
    right <- evalExpr lbls r
    return $ case op of
        Add -> left + right
        Sub -> left - right
        Mul -> left * right
        Div -> left `div` right
        Lt -> if left < right then 1 else 0
        Gt -> if left > right then 1 else 0
        LShift -> left `shift` right
        RShift -> left `shift` (-right)
        Mod -> left `mod` right
        And -> left .&. right
        Or -> left .|. right
        Xor -> left `xor` right

evalExpr lbls xpr = Left $ "Could not evaluate: " ++ show xpr

sizeOf :: Expr -> Int
sizeOf (Directive "db" args) = sum $ map dbSize args
    where dbSize (String s) = length s
          dbSize x = 1
sizeOf (Directive "dw" args) = sizeOf (Directive "db" args) * 2
sizeOf (Instr ins args) = instrSize ins args
sizeOf _ = 0

labelValues :: [Expr] -> Either String Labels
labelValues xprs = values xprs 0 Map.empty
    where values [] _ labels = Right labels

          values (Directive "org" (org:_):xs) o labels = do
              off <- evalExpr labels org
              values xs off labels

          values (Directive "relocate" (org:_):xs) o labels = values (Directive "org" [org]:xs) o labels

          values (LabelDef lbl:xs) o labels = values xs o (addLabel lbl o labels)

          values (Define lbl x:xs) o labels = do
              -- Insert current offset as "$" label
              val <- evalExpr (Map.insert "$" o labels) x
              values xs o (addLabel lbl val labels)

          values (x:xs) o labels = values xs (o + sizeOf x) labels

          addLabel lbl = Map.insert (map toUpper lbl)

directives :: String -> [Expr] -> [[Expr]]
directives dir xprs = [args | (Directive x args) <- xprs, x == dir]

defines :: [Expr] -> [(String,Expr)]
defines xprs = [(name,val) | (Define name val) <- xprs]

memLocs :: [Expr] -> Labels -> Either String MemLocs
memLocs xprs lbls = forM memDefs $
        \(loc:size:_) -> do
            l <- evalExpr lbls loc
            s <- evalExpr lbls size
            return (l,s)
    where memDefs = directives "varloc" xprs

allocMem :: Int -> MemLocs -> Either String (Int,MemLocs)
allocMem size mem
    | null matches = Left "No mem to allocate var."
    | otherwise = let ((addr,bytes):t) = matches in
        Right (addr, (addr + size, bytes - size):t)
    where matches = [x | x@(_,bytes) <- mem, bytes >= size]

allocVars :: [Expr] -> Labels -> MemLocs -> Either String Labels
allocVars xprs lbls varlocs = do
        vars <- forM allocs $ \(size:Literal (Label name):_) -> do
            s <- parseSize size
            return (name,s)
        addrs <- allocAll vars varlocs []
        return $ Map.fromList addrs
    where allocs = directives "var" xprs
          parseSize xpr = case xpr of
              Literal (Label "byte") -> Right 1
              Literal (Label "word") -> Right 2
              _ -> evalExpr lbls xpr
          allocAll [] _ out = Right out
          allocAll ((name,size):vars) varlocs out = do
              (addr,newlocs) <- allocMem size varlocs
              allocAll vars newlocs ((name,addr):out)

allExprBytes :: [Expr] -> Labels -> Either String Builder
allExprBytes xprs labels = bytes xprs 0
    where bytes [] _ = Right mempty
          bytes (Directive "org" (org:_):xs) o = do
              off <- evalExpr labels org
              bytes xs off
          bytes (Directive "relocate" (org:_):xs) o = bytes (Directive "org" [org]:xs) o
          bytes (x:xs) o = do
              xb <- getExprBytes (Map.insert "$" o labels) x
              xsb <- bytes xs (o + sizeOf x)
              return $ xb <> xsb


getExprBytes :: Labels -> Expr -> Either String Builder
getExprBytes lbls (Instr ins args) = getInstrBytes lbls ins args

getExprBytes lbls (Directive "db" args) = do
    bytes <- concat `fmap` mapM (getDataBytes lbls) args
    return $ foldl1 (<>) $ map (word8 . fromIntegral) bytes

getExprBytes lbls (Directive "dw" args) = do
    bytes <- concat `fmap` mapM (getDataBytes lbls) args
    return $ foldl1 (<>) $ map (word16LE . fromIntegral) bytes

getExprBytes _ _ = Right mempty


getDataBytes :: Labels -> Expr -> Either String [Int]
getDataBytes _ (String str) = Right $ map fromEnum str
getDataBytes lbls l = do
    c <- evalExpr lbls l
    return [c]

getInstrBytes :: Labels -> Instruction -> [Expr] -> Either String Builder
getInstrBytes lbls ins args = do
    newArgs <- mapM conv args
    if ins == DJNZ || ins == JR then
        let IB.Num addr = last newArgs
            Just pc = Map.lookup "$" lbls
            off = addr - (pc + 2)
        in if off < -128 || off > 127 then
            Left $ "Could not jump offset of " ++ show off ++ " in instruction " ++ show (Instr ins args)
        else
            Right $ IB.instrBytes ins $ if length newArgs == 1 then [IB.Num off] else head newArgs:[IB.Num off]
    else
        return $ IB.instrBytes ins newArgs
    where conv (Reg8 r) = Right $ IB.Reg8 r
          conv (Reg16 r) = Right $ IB.Reg16 r
          conv (Reg16Index r) = Right $ IB.Reg16Index r
          conv (RegIndir r) = Right $ IB.RegIndir r
          conv (Cond c) = Right $ IB.Cond c
          conv (RegIndex r x) = do
              off <- evalExpr lbls x
              return $ IB.RegIndex r off
          conv (AddrIndir x) = do
              addr <- evalExpr lbls x
              return $ IB.AddrIndir addr
          conv x = do
              c <- evalExpr lbls x
              return $ IB.Num c

-- CONVENIENCE!!!!!!!!!!!!!
w8 :: (Integral a) => a -> Builder
w8 = word8 . fromIntegral

w16 :: (Integral a) => a -> Builder
w16 = word16LE . fromIntegral

w8l :: (Integral a) => [a] -> Builder
w8l x = foldl1 (<>) (map w8 x)

-- Generates bytes for a progvar or appvar
makeProgVar :: String -> VarType -> B.ByteString -> B.ByteString
makeProgVar name vtype contents = toLazyByteString $
    w16 0x0d
    <> w16 (size + 2)
    <> w8 typeid
    <> w8l (take 8 $ map fromEnum name ++ repeat 0)
    <> w8 0
    <> w8 0 -- Set to 0x80 for archive
    <> w16 (size + 2) -- Yes this is here twice
    <> w16 size
    <> lazyByteString contents
    where size = B.length contents
          typeid = case vtype of
              Prog -> 0x05
              EditLockedProg -> 0x06
              AppVar -> 0x15

-- Generates bytes for a file to be sent to a calc with a prog/appvar
makeFile :: String -> VarType -> B.ByteString -> B.ByteString
makeFile name vtype contents = toLazyByteString $
    string8 "**TI83F*"
    <> w8l [0x1A, 0x0A, 0x00]
    <> w8l (replicate 42 0)
    <> w16 (B.length var)
    <> lazyByteString var
    <> w16 checksum
    where var = makeProgVar name vtype contents
          checksum = B.foldl (\x y -> x + fromIntegral y) 0 var

-- Assembles the AST and returns the instruction bytes
assemble :: [Expr] -> Either String B.ByteString
assemble ast = do
    lbls <- labelValues ast
    varlocs <- memLocs ast lbls
    allocated <- Map.union lbls `fmap` allocVars ast lbls varlocs
    toLazyByteString `fmap` allExprBytes ast allocated


-- Assembles assembly code which is assumed to have already been preprocessed, wraps it in
-- a TI file
-- Although no file reading is done, a filename must be passed for the
-- parser to report errors for it. It may be ""
assembleText :: String -> String -> String -> VarType -> Either String B.ByteString
assembleText fname fcontents vname vtype = do
    tree <- parseText fcontents fname
    bytes <- assemble tree
    return $ makeFile vname vtype bytes

-- Reads a file, preprocesses the text, parses it, assembles the resulting AST,
-- and wraps it in a TI file
--
-- assembleFile "code.z80" "CODE" EditLockedProg will assemble file "code.z80" and output
-- a variable named CODE of type program (that's the 0x05)
assembleFile :: FilePath -> String -> VarType -> IO (Either String B.ByteString)
assembleFile fname vname vtype = do
    text <- preprocess fname
    return $ assembleText fname text vname vtype
