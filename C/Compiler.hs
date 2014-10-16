{-# LANGUAGE QuasiQuotes #-}
module C.Compiler (compileTree) where

import qualified C.Expr as C
import C.Expr (Op)
import Data.Hashable (hash)
import Data.Maybe

import Asm.QQ
import Asm.Expr (Register(..), Instruction(..), Condition(..), litNum, litLbl, reg)
import qualified Asm.Parser as A

import qualified Asm.Expr as A
import qualified Data.Map.Strict as Map

type Id = String

data VarDef = VarDef Id Int Int deriving (Eq)
data FuncDef = FuncDef Id [VarDef] [VarDef] [C.Expr] deriving (Eq)

--TODO Transform C AST to a bunch of these things
data Expr = FuncCall FuncDef [Expr]
          | Var VarDef
          | Num Int
          | String String
          | Binop Op Expr Expr
          | Asm String
          | Nop

data Ctx = Ctx [FuncDef] [VarDef]

sizeOf = C.sizeOf;

regLsb :: Register -> Register
regLsb r
    | r <= L = r
    | r == BC = C
    | r == DE = E
    | r == HL = L

regMsb :: Register -> Register
regMsb r
    | r == BC = B
    | r == DE = D
    | r == HL = H

asmHeader :: String -> [A.Expr]
asmHeader "ti84pcse" = [asm|
    .org UserMem - 2
    .db tExtTok, tAsm84CCmp
    ld ix, saveSScreen + 300h
    call main
    ret
|]

asmHeader "ti83p" = [asm|
    .org UserMem - 2
    .db t2ByteTok, tAsmCmp
    ld ix, saveSScreen + 300h
    call main
    ret
|]

strLabel :: String -> String
strLabel str = "STRCONST_" ++ show (toInteger (hash str) + (2^64))

localVars :: [C.Expr] -> [VarDef]
localVars xprs = reverse $ varTable xprs [] 0
    where varTable [] vars _ = vars
          varTable (C.VarDef name vtype:xprs) vars offs =
              varTable xprs (VarDef ('_':name) (sizeOf vtype) offs:vars) (offs + sizeOf vtype)
          varTable (expr:xprs) vars offs = varTable xprs vars offs

varDefLocal :: VarDef -> A.Expr
varDefLocal (VarDef name _ offs) = A.Define name $ litNum offs

storeRegLocal :: Register -> VarDef -> [A.Expr]
storeRegLocal src (VarDef var size _)
    | size == 1 && src > L = [lsb $ regLsb src]
    | src == HL = [lsb L, msb H]
    | src == DE = [lsb E, msb D]
    | src == BC = [lsb C, msb B]
    | src <= L = lsb src:[zeroMsb | size == 2]
    | otherwise = []
    where lsb r   = A.Instr LD [A.RegIndex IX $ litLbl var, reg r]
          msb r   = A.Instr LD [A.RegIndex IX $ A.Binop A.Add (litLbl var) (litNum 1), reg r]
          zeroMsb = A.Instr LD [A.RegIndex IX $ litNum 1, litNum 0]

loadRegLocal :: Register -> VarDef -> [A.Expr]
loadRegLocal src (VarDef var size _)
    | size == 1 && src > L = [lsb $ regLsb src, zero $ regMsb src]
    | src == HL = [lsb L, msb H]
    | src == DE = [lsb E, msb D]
    | src == BC = [lsb C, msb B]
    | src <= L = [lsb src]
    | otherwise = []
    where lsb r  = A.Instr LD [reg r, A.RegIndex IX $ litLbl var]
          msb r  = A.Instr LD [reg r, A.RegIndex IX $ A.Binop A.Add (litLbl var) (litNum 1)]
          zero r = A.Instr LD [reg r, litNum 0]


findVar' :: [VarDef] -> String -> VarDef
findVar' vars x = head $ filter (\(VarDef nm _ _) -> x == nm) vars

findFunc :: [FuncDef] -> String -> FuncDef
findFunc fns x = head $ filter (\(FuncDef nm _ _ _) -> x == nm) fns


convExpr :: Ctx -> C.Expr -> Expr
convExpr ctx@(Ctx fns _) (C.FuncCall nm args) =
        FuncCall (findFunc fns nm) (map (convExpr ctx) args)

convExpr (Ctx _ vars) (C.Var nm) = Var $ findVar' vars ('_':nm)

convExpr _ (C.Num x) = Num x

convExpr _ (C.String x) = String x

convExpr ctx@(Ctx _ _) (C.Binop op x y) = Binop op (convExpr ctx x) (convExpr ctx y)

convExpr _ (C.Asm str) = Asm str

convExpr _ _ = Nop


convFunc :: C.Expr -> FuncDef
convFunc (C.FuncDef nm ret fnargs body) = FuncDef nm args vars body
    where vars = localVars (fnargs ++ body)
          args = take (length fnargs) vars

convFuncBody :: Ctx -> [C.Expr] -> [Expr]
convFuncBody ctx = map (convExpr ctx)

funcAsm :: (FuncDef, [Expr]) -> [A.Expr]
funcAsm (FuncDef name args vars _, body) =
    prologue ++ varDefs ++ argDefs ++ concatMap exprAsm body ++ epilogue
    where label = name
          negVarSize = sum [s | (VarDef _ s _) <- vars] * (-1)
          varDefs = map varDefLocal vars
          argDefs = concat $ zipWith storeRegLocal [HL,DE,BC] args
          prologue = A.LabelDef label : if null vars then [] else [asm|push ix \ ld de,@{negVarSize} \ add ix,de|]
          epilogue = if null vars then [asm|ret|] else [asm|pop ix \ ret|]

exprAsm :: Expr -> [A.Expr]
exprAsm (FuncCall (FuncDef name _ _ _) args) = ldArgs args ++ [asm|call @l{name}|]
    where ldArgs [] = []
          ldArgs (x:[]) = exprAsm x
          ldArgs (x:y:[]) = concat [
                        exprAsm y,
                        [asm|push hl|],
                        exprAsm x,
                        [asm|pop de|]
                    ]
          ldArgs (x:y:z:[]) = concat [
                        exprAsm z,
                        [asm|push hl|],
                        exprAsm y,
                        [asm|push hl|],
                        exprAsm x,
                        [asm|pop de \ pop bc|]
                    ]
          ldArgs _ = []

exprAsm (Binop C.Assign (Var var) xpr) = exprAsm xpr ++ storeRegLocal HL var


exprAsm (Binop C.Add x y) = concat [
        exprAsm y,
        [asm|push hl|],
        exprAsm x,
        [asm|pop de \ add hl,de|]
    ]

exprAsm (Binop C.Sub x y) = concat [
        exprAsm y,
        [asm|push hl|],
        exprAsm x,
        [asm|pop de \ or a \ sbc hl,de|]
    ]

exprAsm (Asm str) = case A.parseText "" str of
    Left err -> error err
    Right ast -> ast

exprAsm (Var var) = loadRegLocal HL var
exprAsm (Num x) = [asm|ld hl,@{x}|]
exprAsm (String str) = [asm|ld hl,@l{str}|]
exprAsm (Binop op x y) = []
exprAsm x = []


compileTree :: [C.Expr] -> [A.Expr]
compileTree tree = asmHeader "ti84pcse" ++ concatMap funcAsm funcs
    where funcDefs = map convFunc [fn | fn@(C.FuncDef {}) <- tree]
          funcs = [(fn, convFuncBody (Ctx funcDefs vs) body) |
                        fn@(FuncDef _ _ vs body) <- funcDefs]
