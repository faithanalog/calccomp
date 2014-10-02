module C.Compiler (compileTree) where

import qualified C.Expr as C
import C.Expr (Op)
import Data.Hashable (hash)
import Data.Maybe
import qualified Data.Map.Strict as Map

type Id = String

data VarDef = VarDef Id Int Int deriving (Eq)
data FuncDef = FuncDef Id [VarDef] [VarDef] [C.Expr] deriving (Eq)

--TODO Transform C AST to a bunch of these things
data Expr = FuncCall FuncDef [Expr]
          | Var VarDef
          | Num Integer
          | String String
          | Binop Op Expr Expr
          | Asm String
          | Nop

data Ctx = Ctx [FuncDef] [VarDef]

data Register = IXH | IXL | IYH | IYL | A | B | C | D | E | H | L | BC | DE | HL | IX | IY deriving (Show,Ord,Eq)

sizeOf = C.sizeOf;

regLsb :: Register -> Register
regLsb r
    | r <= L = r
    | r == BC = C
    | r == DE = E
    | r == HL = L
    | r == IX = IXL
    | r == IY = IYL

regMsb :: Register -> Register
regMsb r
    | r == BC = B
    | r == DE = D
    | r == HL = H
    | r == IX = IXH
    | r == IY = IYH

asmHeader :: String -> String -> String
asmHeader varName "ti84+cse" = unlines [
        ".nolist",
        ".global",
        "#include \"ti84pcse.inc\"",
        ".endglobal",
        ".list",
        ".variablename " ++ varName,
        ".org UserMem-2",
        ".db tExtTok,tAsm84CCmp"
    ] ++ tabLines [
        "ld ix,saveSScreen + 300h",
        "call main",
        "ret"
    ]

asmHeader varName "ti83+" = unlines [
        ".nolist",
        ".global",
        "#include \"ti83plus.inc\"",
        ".endglobal",
        ".list",
        ".variablename " ++ varName,
        ".org UserMem-2",
        ".db t2ByteTok,tAsmCmp"
    ] ++ tabLines [
        "ld ix,saveSScreen + 300h",
        "call main",
        "ret"
    ]

tabLines :: [String] -> String
tabLines = unlines . (map ('\t':))

tabLines' :: [String] -> String
tabLines' = drop 1 . tabLines

strLabel :: String -> String
strLabel str = "STRCONST_" ++ show (toInteger (hash str) + (2^64))

localVars :: [C.Expr] -> [VarDef]
localVars xprs = reverse $ varTable xprs [] 0
    where varTable [] vars _ = vars
          varTable ((C.VarDef name vtype):xprs) vars offs =
              varTable xprs (VarDef ('_':name) (sizeOf vtype) offs:vars) (offs + sizeOf vtype)
          varTable (expr:xprs) vars offs = varTable xprs vars offs

varDefLocal :: VarDef -> String
varDefLocal (VarDef name _ offs) = name ++ " = " ++ show offs

storeRegLocal :: Register -> VarDef -> [String]
storeRegLocal reg (VarDef var size _)
    | size == 1 && reg > L = [lsb $ regLsb reg]
    | reg == HL = [lsb L, msb H]
    | reg == DE = [lsb E, msb D]
    | reg == BC = [lsb C, msb B]
    | reg <= L = lsb reg:if size == 2 then [zeroMsb] else []
    | otherwise = []
    where lsb r = "ld (ix + " ++ var ++ ")," ++ show r
          msb r = "ld (ix + 1 + " ++ var ++ ")," ++ show r
          zeroMsb = "ld (ix + 1),0"

loadRegLocal :: Register -> VarDef -> [String]
loadRegLocal reg (VarDef var size _)
    | size == 1 && reg > L = [lsb $ regLsb reg, zero $ regMsb reg]
    | reg == HL = [lsb L, msb H]
    | reg == DE = [lsb E, msb D]
    | reg == BC = [lsb C, msb B]
    | reg <= L = [lsb reg]
    | otherwise = []
    where lsb r = "ld " ++ show r ++ ",(ix + " ++ var ++ ")"
          msb r = "ld " ++ show r ++ ",(ix + 1 + " ++ var ++ ")"
          zero r = "ld " ++ show r ++ ",0"



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


compileTree :: [C.Expr] -> String
compileTree tree = asmHeader "TESTC" "ti83+" ++ "\n" ++ tabLines (concat (map funcAsm funcs))
    where funcDefs = map convFunc [fn | fn@(C.FuncDef {}) <- tree]
          funcs = [(fn, convFuncBody (Ctx funcDefs vs) body) |
                        fn@(FuncDef _ _ vs body) <- funcDefs]


funcAsm :: (FuncDef, [Expr]) -> [String]
funcAsm ((FuncDef name args vars _), body) =
    prologue ++ varDefs ++ argDefs ++ concat (map exprAsm body) ++ epilogue
    where label = name ++ ":"
          varSize = sum [s | (VarDef _ s _) <- vars]
          varDefs = map varDefLocal vars
          argDefs = concat $ zipWith storeRegLocal [HL,DE,BC] args
          prologue = [
                  label,
                  ".module " ++ name
              ] ++ if null vars then [] else [
                  "push ix",
                  "ld de," ++ show (-varSize),
                  "add ix,de"
              ]
          epilogue = (if null vars then [] else [
                  "pop ix"
              ]) ++ [
                  "ret",
                  ".endmodule"
              ]

exprAsm :: Expr -> [String]
exprAsm (FuncCall (FuncDef name _ _ _) args) = ldArgs args ++ ["call " ++ name]
    where ldArgs [] = []
          ldArgs (x:[]) = exprAsm x
          ldArgs (x:y:[]) = concat [
                        exprAsm y,
                        ["push hl"],
                        exprAsm x,
                        ["pop de"]
                    ]
          ldArgs (x:y:z:[]) = concat [
                        exprAsm z,
                        ["push hl"],
                        exprAsm y,
                        ["push hl"],
                        exprAsm x,
                        ["pop de",
                        "pop bc"]
                    ]
          ldArgs _ = []

exprAsm (Binop C.Assign (Var var) xpr) = exprAsm xpr ++ storeRegLocal HL var

exprAsm (Binop C.Add x y) = exprAsm x ++ ["push hl"] ++ exprAsm y ++
    ["pop de", "add hl,de"]

exprAsm (Binop C.Sub x y) = exprAsm y ++ ["push hl"] ++ exprAsm x ++
    ["pop de", "or a", "sbc hl,de"]

exprAsm (Asm str) = lines str

exprAsm (Var var) = loadRegLocal HL var
exprAsm (Num x) = ["ld hl," ++ show x]
exprAsm (String str) = ["ld hl," ++ strLabel str]
exprAsm (Binop op x y) = [""]
exprAsm x = [""]