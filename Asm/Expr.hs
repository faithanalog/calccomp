module Asm.Expr where

import Data.Char (toUpper)
import Data.List (intercalate)

data Register = IXH | IXL | IYH | IYL | A | B | C | D | E | H | L | HL' | I | R | BC | DE | HL | IX | IY | SP | AF | AF' deriving (Show,Ord,Eq,Read)

data Instruction = EX | EXX | LD | LDD | LDDR | LDI | LDIR | POP | PUSH
                 | ADC | ADD | CP | CPD | CPDR | CPI | CPIR | CPL | DAA
                 | DEC | INC | NEG | SBC | SUB
                 | AND | BIT | CCF | OR | RES | SCF | SET | XOR
                 | RL | RLA | RLC | RLCA | RLD
                 | RR | RRA | RRC | RRCA | RRD | SLA | SRA | SRL
                 | CALL | DJNZ | JP | JR | NOP | RET | RETI | RETN | RST
                 | DI | EI | HALT | IM | IN | IND | INDR | INI | INIR
                 | OTDR | OTIR | OUT | OUTD | OUTI deriving (Eq,Show,Read)

data Op = Add | Sub | Mul | Div | Lt | Gt | LShift | RShift | Mod deriving (Eq)

data Condition = CondNZ | CondZ | CondNC | CondC | CondPO | CondPE | CondP | CondM | CondNOCOND deriving (Eq,Read,Enum)

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Lt = "<"
    show Gt = ">"
    show Mod = "%"
    show LShift = "<<"
    show RShift = ">>"

instance Show Condition where
    show CondZ = "z"
    show CondNZ = "nz"
    show CondC = "c"
    show CondNC = "nc"
    show CondPO = "po"
    show CondPE = "pe"
    show CondP = "p"
    show CondM = "m"

data Literal = Label String | Num Int deriving (Eq)

instance Show Literal where
    show (Label lbl) = lbl
    show (Num x) = show x

data Expr = LabelDef String
          | Instr Instruction [Expr]
          | Cond Condition
          | Directive String [Expr]
          | DefineDirective String Expr
          | Reg8 Register
          | Reg16 Register
          | Reg16Index Register
          | RegIndir Register
          | RegIndex Register Expr
          | AddrIndir Expr
          | Literal Literal
          | String String
          | Binop Op Expr Expr
          | Parens Expr
          deriving (Eq)

instance Show Expr where
    show (Literal x) = show x
    show (LabelDef lbl) = lbl ++ ":"
    show (Instr ins xprs) = show ins ++ " " ++ (intercalate ", " $ map show xprs)
    show (Cond cond) = show cond

    show (Directive "define" xprs) = show (head xprs) ++ " = " ++ show (xprs !! 1)
    show (Directive dir xprs) = "." ++ dir ++ " " ++ (intercalate ", " $ map show xprs)
    show (DefineDirective lbl xpr) = lbl ++ " = " ++ (show xpr)

    show (Reg8 reg) = show reg
    show (Reg16 reg) = show reg
    show (Reg16Index reg) = show reg
    show (RegIndir reg) = "(" ++ show reg ++ ")"
    show (RegIndex reg xpr) = "(" ++ show reg ++ " + " ++ show xpr ++ ")"
    show (AddrIndir xpr) = "(" ++ show xpr ++ ")"

    show (String str) = show str
    show (Binop op lft rt) = showOpArg lft ++ " " ++ show op ++ " " ++ showOpArg rt
    show (Parens xpr) = "((" ++ show xpr ++ "))"

-- Top level binary ops should have no parentheses, nested ones should
showOpArg :: Expr -> String
showOpArg bop@(Binop {}) = "(" ++ show bop ++ ")"
showOpArg x = show x

getInstr :: String -> Instruction
getInstr i = read (map toUpper i)

maybeInstr :: String -> Maybe Instruction
maybeInstr i
    | null res = Nothing
    | otherwise = Just $ fst . head $ res
    where res = reads (map toUpper i)

maybeReg :: String -> Maybe Register
maybeReg r
    | null res = Nothing
    | otherwise = Just $ fst . head $ res
    where res = reads (map toUpper r)

getReg :: String -> Register
getReg r = read (map toUpper r)

getCond :: String -> Condition
getCond c = read $ "Cond" ++ map toUpper c

regIs8Bit :: Register -> Bool
regIs8Bit r = r <= R

regIs16Bit :: Register -> Bool
regIs16Bit = not . regIs8Bit

numArgs :: Instruction -> Int
numArgs EX = 2
numArgs EXX = 0
numArgs LD = 2
numArgs LDD = 0
numArgs LDDR = 0
numArgs LDI = 0
numArgs LDIR = 0
numArgs POP = 1
numArgs PUSH = 1
numArgs ADC = 2
numArgs ADD = 2
numArgs CP = 1
numArgs CPD = 0
numArgs CPDR = 0
numArgs CPI = 0
numArgs CPIR = 0
numArgs CPL = 0
numArgs DAA = 0
numArgs DEC = 1
numArgs INC = 1
numArgs NEG = 0
numArgs SBC = 2
numArgs SUB = 1
numArgs AND = 1
numArgs BIT = 2
numArgs CCF = 0
numArgs OR = 1
numArgs RES = 2
numArgs SCF = 0
numArgs SET = 2
numArgs XOR = 1
numArgs RL = 1
numArgs RLA = 0
numArgs RLC = 1
numArgs RLCA = 0
numArgs RLD = 0
numArgs RR = 1
numArgs RRA = 0
numArgs RRC = 1
numArgs RRCA = 0
numArgs RRD = 0
numArgs SLA = 1
numArgs SRA = 1
numArgs SRL = 1
numArgs CALL = 2
numArgs DJNZ = 1
numArgs JP = 2
numArgs JR = 2
numArgs NOP = 0
numArgs RET = 1
numArgs RETI = 0
numArgs RETN = 0
numArgs RST = 1
numArgs DI = 0
numArgs EI = 0
numArgs HALT = 0
numArgs IM = 1
numArgs IN = 2
numArgs IND = 0
numArgs INI = 0
numArgs INIR = 0
numArgs OTDR = 0
numArgs OTIR = 0
numArgs OUT = 2
numArgs OUTD = 0
numArgs OUTI = 0
