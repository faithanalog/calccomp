module Asm.Expr where

import Data.Char (toUpper)
import Data.List (intercalate)

-- HL' is (HL) since (HL) is treated like an 8 bit register
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

data Op = Add | Sub | Mul | Div | Lt | Gt | LShift | RShift | Mod | Or | And | Xor deriving (Eq)

data Condition = CondNZ | CondZ | CondNC | CondC | CondPO | CondPE | CondP | CondM deriving (Eq,Read,Enum)

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
    show Or = "|"
    show And = "&"
    show Xor = "^"

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
    show (Instr ins xprs) = show ins ++ " " ++ intercalate ", " (map show xprs)
    show (Cond cond) = show cond

    show (Directive "define" xprs) = show (head xprs) ++ " = " ++ show (xprs !! 1)
    show (Directive dir xprs) = "." ++ dir ++ " " ++ intercalate ", " (map show xprs)
    show (DefineDirective lbl xpr) = lbl ++ " = " ++ show xpr

    show (Reg8 reg) = show reg
    show (Reg16 reg) = show reg
    show (Reg16Index reg) = show reg
    show (RegIndir reg) = "(" ++ show reg ++ ")"
    show (RegIndex reg xpr) = "(" ++ show reg ++ " + " ++ show xpr ++ ")"
    show (AddrIndir xpr) = "(" ++ show xpr ++ ")"

    show (String str) = show str
    show (Binop op lft rt) = showOpArg lft ++ " " ++ show op ++ " " ++ showOpArg rt
    show (Parens xpr) = "(" ++ show xpr ++ ")"

-- Top level binary ops should have no parentheses, nested ones should
showOpArg :: Expr -> String
showOpArg bop@(Binop {}) = "(" ++ show bop ++ ")"
showOpArg x = show x

maybeRead :: (Read a) => String -> Maybe a
maybeRead x
    | null res = Nothing
    | otherwise = Just $ fst . head $ res
    where res = reads x

readMaybeUpper :: (Read a) => String -> Maybe a
readMaybeUpper = maybeRead . map toUpper

readInstr :: String -> Instruction
readInstr = read . map toUpper

readReg :: String -> Register
readReg = read . map toUpper

readCond :: String -> Condition
readCond c = read $ "Cond" ++ map toUpper c

readCondMaybe :: String -> Maybe Condition
readCondMaybe c = maybeRead $ "Cond" ++ map toUpper c

regIs8Bit :: Register -> Bool
regIs8Bit r = r <= R

regIs16Bit :: Register -> Bool
regIs16Bit = not . regIs8Bit
