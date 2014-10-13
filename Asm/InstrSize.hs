module Asm.InstrSize (instrSize) where
import Asm.Expr
-- Instruction size calculations

-- Size of a register argument
regArgSize :: Expr -> Int
regArgSize r = case r of
    Reg8 _ -> 1
    RegIndex _ _ -> 3
    Reg16 _ -> 1
    Reg16Index _ -> 2

-- Size of an 8 bit arithmetic arg
arg8Size :: Expr -> Int
arg8Size r = case r of
    Literal _ -> 2
    Binop{} -> 2
    _ -> regArgSize r

instrSize :: Instruction -> [Expr] -> Int
-- Data movement ops
instrSize EX (l:r:[]) = case l of
    RegIndir SP -> regArgSize r
    _ -> 1
instrSize EXX _ = 1
instrSize LD (l:r:[]) = case l of
    Reg8 A -> case r of
        RegIndir _ -> 1
        AddrIndir _ -> 3
        _ -> reg8size
    Reg16 HL -> regHL
    Reg16Index _ -> regHL + 1
    Reg16 SP -> regArgSize r

    Reg8 _ -> reg8size
    Reg16 _ -> reg16size
    RegIndex _ _ -> reg8size + 1
    RegIndir _ -> 1

    AddrIndir _ -> case r of
        Reg8 A -> 3
        Reg16 HL -> 3
        Reg16 _ -> 4
        Reg16Index _ -> 4
    where reg8size = arg8Size r
          reg16size = case r of
              Literal _ -> 3
              Binop{} -> 3
              AddrIndir _ -> 4
          regHL = case r of
              AddrIndir _ -> 3
              _ -> reg16size

instrSize LDD _  = 2
instrSize LDDR _ = 2
instrSize LDI _  = 2
instrSize LDIR _ = 2
instrSize POP (r:[]) = case r of
    Reg16 _ -> 1
    Reg16Index _ -> 2
instrSize PUSH xs = instrSize POP xs

-- Arithmetic ops
instrSize ADC (l:r:[]) = case l of
    Reg8 A -> arg8Size r
    Reg16 HL -> 2

instrSize ADD (l:r:[]) = case l of
    Reg8 A -> arg8Size r
    _ -> regArgSize l

instrSize CP (r:[]) = arg8Size r
instrSize CPD _  = 2
instrSize CPDR _ = 2
instrSize CPI _  = 2
instrSize CPIR _ = 2
instrSize CPL _  = 1
instrSize DAA _  = 1
instrSize DEC (r:[]) = regArgSize r
instrSize INC x = instrSize DEC x
instrSize NEG _  = 2
instrSize SBC (l:r:[]) = case l of
    Reg8 A -> arg8Size r
    Reg16 HL -> 2
instrSize SUB (r:[]) = arg8Size r

-- Bit ops
instrSize AND (r:[]) = arg8Size r
instrSize BIT (_:r:[]) = case r of
    RegIndex _ _ -> 4
    _ -> 2
instrSize CCF _ = 1
instrSize OR (r:[])  = arg8Size r
instrSize RES args   = instrSize BIT args
instrSize SCF _  = 1
instrSize SET args   = instrSize BIT args
instrSize XOR (r:[]) = arg8Size r

-- Shift/Rotate ops
instrSize RL (r:[]) = regArgSize r + 1
instrSize RLA _  = 1
instrSize RLC args = instrSize RL args
instrSize RLCA _ = 1
instrSize RLD _  = 2
instrSize RR args  = instrSize RL args
instrSize RRA _  = 1
instrSize RRC args = instrSize RL args
instrSize RRCA _ = 1
instrSize RRD _  = 2
instrSize SLA args = instrSize RL args
instrSize SRA args = instrSize RL args
instrSize SRL args = instrSize RL args

-- Control ops
instrSize CALL _ = 3
instrSize DJNZ _ = 2
instrSize JP _   = 3
instrSize JR _   = 2
instrSize NOP _  = 1
instrSize RET _  = 1
instrSize RETI _ = 2
instrSize RETN _ = 2
instrSize RST _  = 1

-- Hardware ops
instrSize DI _   = 1
instrSize EI _   = 1
instrSize HALT _ = 1
instrSize IM _   = 2
instrSize IN _   = 2
instrSize IND _  = 2
instrSize INI _  = 2
instrSize INIR _ = 2
instrSize OTDR _ = 2
instrSize OTIR _ = 2
instrSize OUT _  = 2
instrSize OUTD _ = 2
instrSize OUTI _ = 2
