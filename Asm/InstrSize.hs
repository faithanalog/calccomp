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
instrSize EX (RegIndir SP:r:_) = regArgSize r
instrSize EX _ = 1

instrSize EXX _ = 1

-- Oh god why
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

instrSize LDD  _ = 2
instrSize LDDR _ = 2
instrSize LDI  _ = 2
instrSize LDIR _ = 2
instrSize POP (Reg16 _:_) = 1
instrSize POP (Reg16Index _:_) = 2
instrSize PUSH xs = instrSize POP xs

-- Arithmetic ops
instrSize ADC (Reg8 A:r:_) = arg8Size r
instrSize ADC (Reg16 HL:_) = 2

instrSize ADD (Reg8 A:r:_) = arg8Size r
instrSize ADD (l:_) = regArgSize l

instrSize CP (r:_) = arg8Size r
instrSize CPD _  = 2
instrSize CPDR _ = 2
instrSize CPI _  = 2
instrSize CPIR _ = 2
instrSize CPL _  = 1
instrSize DAA _  = 1
instrSize DEC (r:_) = regArgSize r
instrSize INC x = instrSize DEC x
instrSize NEG _  = 2

instrSize SBC xs = instrSize ADC xs
instrSize SUB (r:_) = arg8Size r

-- Bit ops
instrSize AND (r:_) = arg8Size r
instrSize OR  xs = instrSize AND xs
instrSize XOR xs = instrSize AND xs

instrSize BIT (_:RegIndex{}:_) = 4
instrSize BIT _ = 2
instrSize RES args   = instrSize BIT args
instrSize SET args   = instrSize BIT args

instrSize CCF _ = 1
instrSize SCF _ = 1

-- Shift/Rotate ops
instrSize RL (r:_) = regArgSize r + 1
instrSize RLC args = instrSize RL args
instrSize RR  args = instrSize RL args
instrSize RRC args = instrSize RL args
instrSize SLA args = instrSize RL args
instrSize SRA args = instrSize RL args
instrSize SRL args = instrSize RL args

instrSize RLA _  = 1
instrSize RLCA _ = 1
instrSize RRA _  = 1
instrSize RRCA _ = 1
instrSize RLD _  = 2
instrSize RRD _  = 2

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

instrSize _ _ = 0
