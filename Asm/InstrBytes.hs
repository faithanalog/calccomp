module Asm.InstrBytes where
import Asm.Expr hiding (Expr, Cond, Reg8, Reg16, Reg16Index, RegIndir, RegIndex, AddrIndir, Num)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Builder
import Data.Monoid ((<>))

data Expr = Cond Condition
          | Reg8 Register
          | Reg16 Register
          | Reg16Index Register
          | RegIndir Register
          | RegIndex Register Int
          | AddrIndir Int
          | Num Int
          deriving (Eq)

-- Combines Constant c with bits for register r shifted s bits left
regBits :: Int -> Register -> Int -> Int
regBits c r s = fromIntegral $ c + (bits * (2 ^ s))
    where bits = if regIs8Bit r then case r of
                       A -> 7
                       B -> 0
                       C -> 1
                       D -> 2
                       E -> 3
                       H -> 4
                       L -> 5
                       _ -> 6
                   else case r of
                       BC -> 0
                       DE -> 1
                       HL -> 2
                       _ -> 3

condBits :: Int -> Condition -> Builder
condBits c cond = w8 $ fromIntegral (c + fromEnum cond * 8)

w8 :: Int -> Builder
w8 = word8 . fromIntegral

w16 :: Int -> Builder
w16 = word16LE . fromIntegral

w8l :: [Int] -> Builder
w8l x = foldl1 (<>) (map w8 x)

w16l :: [Int] -> Builder
w16l x = foldl1 (<>) (map w16 x)

regIndex :: Register -> Builder
regIndex IX = w8 0xDD
regIndex IY = w8 0xFD

instrBytes :: Instruction -> [Expr] -> Builder
-- Data movement ops
instrBytes EX (Reg16 DE:Reg16 HL:_)        = w8 0xEB
instrBytes EX (Reg16 AF:Reg16 AF':_)       = w8 0x08
instrBytes EX (RegIndir SP:Reg16 HL:_)     = w8 0xE3
instrBytes EX (RegIndir SP:Reg16Index r:_) = regIndex r <> w8 0xEB

instrBytes EXX _ = w8 0xD9

instrBytes LD (Reg8 A:RegIndir BC:_)           = w8 0x0A
instrBytes LD (Reg8 A:RegIndir DE:_)           = w8 0x1A
instrBytes LD (Reg8 A:AddrIndir addr:_)        = w8 0x3A <> w16 addr
instrBytes LD (RegIndir BC:Reg8 A:_)           = w8 0x02
instrBytes LD (RegIndir DE:Reg8 A:_)           = w8 0x12
instrBytes LD (AddrIndir addr:Reg8 A:_)        = w8 0x32 <> w16 addr
instrBytes LD (Reg8 A:Reg8 I:_)                = w8l [0xED, 0x57]
instrBytes LD (Reg8 A:Reg8 R:_)                = w8l [0xED, 0x5F]
instrBytes LD (Reg8 I:Reg8 A:_)                = w8l [0xED, 0x47]
instrBytes LD (Reg8 R:Reg8 A:_)                = w8l [0xED, 0x4F]
instrBytes LD (Reg16 SP:Reg16 HL:_)            = w8 0xF9
instrBytes LD (Reg16 SP:Reg16Index r:_)        = regIndex r <> w8 0xF9
instrBytes LD (Reg16 l:Num r:_)                = w8 (regBits 1 l 4) <> w16 r
instrBytes LD (Reg16Index l:Num r:_)           = regIndex l <> w8 21 <> w16 r
instrBytes LD (Reg16 HL:AddrIndir addr:_)      = w8 0x2A <> w16 addr
instrBytes LD (Reg16 l:AddrIndir addr:_)       = w8l [0xED, regBits 0x4B l 4] <> w16 addr
instrBytes LD (Reg16Index l:r@(AddrIndir{}):_) = regIndex l <> instrBytes LD [Reg16 HL, r]
instrBytes LD (AddrIndir addr:Reg16 HL:_)      = w8 0x22 <> w16 addr
instrBytes LD (AddrIndir addr:Reg16 r:_)       = w8l [0xED, regBits 0x43 r 4] <> w16 addr
instrBytes LD (l@(AddrIndir{}):Reg16Index r:_) = regIndex r <> instrBytes LD [l, Reg16 HL]
instrBytes LD (Reg8 l:Reg8 r:_)                = w8 $ regBits 0x40 l 3 + regBits 0 r 0
instrBytes LD (Reg8 l:Num r:_)                 = w8l [regBits 6 l 3, r]
instrBytes LD (Reg8 l:RegIndex r o:_)          = regIndex r <> w8l [regBits 6 l 3, o]
instrBytes LD (RegIndex l o:Reg8 r:_)          = regIndex l <> w8l [regBits 0x70 l 0, o]
instrBytes LD (RegIndex l o:Num r:_)           = regIndex l <> w8l [0x36, o, r]

instrBytes LDD _  = w8l [0xED, 0xA8]
instrBytes LDDR _ = w8l [0xED, 0xB8]
instrBytes LDI _  = w8l [0xED, 0xA0]
instrBytes LDIR _ = w8l [0xED, 0xB0]
instrBytes POP (Reg16 r:_)      = w8 $ regBits 0xC1 r 4
instrBytes POP (Reg16Index r:_) = regIndex r <> instrBytes POP [Reg16 HL]

instrBytes PUSH (Reg16 r:_)      = w8 $ regBits 0xC5 r 4
instrBytes PUSH (Reg16Index r:_) = regIndex r <> instrBytes PUSH [Reg16 HL]

-- Arithmetic ops
instrBytes ADC (Reg8 A:Reg8 r:_)       = w8 $ regBits 0x88 r 0
instrBytes ADC (Reg8 A:Num r:_)        = w8l [0xCE, r]
instrBytes ADC (Reg8 A:RegIndex r o:_) = regIndex r <> w8l [regBits 0x88 HL' 0, o]
instrBytes ADC (Reg8 HL:Reg16 r:_)     = w8l [0xED, regBits 0x4A r 4]

instrBytes ADD (Reg8 A:Reg8 r:_)         = w8 $ regBits 0x80 r 0
instrBytes ADD (Reg8 A:Num r:_)          = w8l [0xC6, r]
instrBytes ADD (Reg8 A:RegIndex r o:_)   = regIndex r <> w8l [regBits 0x80 HL' 0, o]
instrBytes ADD (Reg16 HL:Reg16 r:_)      = w8 $ regBits 0x09 r 4
instrBytes ADD (Reg16Index l:Reg16 BC:_) = regIndex l <> w8 (regBits 0x09 BC 4)
instrBytes ADD (Reg16Index l:Reg16 DE:_) = regIndex l <> w8 (regBits 0x09 DE 4)
instrBytes ADD (Reg16Index l:Reg16 SP:_) = regIndex l <> w8 (regBits 0x09 SP 4)
instrBytes ADD (Reg16Index IX:Reg16Index IX:_) = regIndex IX <> w8 (regBits 0x09 HL 4)
instrBytes ADD (Reg16Index IY:Reg16Index IY:_) = regIndex IY <> w8 (regBits 0x09 HL 4)

instrBytes CP (Reg8 r:_) = w8 $ regBits 0xB8 r 0
instrBytes CP (Num r:_)  = w8l [0xFE, r]
instrBytes CP (RegIndex r o:_) = regIndex r <> instrBytes CP [Reg8 HL'] <> w8 o

instrBytes CPD _  = w8l [0xED, 0xA9]
instrBytes CPDR _ = w8l [0xED, 0xB9]
instrBytes CPI _  = w8l [0xED, 0xA1]
instrBytes CPIR _ = w8l [0xED, 0xB1]
instrBytes CPL _  = w8 0x2F
instrBytes DAA _  = w8 0x27

instrBytes DEC (Reg8 r:_) = w8 $ regBits 0x05 r 3
instrBytes DEC (RegIndex r o:_) = regIndex r <> instrBytes DEC [Reg8 HL'] <> w8 o
instrBytes DEC (Reg16 r:_) = w8 $ regBits 0x0B r 4
instrBytes DEC (Reg16Index r:_) = regIndex r <> instrBytes DEC [Reg16 HL]

instrBytes INC (Reg8 r:_) = w8 $ regBits 0x04 r 3
instrBytes INC (RegIndex r o:_) = regIndex r <> instrBytes INC [Reg8 HL'] <> w8 o
instrBytes INC (Reg16 r:_) = w8 $ regBits 0x03 r 4
instrBytes INC (Reg16Index r:_) = regIndex r <> instrBytes INC [Reg8 HL]

instrBytes NEG _ = w8l [0xED, 0x44]

instrBytes SBC (Reg8 A:Reg8 r:_) = w8 $ regBits 0x98 r 0
instrBytes SBC (Reg8 A:Num r:_) = w8l [0xDE, r]
instrBytes SBC (Reg8 A:RegIndex r o:_) = regIndex r <> instrBytes SBC [Reg8 HL'] <> w8 o
instrBytes SBC (Reg16 HL:Reg16 r:_) = w8l [0xED, regBits 0x42 r 4]

instrBytes SUB (Reg8 r:_) = w8 $ regBits 0x90 r 0
instrBytes SUB (Num r:_) = w8l [0xD6, r]
instrBytes SUB (RegIndex r o:_) = regIndex r <> instrBytes SUB [Reg8 HL'] <> w8 o

-- Bit ops
instrBytes AND (Reg8 r:_) = w8 $ regBits 0xA0 r 0
instrBytes AND (Num r:_) = w8l [0xE6, r]
instrBytes AND (RegIndex r o:_) = regIndex r <> instrBytes AND [Reg8 HL'] <> w8 o

instrBytes BIT (Num l:Reg8 r:_) = w8l [0xCB, regBits (0x40 + l * 8) r 0]
instrBytes BIT (Num l:RegIndex r o:_) = regIndex r <> w8l [0xCB, o, regBits (0x40 + l * 8) HL' 0]

instrBytes CCF _ = w8 0x3F

instrBytes OR (Reg8 r:_) = w8 $ regBits 0xB0 r 0
instrBytes OR (Num r:_) = w8l [0xF6, r]
instrBytes OR (RegIndex r o:_) = regIndex r <> instrBytes OR [Reg8 HL'] <> w8 o

instrBytes RES (Num l:Reg8 r:_) = w8l [0xCB, regBits (0x80 + l * 8) r 0]
instrBytes RES (Num l:RegIndex r o:_) = regIndex r <> w8l [0xCB, o, regBits (0x80 + l * 8) HL' 0]

instrBytes SCF _ = w8 0x37

instrBytes SET (Num l:Reg8 r:_) = w8l [0xCB, regBits (0xC0 + l * 8) r 0]
instrBytes SET (Num l:RegIndex r o:_) = regIndex r <> w8l [0xCB, o, regBits (0xC0 + l * 8) HL' 0]

instrBytes XOR (Reg8 r:_) = w8 $ regBits 0xA8 r 0
instrBytes XOR (Num r:_) = w8l [0xEE, r]
instrBytes XOR (RegIndex r o:_) = regIndex r <> instrBytes OR [Reg8 HL'] <> w8 o

-- Shift/Rotate ops

-- SO MUCH CODE DUPE PLZ FIX
instrBytes RL (Reg8 r:_) = w8l [0xCB, regBits 0x10 r 0]
instrBytes RL (RegIndex r o:_) = regIndex r <> w8l [0xCB, o, regBits 0x10 HL' 0]

instrBytes RLC (Reg8 r:_) = w8l [0xCB, regBits 0x00 r 0]
instrBytes RLC (RegIndex r o:_) = regIndex r <> w8l [0xCB, o, regBits 0x00 HL' 0]

instrBytes RR (Reg8 r:_) = w8l [0xCB, regBits 0x18 r 0]
instrBytes RR (RegIndex r o:_) = regIndex r <> w8l [0xCB, o, regBits 0x18 HL' 0]

instrBytes RRC (Reg8 r:_) = w8l [0xCB, regBits 0x08 r 0]
instrBytes RRC (RegIndex r o:_) = regIndex r <> w8l [0xCB, o, regBits 0x08 HL' 0]

instrBytes SLA (Reg8 r:_) = w8l [0xCB, regBits 0x20 r 0]
instrBytes SLA (RegIndex r o:_) = regIndex r <> w8l [0xCB, o, regBits 0x20 HL' 0]

instrBytes SRA (Reg8 r:_) = w8l [0xCB, regBits 0x28 r 0]
instrBytes SRA (RegIndex r o:_) = regIndex r <> w8l [0xCB, o, regBits 0x28 HL' 0]

instrBytes SRL (Reg8 r:_) = w8l [0xCB, regBits 0x38 r 0]
instrBytes SRL (RegIndex r o:_) = regIndex r <> w8l [0xCB, o, regBits 0x38 HL' 0]

instrBytes RLA _  = w8 0x17
instrBytes RLCA _ = w8 0x07
instrBytes RLD _  = w8l [0xED, 0x6F]
instrBytes RRA _  = w8 0x1F
instrBytes RRCA _ = w8 0x0F
instrBytes RRD _  = w8l [0xED, 0x67]

-- Control ops
instrBytes CALL (Num addr:_) = w8 0xCD <> w16 addr
instrBytes CALL (Cond c:Num addr:_) = condBits 0xC4 c <> w16 addr

instrBytes DJNZ (Num o:_) = w8l [0x10, o]

instrBytes JP (Num addr:_) = w8 0xC3 <> w16 addr
instrBytes JP (Cond c:Num addr:_) = condBits 0xC2 c <> w16 addr

instrBytes JR (Num o:_) = w8l [0x18, o]
instrBytes JR (Cond c:Num o:_) = condBits 0x20 c <> w8 o

instrBytes NOP _ = w8 0x00

instrBytes RET (Cond c:_) = condBits 0xC0 c
instrBytes RET _ = w8 0xC9

instrBytes RETI _ = w8l [0xED, 0x4D]
instrBytes RETN _ = w8l [0xED, 0x45]
instrBytes RST (Num addr:_) = w8 $ 0xC7 + 8 * case addr of
    0x00 -> 0
    0x08 -> 1
    0x10 -> 2
    0x18 -> 3
    0x20 -> 4
    0x28 -> 5
    0x30 -> 6
    0x38 -> 7
    _ -> error $ "Cannot RST to addr " ++ show addr

-- Hardware ops
instrBytes DI _ = w8 0xF3
instrBytes EI _ = w8 0xFb
instrBytes HALT _ = w8 0x76
instrBytes IM (Num m:_) = w8 0xED <> w8 (case m of
    0 -> 0x46
    1 -> 0x56
    2 -> 0x5E
    _ -> error $ "Invalid interrupt mode " ++ show m)

instrBytes IN (Reg8 A:AddrIndir port:_) = w8l [0xDB, port]
instrBytes IN (Reg8 l:RegIndir C:_) = w8l [0xEB, regBits 0x40 l 3]

instrBytes OUT (Reg8 A:AddrIndir port:_) = w8l [0xD3, port]
instrBytes OUT (Reg8 l:RegIndir C:_) = w8l [0xEB, regBits 0x41 l 3]

instrBytes IND _  = w8l [0xED, 0xAA]
instrBytes INDR _ = w8l [0xED, 0xBA]
instrBytes INI _  = w8l [0xED, 0xA2]
instrBytes INIR _ = w8l [0xED, 0xB2]

instrBytes OUTD _ = w8l [0xED, 0xAB]
instrBytes OTDR _ = w8l [0xED, 0xBB]
instrBytes OUTI _ = w8l [0xED, 0xA3]
instrBytes OTIR _ = w8l [0xED, 0xB3]
