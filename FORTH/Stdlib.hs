{-# LANGUAGE QuasiQuotes #-}
module FORTH.Stdlib (stdlibAsm
                    , Dependency(..)
                    , Rtn(..)
                    ) where

import FORTH.Parser
import Asm.QQ
import qualified Asm.Expr as A
import qualified Asm.Parser as A

type Dependency = [A.Expr]
data Rtn = Rtn {code :: [A.Expr], inline :: Bool, depends :: [Dependency]}

-- Routine
rtn x = Just Rtn {code = x, inline = False, depends = []}

-- Inlined routine
rtni x = Just Rtn {code = x, inline = True, depends = []}

withDeps xs (Just (Rtn c i _)) = Just Rtn {code = c, inline = i, depends = xs}

stdlibAsm = wasm

-- Standard FORTH words
wasm :: String -> Maybe Rtn

wasm "TRUE" = rtni [asm|ld hl, -1 \ push hl|]
wasm "FALSE" = rtni [asm|ld hl, 0 \ push hl|]

wasm "DUP" = rtni [asm|pop hl \ push hl \ push hl|]

-- DUP if top != 0, else leave it alone
wasm "?DUP" = rtni [asm|
    pop hl
    push hl
    ld a,h
    or l
    jr nz,$+3
    pop hl
    push hl
|]

wasm "DROP" = rtni [asm|pop hl|]

wasm "SWAP" = rtni [asm|pop hl \ ex (sp),hl \ push hl|]

-- x1 x2 -- x1 x2 x1
wasm "OVER" = rtni [asm|
    pop hl
    pop de
    push de
    push hl
    push de
|]

-- x1 x2 -- x2
wasm "NIP" = rtni [asm|pop hl \ pop de \ push hl|]

-- x1 x2 -- x2 x1 x2
wasm "TUCK" = rtni [asm|
    pop hl
    pop de
    push hl
    push de
    push hl
|]

-- Rotate left
-- x1 x2 x3 -- x2 x3 x1
wasm "ROT" = rtni [asm|
    pop de ;n3
    pop hl ;n2
    ex (sp), hl ;n1 -> n2
    push de
    push hl
|]

-- Rotate right
-- x1 x2 x3 -- x3 x1 x2
wasm "-ROT" = rtni [asm|
    pop hl ;n3
    pop de ;n2
    ex (sp), hl ;n1 -> n3
    push hl
    push de
|]

-- xu .. x0 u -- xu .. x0 xu
wasm "PICK" = rtni [asm|
    pop hl
    add hl,hl ;Stack item is 2 bytes
    add hl,sp ;HL has address
    ld e,(hl)
    inc hl
    ld d,(hl)
    push de
|]

wasm "2DUP" = rtni [asm|
    pop hl
    pop de
    push de
    push hl
    push de
    push hl
|]

wasm "2DROP" = rtni [asm|pop hl \ pop hl|]

-- x1 x2 x3 x4 -- x3 x4 x1 x2
wasm "2SWAP" = rtni [asm|
    pop bc ;x4
    pop hl ;x3
    pop de ;x2
    ex (sp),hl ;hl = x1
    push bc
    push hl
    push de
|]

-- x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
wasm "2OVER" = rtn [asm|
    pop de ;Save return address
    ld hl,7 ;Position on MSB of x1
    add hl,sp
    ld b,(hl)
    dec hl
    ld c,(hl)
    dec hl
    push bc
    ld b,(hl)
    dec hl
    ld c,(hl)
    push bc
    ex de,hl
    jp (hl)
|]

wasm ">R" = rtni [asm|
    pop hl
    dec ix
    ld (ix),h
    dec ix
    ld (ix),l
|]

wasm "R@" = rtni [asm|
    ld l,(ix)
    ld h,(ix + 1)
    push hl
|]

wasm "R>" = rtni [asm|
    ld l,(ix)
    inc ix
    ld h,(ix)
    inc ix
    push hl
|]

wasm "2>R" = rtn [asm|
    pop hl
    pop bc
    pop de
    ld (ix - 0),d
    ld (ix - 1),e
    ld (ix - 2),b
    ld (ix - 3),c
    ld de,-4
    add ix,de
    jp (hl)
|]

wasm "2R@" = rtn [asm|
    pop hl
    ld e,(ix + 2)
    ld d,(ix + 3)
    push de
    ld e,(ix + 0)
    ld d,(ix + 1)
    push de
    jp (hl)
|]

wasm "2R>" = rtn [asm|
    pop hl
    ld c,(ix)
    ld b,(ix + 1)
    ld e,(ix + 2)
    ld d,(ix + 3)
    push de
    push bc
    ld de, 4
    add ix, de
    jp (hl)
|]

wasm "+" = rtni [asm|
    pop hl
    pop de
    add hl, de
    push hl
|]

wasm "1+" = rtni [asm|
    pop hl
    inc hl
    push hl
|]

wasm "2+" = rtni [asm|
    pop hl
    inc hl
    inc hl
    push hl
|]

wasm "-" = rtni [asm|
    pop de
    pop hl
    or a ;Clear carry
    sbc hl, de
    push hl
|]

wasm "1-" = rtni [asm|
    pop hl
    dec hl
    push hl
|]

wasm "2-" = rtni [asm|
    pop hl
    dec hl
    dec hl
    push hl
|]

wasm "*" = withDeps [mult16] $ rtni [asm|
    pop hl
    pop bc
    call MULT_16
    push hl
|]

wasm "/" = withDeps [div16] $ rtni [asm|
    pop de
    pop hl
    call DIV_16
    push bc
|]

wasm "MOD" = withDeps [div16] $ rtni [asm|
    pop de
    pop hl
    call DIV_16
    push hl
|]

wasm "/MOD" = withDeps [div16] $ rtni [asm|
    ;push remainder, then quotient
    pop de
    pop hl
    call DIV_16
    push hl
    push bc
|]

wasm "NEGATE" = rtni [asm|
    pop hl
    xor a
    sub l
    ld l,a
    sbc a,a
    sub h
    ld h,a
    push hl
|]

wasm "ABS" = rtn [asm|
    pop hl
    pop de
    bit 7,d ;Check sign bit
    jr z,CTRL_ABS
    xor a
    sub e
    ld e,a
    sbc a,a
    sub d
    ld d,a
    CTRL_ABS:
    push de
    jp (hl)
|]

wasm "MIN" = rtn [asm|
    pop de ;Return addr
    pop bc
    pop hl
    or a
    sbc hl,bc
    add hl,bc
    ;hl >= bc, push bc. Else push hl
    jr nc,$+5
    push hl
    ex de,hl
    jp (hl)
    ;Jump end
    push bc
    ex de,hl
    jp (hl)
|]

wasm "MAX" = rtn [asm|
    pop de ;Return addr
    pop bc
    pop hl
    or a
    sbc hl,bc
    add hl,bc
    ;hl >= bc, push bc. Else push hl
    jr nc,$+5
    push bc
    ex de,hl
    jp (hl)
    ;Jump end
    push hl
    ex de,hl
    jp (hl)
|]

-- X Y
-- HL = Y
-- DE = X
-- HL > DE = Y > X
wasm "<" = withDeps [hlGTDEsigned] $ rtni [asm|
    pop hl
    pop de
    call HL_GT_DE_SIGNED
    push hl
|]

wasm ">" = withDeps [hlGTDEsigned] $ rtni [asm|
    pop de
    pop hl
    call HL_GT_DE_SIGNED
    push hl
|]

wasm ">=" = withDeps [hlGTEQDEsigned] $ rtni [asm|
    pop de
    pop hl
    call HL_GTEQ_DE_SIGNED
    push hl
|]

wasm "<=" = withDeps [hlGTEQDEsigned] $ rtni [asm|
    pop hl
    pop de
    call HL_GTEQ_DE_SIGNED
    push hl
|]

wasm "=" = rtni [asm|
    pop hl
    pop de
    xor a
    sbc hl,de
    ld h,a
    ld l,a
    jr nz,$+3
    dec hl
    push hl
|]

wasm "U<" = rtni [asm|
    pop de
    pop hl
    or a
    sbc hl,de
    sbc hl,hl
    push hl
|]

wasm "U>" = rtni [asm|
    pop hl
    pop de
    or a
    sbc hl,de
    sbc hl,hl
    push hl
|]

wasm "!=" = rtni [asm|
    pop hl
    pop de
    xor a
    sbc hl,de
    jr z,$+5
    ld hl,-1
    push hl
|]

wasm "<>" = wasm "!="

wasm "0=" = rtni [asm|
    pop hl
    ld a,l
    or h
    jr z,$+5
    ld hl,1
    dec hl
    push hl
|]

wasm "0!=" = rtni [asm|
    pop hl
    ld a,l
    or h
    add a,255
    sbc hl,hl
    push hl
|]

wasm "0<>" = wasm "0!="

wasm "AND" = rtni [asm|
    pop hl
    pop de
    ld a,h
    and d
    ld h,a
    ld a,l
    and e
    ld l,a
    push hl
|]

wasm "OR" = rtni [asm|
    pop hl
    pop de
    ld a,h
    or d
    ld h,a
    ld a,l
    or e
    ld l,a
    push hl
|]

wasm "XOR" = rtni [asm|
    pop hl
    pop de
    ld a,h
    xor d
    ld h,a
    ld a,l
    xor e
    ld l,a
    push hl
|]

wasm "INVERT" = rtni [asm|
    pop hl
    ld a,h
    cpl
    ld h,a
    ld a,l
    cpl
    ld l,a
    push hl
|]

wasm "LSHIFT" = rtni [asm|
    pop hl
    add hl,hl
    push hl
|]

wasm "RSHIFT" = rtni [asm|
    pop hl
    sra h
    rr l
    push hl
|]

wasm "URSHIFT" = rtni [asm|
    pop hl
    srl h
    rr l
    push hl
|]

wasm "<<" = wasm "LSHIFT"
wasm ">>" = wasm "RSHIFT"
wasm ">>>" = wasm "URSHIFT"

wasm "BITROTR" = rtni [asm|
    pop hl
    ld a, l
    rra
    rr h
    rr l
    push hl
|]

wasm "BITROTL" = rtni [asm|
    pop hl
    ld a, l
    rla
    rl h
    rl l
    push hl
|]

wasm "@" = rtni [asm|
    pop hl
    ld e,(hl)
    inc hl
    ld d,(hl)
    push de
|]

wasm "!" = rtni [asm|
    pop hl
    pop de
    ld (hl),e
    inc hl
    ld (hl),d
|]

-- Stack: n, addr
-- Adds n to the value at addr and stores it back in addr
wasm "+!" = rtn [asm|
    pop de ;Return addr
    pop hl ;addr

    ;Load value at addr
    ld c,(hl)
    inc hl
    ld b,(hl)

    ;Store value addr, load other value
    ex (sp),hl ;store addr, load other val
    add hl, bc

    ;Put return address back on stack, get value addr
    ex de,hl ;de = result, hl = return addr
    ex (sp),hl ;store return addr, hl = store addr

    ;Store result value
    ld (hl),d
    dec hl
    ld (hl),e
    ;Now just ret, return address is on stack
    ret
|]

wasm "c@" = rtni [asm|
    pop hl
    ld e,(hl)
    ld d,0
    push de
|]

wasm "c!" = rtni [asm|
    pop hl
    pop de
    ld (hl),e
|]

wasm "CELLS" = rtni [asm|
    pop hl
    add hl,hl
    push hl
|]

-- nop
wasm "CHARS" = rtni []

-- Return from the current word
wasm "RETURN" = rtni [asm|
    ld l,(ix)
    inc ix
    ld h,(ix)
    inc ix
    jp (hl)
|]

wasm "PUTNUM" = withDeps [dispHL] $ rtn [asm|
    pop hl
    ex (sp),hl
    bit 7,h
    ld a,' '
    jr z,DISP_NUM_B0
    xor a
    sub l
    ld l,a
    sbc a,a
    sub h
    ld h,a
    ld a,$1A ;LNeg
DISP_NUM_B0:
    b_call(_PutC)
    call DISP_HL
    b_call(_NewLine)
    ret
|]

wasm "." = wasm "PUTNUM"

wasm _ = Nothing

-- ASM dependencies of FORTH words
hlGTDEsigned = [asm|
HL_GT_DE_SIGNED:
    ld a,e
    sub l
    ld a,d
    sbc a,h
    rra
    xor d
    xor h
    rla
    sbc hl,hl
    ret
|]

hlGTEQDEsigned = [asm|
HL_GTEQ_DE_SIGNED:
    ;Cheks if HL >= DE signed. Returns result in HL. -1 if true, 0 if false
    xor a
    ld b,h
    sbc hl,de
    ld h,a
    rra
    xor b
    xor d
    rlca
    and 1
    ld l,a
    dec hl
    ret
|]

dispHL = [asm|
DISP_HL:
    ;Optimized display of 0
    ld a,h
    or l
    jr nz,DISP_HL_BR0
    ld a, '0'
    b_call(_PutC)
    ret
DISP_HL_BR0:
    ld d,0 ;Whether or not we've seen a number other than 0
    ld bc,-10000
    call DISP_HL_BR1
    ld bc,-1000
    call DISP_HL_BR1
    ld bc,-100
    call DISP_HL_BR1
    ld c,-10
    call DISP_HL_BR1
    ld c,-1
DISP_HL_BR1:
    ld a,'0'-1
DISP_HL_BR2:
    inc a
    add hl,bc
    jr c,DISP_HL_BR2
    sbc hl,bc
    cp '0'
    jr z,DISP_HL_BR3
    ld d,FFh
DISP_HL_BR3:
    and d
    ret z
    b_call(_PutC)
    ret
|]

mult16 = [asm|
MULT_16:
    ;16*16 unsigned by z80bits (DE:HL + DE * BC)
    ;Params are passed in, one in hl, the other in bc
    ;This lets us inline popping to hl and pushing result (since that can get optimized)
    ;but we save space by doing everything in here
    ex de, hl
    ld hl,0
    sla e
    rl d
    jr nc,$+4
    ld h,b
    ld l,c
    ld a, 15 ;loop countter
    MULT_16_LP:
        add hl,hl
        rl e
        rl d
        jr nc,$+6
        add hl,bc
        jr nc,$+3
        inc de
        dec a
        jr nz,MULT_16_LP
    ret
|]

div16 = [asm|
DIV_16:
    ;16/16 signed (modified from unsigned) by z80bits (A:C / DE, quotient = BC, remainder = HL)
    ;Params are passed in as HL and DE (so it does HL / DE)
    ld a,%10000000
    and h
    xor d
    rla ;Will carry if signs are different
    push af
;
    ;A:C = Absolute val of HL
    bit 7,h ;Check sign bit
    jr z,DIV_16_BR0
    xor a
    sub l
    ld l,a
    sbc a,a
    sub h
    ld h,a
DIV_16_BR0:
    ld a,h
    ld c,l
;
    ;DE = Absolute val of DE
    bit 7,d ;Check sign bit
    jr z,DIV_16_BR1
    xor a
    sub e
    ld e,a
    sbc a,a
    sub d
    ld d,a
DIV_16_BR1:
;
    ld hl,0
    ld b, 16
DIV_16_LP:
        rl c
        rla
        adc hl,hl
        sbc hl,de
        jr nc,$+3
        add hl,de
        djnz DIV_16_LP
    rl c
    rla
;
    ld b,a
    inc bc ;BC is negative now
    pop af ;Carry will be set if result is negative
    jr c, DIV_16_BR2
    ;Make BC positive again
    xor a
    sub c
    ld c,a
    sbc a,a
    sub b
    ld b,a
    ret
DIV_16_BR2:
    ;Invert remainder
    xor a
    sub l
    ld l,a
    sbc a,a
    sub h
    ld h,a
    ret
|]
