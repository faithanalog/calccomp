1 CPU_SPEED
VAR X
VAR Y
0 X !
0 Y !

HALF_RES
0 0 0x0000 320 120 FILLRECT
GAMELOOP
FULL_RES

WORD GAMELOOP {
    GETCSC
    DUP 15 = IF
        DROP
        RETURN
    THEN

    X @ Y @ (Old X/Y)   (KEY X Y)
    ROT (X Y KEY)
    DUP DUP DUP (KEY, KEY, KEY, KEY)

    2 = SWAP 3 = - << << X +! (X += (isRight - isLeft) * 4)
    4 = SWAP 1 = - << << Y +! (Y += (isDown - isUp) * 4)
    X @ Y @ 0xFE00 16 16 FILLRECT

    FLIP_BUFFER (Disp new square)

    0x0000 16 16 FILLRECT (Erase old square)
    RECURSE
}

WORD HALF_RES {
    0x0400 0x01 SETLCD (Interlacing)
    0x3033 0x07 SETLCD (Enable partial images)
    0      0x80 SETLCD (Partial img 1 disp pos)
    160    0x83 SETLCD (Partial img 2 disp pos)
    0 DISP_BUFFER
    160 CURBUFF !
}

WORD FULL_RES {
    0x0000 0x01 SETLCD (No interlacing)
    0x0133 0x07 SETLCD (Disable partial images)
}

WORD DISP_BUFFER {
    DUP DUP
    0x81 SETLCD (Img 1 start)
    0x84 SETLCD (Img 2 start)
    159 + DUP
    0x82 SETLCD (Img 1 end)
    0x85 SETLCD (Img 2 end)
}

VAR CURBUFF
WORD FLIP_BUFFER {
    CURBUFF @ IF
        160 0
    ELSE
        0 160
    THEN
    CURBUFF !
    DISP_BUFFER
}

( INPUT: X Y COLOR WIDTH HEIGHT )
( Configured for half-res mode )
WORD FILLRECT {
    ROT >R                (Color on R stack)
    << (Height *= 2)
    2SWAP                 (STACK: WIDTH HEIGHT X Y)
    << (Y *= 2)
    SWAP (Y X)
    CURBUFF @ +
    SWAP (X Y)

    DUP DUP               (W H X Y Y Y)
    0x50 SETLCD           (Win top)
    0x20 SETLCD           (Cursor y)
    2 PICK + 1- 0x51 SETLCD  (Win bottom, STACK: WIDTH HEIGHT X)

    DUP DUP               (W H X X X)
    0x52 SETLCD           (Win left)
    0x21 SETLCD           (Cursor x)
    2 PICK + 1- 0x53 SETLCD  (Win right,  STACK: WIDTH HEIGHT)
    >> (Height /= 2, PIXELFILL also is configured for half-res mode. This lets us do half as many DJNZs)

    R>                    (Color off R stack)
    PIXELFILL
}

ASMWORD CPU_SPEED {
    pop hl
    ld a,l
    out (20h),a
}

ASMWORD SLEEP {
    pop hl
WasteTime:
    djnz WasteTime
    dec hl
    ld a,h
    or l
    jr nz,WasteTime
}

ASMWORD GETCSC {
    b_call(_GetCSC)
    ld h,0
    ld l,a
    push hl
}

( Generate a random number from 0 - 31 )
ASMWORD RAND {
    .var 2, randData
    ld hl,(randData)
    ld a,r
    ld d,a
    ld e,(hl)
    add hl,de
    add a,l
    xor h
    and 31
    ld (randData),hl
    ld h,0
    ld l,a
    push hl
}


( Sets an LCD register )
( INPUT: VAL REG )
ASMWORD SETLCD {
    pop hl
    ld a,l
    out (10h),a
    out (10h),a
    pop hl
    ld a,h
    out (11h),a
    ld a,l
    out (11h),a
}

( Fills WIDTH * HEIGHT pixels with COLOR )
( INPUT: WIDTH HEIGHT COLOR )
ASMWORD PIXELFILL {
    pop de ;Color
    pop bc ;C = height
    pop hl ;HL = width

    ld a,22h
    out (10h),a
    out (10h),a

    ld b,c ;height
    ld c,11h ;Output reg

_pixelFillLpX:
    push bc ;Save height
    _pixelFillLpY:
        out (c),d
        out (c),e
        out (c),d ;Half-res, do twice
        out (c),e
        djnz _pixelFillLpY
    pop bc
    dec hl
    ld a,h
    or l
    jr nz, _pixelFillLpX
}