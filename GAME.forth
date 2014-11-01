1 CPU_SPEED
VAR X
VAR Y
0 X !
0 Y !

DI
1 CPU_SPEED
HALF_RES
CLEARSCREEN
GAMELOOP_
FULL_RES
0 CPU_SPEED
EI

WORD GAMELOOP {
    GETCSC
    15 = IF
        RETURN
    THEN

    X @ Y @ (Old X/Y)

    Y @ 4 +
    DUP 104 > IF
        DROP 0
    THEN
    DUP Y !
    X @ SWAP (Stack: OldX, OldY, X, Y)
    0xF800 16 16 FILLRECT
    FLIP_BUFFER

    (Stack: OldX, OldY)
    0x0000 16 16 FILLRECT

    150 SLEEP
    RECURSE
}

WORD GAMELOOP_ {
    GETCSC
    15 = IF
        RETURN
    THEN

    X @ Y @ (Old X/Y)

    GETARROWS
    -ROT (STACK: DOWN UP LEFT RIGHT)
         - << << X +! (X += (isRight - isLeft) * 4)
    SWAP - << << Y +! (Y += (isDown - isUp) * 4)


    X @ Y @ 0xFE00 16 16 FILLRECT

    FLIP_BUFFER (Disp new square)

    0x0000 16 16 FILLRECT (Erase old square)
    RECURSE
}

WORD CLEARSCREEN {
    CURBUFF @   (Save offset)
    0 CURBUFF ! (No offset to screen-clear)
    0 0 0x0000 320 120 FILLRECT
    CURBUFF !   (Restore offset)
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

( INPUT: X Y WIDTH HEIGHT )
( RETURN: WIDTH HEIGHT )
WORD SET_LCD_WIN {
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
    >> (Height /= 2)
}

( Input: X Y POINTER )
WORD DISP_SPRT {
    DUP DUP >R (Store ptr on stack)
            @  (WIDTH)
    SWAP 2+ @  (HEIGHT)
    SET_LCD_WIN
    2DROP      (Dont need width/height anymore)
    R>
    DISP_SPRT_FILL
}

( Input: X Y POINTER )
WORD DISP_SPRT_XLIB {
    DUP DUP >R (Store ptr on stack)
            @  (WIDTH)
    SWAP 2+ @  (HEIGHT)
    SET_LCD_WIN
    2DROP      (Dont need width/height anymore)
    R>
    DISP_SPRT_XLIB_FILL
}

( INPUT: X Y COLOR WIDTH HEIGHT )
( Configured for half-res mode )
WORD FILLRECT {
    ROT >R                (Color on R stack)
    SET_LCD_WIN
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
    b_call(_kbdScan)
    b_call(_GetCSC)
    ld h,0
    ld l,a
    push hl
}

( Returns: DOWN LEFT RIGHT UP )
ASMWORD GETARROWS {
    ld a,FFh
    out (01h),a
    ld a,FEh ;Arrow group
    out (01h),a
	or a
	push af
	sbc hl,hl
	pop af
	in a,(01h)
	ld b,4
_arrowLp:
    rla
    ld hl,0
    sbc hl,hl
    push hl
    djnz _arrowLp
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
    jp nz, _pixelFillLpX
}

(INPUT: Pointer)
(Display sprite with xLIBC palette, adjusted for half-res)
ASMWORD DISP_SPRT_XLIB_FILL {
    ld a,22h    ;Switch to GRAM
    out (10h),a
    out (10h),a

    pop hl      ;Data pointer
    ;Size in bytes
    ld b,(hl) \ inc hl ;LSB
    ld d,(hl) \ inc hl ;MSB

    ld a,b
    or a
    jr z,$+3
    inc d       ;If B is non 0, we need an extra iteration of the inner loop

_dispxlibLp:
    _dispxlibLpInner:
        ld a,(hl) \ inc hl
        out (11h),a
        out (11h),a
        out (11h),a
        out (11h),a
        djnz _dispxlibLpInner
    dec d
    jp nz,_dispxlibLp
}

(INPUT: Pointer)
(Not adjusted for half-res)
ASMWORD DISP_SPRT_FILL {
    ld c,11h    ;Out port
    ld a,22h    ;Switch to GRAM
    out (10h),a
    out (10h),a

    pop hl      ;Data pointer
    ;Size in bytes
    ld b,(hl) \ inc hl ;LSB
    ld d,(hl) \ inc hl ;MSB

    ;If B != 0, we need an extra iteration
    ld a,b
    or a
    jr z,$+3
    inc d

_dispSprtLp:
    otir
    dec d
    jp nz,_dispSprtLp
}