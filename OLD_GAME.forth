1 CPU_SPEED
VAR X
VAR Y
0 X !
0 Y !

DI
1 CPU_SPEED
HALF_RES
CLEARSCREEN
0 CURBUFF !
GAMELOOP
FULL_RES
0 CPU_SPEED
EI

( CHALLENGES AND WHATNOT

CLOUD GEN.
Needs to not be entirely random
progressively get smaller to a min value
Cursor needs to inc vertically, store images column by column
    why? So they can be split in half
    Scrolling screen, at the edge split sprite down the middle and draw at end and start of screen

How to do hit detection?
    - Create 1 byte per pixel hitmask, check entire sprite?
    - Could check only the front, or edges

Displaying sprite:
    - Transparency: slower, no OTIR, maybe use xlibc palette to try and regain some speed
    - Split into multiple sprites [multiple window sets, no big deal though]
        - For Rainbow dash:
            * Main body
            * Wing
            * Head
            * Right hoof

Optimize: window set
    - Windows really only need to be set on 1 axis.
    - Use the ORIG flag thingy to reduce work down to 2 boundaries and 1 cursor setting
)

WORD GAMELOOP {
    GETCSC
    15 = IF
        RETURN
    THEN

    GETARROWS (STACK: UP DOWN LEFT RIGHT)

    IF (RIGHT KEY)
         X @ Y @ 0x64DC 2 19 FILLRECT
         2 X +!
    THEN
    IF (LEFT KEY)
         X @ 46 + Y @ 0x64DC 2 19 FILLRECT
         -2 X +!
    THEN
    IF (DOWN KEY)
        X @ Y @ 0x64DC 48 2 FILLRECT
        2 Y +!
    THEN
    IF (UP KEY)
        X @ Y @ 17 + 0x64DC 48 2 FILLRECT
        -2 Y +!
    THEN

    (X @ Y @ 0xFE00 48 19 FILLRECT)
    X @ Y @ RDSprite DISP_SPRT

    RECURSE
}

WORD CLEARSCREEN {
    CURBUFF @   (Save offset)
    0 CURBUFF ! (No offset to screen-clear)
    0 0 0x64DC 320 120 FILLRECT
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
    DUP DUP >R (Save pointer)
            @  (WIDTH)
    SWAP 2+ @  (HEIGHT)
    SET_LCD_WIN
    2DROP      (Dont need width/height anymore)
    R> 4 + (Advance to image data)
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
    ;Size in bytes, Big Endian
    ld d,(hl) \ inc hl ;MSB
    ld b,(hl) \ inc hl ;LSB

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


(Sprites)
INCBIN RDSprite "RDSprite.sprt"
