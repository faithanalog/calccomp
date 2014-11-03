1 CPU_SPEED
VAR X
VAR Y
VAR VX
VAR VY
VAR CURBUFF

0 X !
0 Y !
0 CURBUFF !

DI
1 CPU_SPEED
CLEARSCREEN
GAMELOOP
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

    0 DUP VX ! VY !

    IF (RIGHT KEY)
         X @ Y @ 22 + 0x5C5C 4 2 FILLRECT (Clear tail)
         4 VX +!
    THEN
    IF (LEFT KEY)
         X @ 92 + Y @ 21 + 0x5C5C 4 8 FILLRECT (Clear left hoof)
         -4 VX +!
    THEN
    IF (DOWN KEY)
        X @ 40 + Y @ 0x5C5C 39 4 FILLRECT (Clear wing/hair)
        4 VY +!
    THEN
    IF (UP KEY)
        X @ 59 + Y @ 33 + 0x5C5C 10 4 FILLRECT
        -4 VY +!
    THEN

    X @ VX @ +
    0   MAX (Lower clamp)
    224 MIN (Upper clamp)
    DUP X ! (Stack: X + VX)

    Y @ VY @ +
    0   MAX
    201 MIN
    DUP Y ! (Stack: X + VX, Y + VY)
    RDSprite DISP_SPRT_XLIB

    (X @ Y @ 0xFE00 48 19 FILLRECT)


    RECURSE
}

WORD CLEARSCREEN {
    CURBUFF @   (Save offset)
    0 CURBUFF ! (No offset to screen-clear)
    0 0 0x5C5C 320 240 FILLRECT
    CURBUFF !   (Restore offset)
}

WORD HALF_RES {
    0x0400 0x01 SETLCD (Interlacing)
    0x3033 0x07 SETLCD (Enable partial images)
    0      0x80 SETLCD (Partial img 1 disp pos)
    160    0x83 SETLCD (Partial img 2 disp pos)
}


( INPUT: X Y WIDTH HEIGHT )
WORD SET_LCD_WIN {
    ROT DUP DUP           (X W H Y Y Y)
    0x50 SETLCD           (Win top)
    0x20 SETLCD           (Cursor y)
    + 1- 0x51 SETLCD  (Win bottom, STACK: WIDTH X)

    SWAP DUP DUP          (W X X X)
    0x52 SETLCD           (Win left)
    0x21 SETLCD           (Cursor x)
    + 1- 0x53 SETLCD  (Win right)
}

( Input: X Y POINTER )
WORD DISP_SPRT {
    DUP DUP >R (Save pointer)
            @  (WIDTH)
    SWAP 2+ @  (HEIGHT)
    SET_LCD_WIN
    R> 4 + (Advance to image data)
    DISP_SPRT_FILL
}

( Input: X Y POINTER )
WORD DISP_SPRT_XLIB {
    DUP DUP >R (Store ptr on stack)
            @  (WIDTH)
    SWAP 2+ @  (HEIGHT)
    SET_LCD_WIN
    R> 4 +
    DISP_SPRT_XLIB_FILL
}

( INPUT: X Y COLOR WIDTH HEIGHT )
WORD FILLRECT {
    ROT >R                (Color on R stack)
    2SWAP 2OVER           (Save width/height)
    SET_LCD_WIN           (Stack = width/height)
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
( Only handles even heights )
ASMWORD PIXELFILL {
    pop de ;Color
    pop bc ;C = height
    srl c  ;Height /= 2
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
        out (c),d ;do twice for speeds
        out (c),e
        djnz _pixelFillLpY
    pop bc
    dec hl
    ld a,h
    or l
    jp nz, _pixelFillLpX
}

(INPUT: Pointer)
(Display sprite with xLIBC palette)
ASMWORD DISP_SPRT_XLIB_FILL {
    ld a,22h    ;Switch to GRAM
    out (10h),a
    out (10h),a

    pop hl      ;Data pointer
    ;Size in bytes, Big Endian
    ld d,(hl) \ inc hl ;MSB
    ld e,(hl) \ inc hl ;LSB
    ld b,e

    ;If B != 0, we need an extra iteration
    dec de
    inc d

_dispxlibLp:
    _dispxlibLpInner:
        ld a,(hl) \ inc hl
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
    ld e,(hl) \ inc hl ;LSB
    ld b,e

    ;If B != 0, we need an extra iteration
    dec de
    inc d

_dispSprtLp:
    otir
    dec d
    jp nz,_dispSprtLp
}


(Sprites)
INCBIN RDSprite "RDSprite.sprt"
