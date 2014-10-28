( VX and VY are velocity )
( A60B )
VAR VX
VAR VY

VAR HEADX
VAR HEADY

VAR FOODX
VAR FOODY

VAR HEAD_OFF
VAR TAIL_OFF

VAR GROWTH
VAR BSIZE


CPU_FAST (15mhz)
( Initialize variables )

4 HEADX !
0 HEADY !

1 VX !
0 VY !

4 HEAD_OFF !
0 TAIL_OFF !

0 GROWTH !
5 BSIZE !

CLEARSCREEN
5 INITSNAKE
GENFOOD

GAMELOOP

CPU_NORM (6mhz)

WORD GAMELOOP {
    (Remember, TRUE = -1, FALSE = 0)
    GETCSC
    DUP 15 = IF
        DROP
        RETURN
    THEN
    DUP 1- 4 U< IF  (If key code is from 1-4...)
        DUP DUP DUP (STACK: KEY KEY KEY KEY)
        (Y axis)
        4 = SWAP 1 = -      (-1 if key up, 1 if key down, 0 if both)    (STACK: KEY KEY VELY)
        DUP VY @ AND 0= IF (Same as VX for VY)
            VY !
        ELSE DROP THEN

        (X Axis)
        2 = SWAP 3 = -      (-1 if key left, 1 if key right, 0 if both) (STACK: VELX)
        DUP VX @ AND 0= IF  (If VX & NEWVX == 0, set VX to NEWVX, else DROP it)
            VX !
        ELSE DROP THEN
    ELSE DROP THEN

    (Increment head offset)
    HEAD_OFF @ 1+ 1023 AND
    HEAD_OFF !

    (Move head)
    HEADX @ VX @ + 31 AND
    DUP HEADX !
    HEAD_OFF @ BODY_X + c!

    HEADY @ VY @ + 31 AND
    DUP HEADY !
    HEAD_OFF @ BODY_Y + c!

    (Erase/move tail)
    TAIL_OFF @ DUP DUP
    BODY_X + c@ SWAP
    BODY_Y + c@
    2DUP 0 SETCELL
    0x0000 FILLCELL
    1+ 1023 AND
    TAIL_OFF !

    (Draw head)
    HEADX @ HEADY @
    2DUP 1 SETCELL
    0x07E0 FILLCELL

    225 SLEEP
    RECURSE
}

(
Initializes the snake on LCD/board
Input: Length
)
WORD INITSNAKE {
    1-
    DUP DUP BODY_X + c!        (Set X position in body data)
        DUP BODY_Y + 0 SWAP c! (Set Y position in body data)
    DUP 0 0x07E0 FILLCELL
    DUP 0 1 SETCELL
    DUP 0!= IF
        RECURSE
    THEN
    DROP
}

(Generates a new food tile)
WORD GENFOOD {
    RAND RAND (X and Y)
    2DUP TESTCELL IF
        2DROP
        RECURSE          (Jump back to start until we find a blank tile)
    THEN
    2DUP FOODY ! FOODX ! (Save food position)
    2DUP 2 SETCELL       (Set food position on board)
    0xF800 FILLCELL      (Draw food)
}

( Tests to see if a cell at X Y has a snake body )
WORD TESTCELL {
    CELLADDR c@ 0!=
}

( Input: X Y TYPE )
WORD SETCELL {
    -ROT CELLADDR c!
}



( Gets cell addr for cell X Y )
WORD CELLADDR {
    32 * +      ( Y * 32 + x)
    BOARD +     ( Add board address )
}

ASMWORD BODY_X {
    .var 1024, snakeBodyX
    ld hl,snakeBodyX
    push hl
}

ASMWORD BODY_Y {
    .var 1024, snakeBodyY
    ld hl,snakeBodyY
    push hl
}

ASMWORD BOARD {
    .var 1024, board
    ld hl, board
    push hl
}

ASMWORD CPU_FAST {
    ld a,01h
    out (20h),a
}

ASMWORD CPU_NORM {
    ld a,00h
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

ASMWORD GETKEY {
    b_call(_GetKey)
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

( Below here is scary LCD code )

( Fills the screen with black )
ASMWORD CLEARSCREEN {
    ld b,0
    ld c,11h

    ;Win left
    ld a,52h
    out (10h),a
    out (10h),a
    out (c),b
    out (c),b

    ;Win right
    ld a,53h
    out (10h),a
    out (10h),a
    ld a, 319 >> 8
    out (11h),a
    ld a, 319
    out (11h),a

    ;Win top
    ld a,50h
    out (10h),a
    out (10h),a
    out (c),b
    out (c),b

    ;Win bot
    ld a,51h
    out (10h),a
    out (10h),a
    xor a
    out (11h),a
    ld a, 239
    out (11h),a

    ;Win cursor x
    ld a,21h
    out (10h),a
    out (10h),a
    out (c),b
    out (c),b

    ;Win cursor y
    ld a,20h
    out (10h),a
    out (10h),a
    out (c),b
    out (c),b

    ld a,22h
    out (10h),a
    out (10h),a


    ld b,0
    ld hl,300
    _clearScreenOuterLoop:
        xor a
        _clearScreenLoop:
            out (11h),a
            out (11h),a
            djnz _clearScreenLoop
        dec hl
        ld a,h
        or l
        jr nz,_clearScreenOuterLoop
}

( INPUT: X Y COLOR )
ASMWORD FILLCELL {
    pop de ;Color
    pop hl ;Y
    pop bc ;X
    ld h,c ;H = X, L = Y

    ld a,h
    add a,a
    add a,a
    add a,a
    sub h
    ; add a,2 + 14 ;Offset right by 16
    ld h,a

    ld a,l
    add a,a
    add a,a
    add a,a
    sub l
    ; add a,2 ;Offset by 8 down
    ld l,a

    ld b,0
    ld c,11h

    ;Win left
    ld a,52h
    out (10h),a
    out (10h),a
    out (c),b
    out (c),h

    ;Win cursor x
    ld a,21h
    out (10h),a
    out (10h),a
    out (c),b
    out (c),h

    ;Win right
    ld a,53h
    out (10h),a
    out (10h),a
    out (c),b
    ld a,6
    add a,h
    out (11h),a

    ;Win top
    ld a,50h
    out (10h),a
    out (10h),a
    out (c),b
    out (c),l

    ;Win cursor y
    ld a,20h
    out (10h),a
    out (10h),a
    out (c),b
    out (c),l

    ;Win bot
    ld a,51h
    out (10h),a
    out (10h),a
    out (c),b
    ld a,6
    add a,l
    out (11h),a

    ld a,22h
    out (10h),a
    out (10h),a

    ld a,d ;out with a is faster marginally, use it for the msb of the color
    ld b,49
    _fillSnakePart:
        out (11h),a
        out (c),e
        djnz _fillSnakePart
}
