( VX and VY are velocity )
VAR VX
VAR VY

VAR HEADX
VAR HEADY

VAR HEAD_OFF
VAR TAIL_OFF

VAR BSIZE


1 CPU_SPEED (15mhz)
0 0 0x0000 320 240 FILLRECT
0 0 0xFFFF 228 228 FILLRECT
RUNGAME
0 CPU_SPEED (6mhz)

( Runs the game loop multiple times, return val determines if it runs again )
WORD RUNGAME {
    ( Initialize variables )
    4 HEADX !    0 HEADY !
    1 VX !       0 VY !
    4 HEAD_OFF ! 0 TAIL_OFF !
    5 BSIZE !
    2 2 0x0000 224 224 FILLRECT
    CLEARBOARD
    5 INITSNAKE
    GENFOOD
    GAMELOOP IF
        RECURSE
    THEN
}

( Actual loop for in the game )
( Return TRUE if game should restart, FALSE if it should exit )
WORD GAMELOOP {
    (Remember, TRUE = -1, FALSE = 0)
    GETCSC
    DUP 15 = IF
        DROP
        FALSE RETURN
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


    (Food/collision test)
    HEADX @ HEADY @
    CELLADDR c@ (Tile at new head loc)
    DUP 1 = IF
        DROP
        TRUE RETURN (Restart game)
    THEN
    2 = IF
        GENFOOD
    ELSE
        (Erase/move tail)
        TAIL_OFF @ DUP DUP
        BODY_X + c@ SWAP
        BODY_Y + c@
        2DUP 0 SETCELL
        0x0000 FILLCELL
        1+ 1023 AND
        TAIL_OFF !
    THEN

    (Draw head)
    HEADX @ HEADY @
    2DUP 1 SETCELL
    0x07E0 FILLCELL

    245 SLEEP
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

    DUP IFRECURSE (If size != 0, recurse)
    DROP          (Drop size)
}

(Generates a new food tile)
WORD GENFOOD {
    RAND RAND (X and Y)
    2DUP CELLADDR c@ IF
        2DROP
        RECURSE          (Jump back to start until we find a blank tile)
    THEN
    (2DUP FOODY ! FOODX ! (Save food position))
    2DUP 2 SETCELL       (Set food position on board)
    0xF800 FILLCELL      (Draw food)
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

( Fills board with zeroes )
( Input: Addr Val Amount )
WORD CLEARBOARD {
    BOARD >R (addr on R stack)
    1024     (Amount of bytes to fill)
    RECURSEPOINT
    2-       (We fill 2 at a time)
    DUP R@ + 0 SWAP !
    ?DUP IF  (If offset > 0, recurse)
        RECURSE
    THEN
    R> DROP (Drop BOARD)
}

( Fills a board cell )
( Input: X Y COLOR )
WORD FILLCELL {
    ROT
    7 * 2+  (X = X * 7 + 2)
    ROT
    7 * 2+  (Y = Y * 7 + 2)
    ROT 7 7 (Stack: X Y COLOR 7 7)
    FILLRECT
}

( INPUT: X Y COLOR WIDTH HEIGHT )
WORD FILLRECT {
    ROT >R                (Color on R stack)
    2SWAP                 (STACK: WIDTH HEIGHT X Y)

    DUP DUP               (W H X Y Y Y)
    0x50 SETLCD           (Win top)
    0x20 SETLCD           (Cursor y)
    2 PICK + 1- 0x51 SETLCD  (Win bottom, STACK: WIDTH HEIGHT X)

    DUP DUP               (W H X X X)
    0x52 SETLCD           (Win left)
    0x21 SETLCD           (Cursor x)
    2 PICK + 1- 0x53 SETLCD  (Win right,  STACK: WIDTH HEIGHT)

    R>                    (Color off R stack)
    PIXELFILL
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
        djnz _pixelFillLpY
    pop bc
    dec hl
    ld a,h
    or l
    jr nz, _pixelFillLpX
}

