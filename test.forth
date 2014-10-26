CLEARSCREEN 0 TO5 . GETKEY

WORD TO5 {
    1 +
    DUP 5 <
    IF
        RECURSE
    THEN
}

ASMWORD CLEARSCREEN {
    b_call(_ClrLCDFull)
    xor a
    ld (curRow), a
    ld (curCol), a
}

ASMWORD GETKEY {
    b_call(_GetKey)
    ld h,0
    ld l,a
    push hl
}
