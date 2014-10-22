CLEARSCREEN "HODOR" "KermM" "HODOR" PUTS PUTS PUTS GETKEY

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

ASMWORD PUTS {
    pop hl
    b_call(_PutS)
    b_call(_NewLine)
}


