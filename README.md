#C compiler targeting TI calcs

Edit Main.hs to change the target file (by default it reads 'test.c').

Output is STDOUT, so just pipe that wherever. It outputs code that brass should
be able to assemble. For best results, use with DCS7 SDK, because that's what I
test with.

Everything in the Asm/ folder doesn't do anything yet, I'll be messing with
that later. the stuff in the C/ folder is functional