# UrLISP

## About This Program

This name doesn't mean "You are LISP".
It means "primitive LISP".
UrLISP is an interpreter of so-called "pure LISP".

It supports "super brackets" '[', ']' to group and close brackets together.

Please refer to "INIT.TXT" for usage.
By the way, "INIT.TXT" is loaded as a sample program file at startup.

## How to compile

Execute Makefile with GCC or Clang.

If you want to compile with Turbo-C on MS-DOS, do the following.

Change the lines 10 and 11 in LISP.H to the following:

    #define CELLS_SIZE 0x3800
    #define STACK_SIZE 0x2000

Add the following line near the beginning of MAIN.C:

    #include <dos.h>
    extern unsigned _stklen = 60000;

## Starting UrLISP

In the console, run the file we just compiled and generated, "lisp" (rename it to your liking).

## Exiting UrLISP

Please press Ctrl+C.

Or

    % (quit)
