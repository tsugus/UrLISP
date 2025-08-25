lisp:		main.o io.o gc.o eval.o
			gcc -o lisp main.o io.o gc.o eval.o
# Ex. The case of stack size is 0x8000 bytes
# Windows, Linux:
#	gcc -Wl,-stack,0x8000 -o lisp ...
# Mac:
#	gcc -Wl,-stack_size,0x80000 -o lisp ...
#	(stack_size must be multiples of page size 0x4000.)
main.o:		MAIN.c
			gcc -c MAIN.c
io.o:		IO.c
			gcc -c IO.c
gc.o:		GC.c
			gcc -c GC.c
eval.o:		EVAL.c
			gcc -c EVAL.c
clean:;		rm -f *.o
