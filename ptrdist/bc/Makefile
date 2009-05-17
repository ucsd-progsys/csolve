PROG=bc
LOCAL_CFLAGS=
LOCAL_LIBS=

OBJS = scan.o util.o main.o number.o storage.o load.o execute.o bc.o global.o

include ../Makefile

test:
	$(SIM) ./bc < primes.b > FOO
	$(DIFF) FOO primes.OUT
	rm -f FOO

