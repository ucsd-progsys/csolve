PROG=ks.exe
LOCAL_CFLAGS=
LOCAL_LIBS=

OBJS = KS-1.o KS-2.o

include ../Makefile.ptrdist

test:
	$(SIM) ./ks.exe KL-4.in > FOO
	$(DIFF) FOO KL-4.OUT
	rm -f FOO

