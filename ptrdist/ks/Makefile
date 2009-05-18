PROG=ks
LOCAL_CFLAGS=
LOCAL_LIBS=

OBJS = KS-1.o KS-2.o

include ../Makefile

test:
	$(SIM) ./ks KL-1.in > FOO
	$(DIFF) FOO KL-1.OUT
	rm -f FOO
	$(SIM) ./ks KL-2.in > FOO
	$(DIFF) FOO KL-2.OUT
	rm -f FOO

