PROG=backprop
LOCAL_CFLAGS=
LOCAL_LIBS=-lm

OBJS = backprop.o

include ../Makefile

test:
	$(SIM) ./backprop > FOO
	$(DIFF) FOO backprop.OUT
	rm -f FOO

