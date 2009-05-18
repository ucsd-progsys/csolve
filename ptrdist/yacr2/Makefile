PROG=yacr2
LOCAL_CFLAGS=-DTODD
LOCAL_LIBS=

OBJS = main.o option.o channel.o vcg.o hcg.o assign.o maze.o

include ../Makefile

test:
	$(SIM) ./yacr2 input1.in > FOO
	$(DIFF) FOO input1.OUT
	rm -f FOO
	$(SIM) ./yacr2 input2.in > FOO
	$(DIFF) FOO input2.OUT
	rm -f FOO

