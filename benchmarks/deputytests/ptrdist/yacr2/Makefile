PROG=yacr2.exe
LOCAL_CFLAGS=-DTODD
LOCAL_LIBS=

OBJS = main.o option.o channel.o vcg.o hcg.o assign.o maze.o

include ../Makefile.ptrdist

test:
	$(SIM) ./yacr2.exe input2.in > FOO
	$(DIFF) FOO input2.OUT && rm -f FO


