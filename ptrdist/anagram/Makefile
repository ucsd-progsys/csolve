PROG=anagram
LOCAL_CFLAGS=
LOCAL_LIBS=

OBJS = anagram.o

include ../Makefile

test:
	$(SIM) ./anagram words < input.in > FOO
	$(DIFF) FOO input.OUT
	rm -f FOO

