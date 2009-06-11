PROG=anagram.exe
LOCAL_CFLAGS=
LOCAL_LIBS=

OBJS = anagram.o

include ../Makefile.ptrdist

# sm: added back in the run with the original data, so we
# can tell if our stuff breaks it
test:
# A quick check that it works
	./anagram.exe words < input.orig.in > output
	$(DIFF) input.OUT output >/dev/null
# Timing on a larger test
	$(SIM) ./anagram.exe words < input.in >/dev/null
	rm output

