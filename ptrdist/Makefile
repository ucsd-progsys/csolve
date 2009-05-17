#
# master makefile, editing the following variables will affect all benchmarks
#

# C compiler
CC=gcc

# execution prefix, e.g., simulator executable
SIM=time

# difference tester
DIFF=diff -s

# global optimization level
OPT_LEVEL = -g -O3


#
# END of user-modifiable variables.
#

CFLAGS = $(OPT_LEVEL) $(LOCAL_CFLAGS)

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LOCAL_LIBS)

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	/bin/rm -f $(PROG) $(OBJS) *~ core *.qpt *.QCounts *.bak FOO

