##############################################################################
# MAKEFILE: compile tree-code program.                                       #
#                                                                            #
# Copyright (c) 1993, Joshua E. Barnes, Honolulu, HI.                        #
# It's free because it's yours.                                              #
##############################################################################

# Compiler flags for development on SGI; not used on other systems.
# single version: CFLAGS = -fullwarn -woff 269,302 -prototypes -O -float
# double version: CFLAGS = -DDOUBLEPREC -fullwarn -woff 269,302 -prototypes -O

# Standard compiler flags.  Double precision is the default because not
# all available systems seem to have single-precision square root.
#sm: orig: CFLAGS = -DDOUBLEPREC -O
CFLAGS = -DDOUBLEPREC -O3
# CFLAGS = -DDOUBLEPREC -g
LDFLAGS = -O3

# Build a treecode.

# code.o io.o load.o grav.o util.o getparam.o
code.exe: code.o io.o load.o grav.o util.o getparam.o
	$(CC) $(LDFLAGS) -o code.exe $^ -lm $(EXTRA_LIBS)

code.o: code.c defs.h code.h stdinc.h
	$(CC) $(CFLAGS) -c code.c

io.o: io.c defs.h stdinc.h
	$(CC) $(CFLAGS) -c io.c

load.o: load.c defs.h stdinc.h
	$(CC) $(CFLAGS) -c load.c

grav.o: grav.c defs.h stdinc.h
	$(CC) $(CFLAGS) -c grav.c

#  Utility routines etc.

util.o: util.c stdinc.h
	$(CC) $(CFLAGS) -c util.c


getparam.o: getparam.c stdinc.h
	$(CC) $(CFLAGS) -c getparam.c

clean: 
	rm -f *.o *.lib *.exe *.obj *~ *cil.c *_comb.c *infer.c *cured.c *.i *cured*optim.c