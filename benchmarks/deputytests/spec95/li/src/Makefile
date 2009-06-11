###############################################################################
# SPECtools95 
#   Version 0.2
#   This makefile should NOT be changed other than to customize for a 
#   particular benchmark.  Users may overide variables by placing them
#   in the Makefile.spec file.  Or by using the "runspec" program to 
#   build the Makefile.spec file for them.

###############################################################################
#	BENCHMARK	-- standard definitions for this benchmark

NUMBER    = 130
NAME      = li
EXEBASE   = li
OFILES    = xlbfun xlcont xldbug xldmem xleval xlfio xlftab xlglob xlinit \
		xlio xlisp xljump xllist xlmath xlobj xlprin xlread xlstr \
		xlsubr xlsym xlsys os
LIBS      = -lm
LD        = $(CC) $(CFLAGS)


###############################################################################
#	DEFINES		-- standard definitions for SPEC
#                          these may be overriden in Makefile.spec
BENCHMARK = $(NUMBER).$(NAME)
EXT       = 
OBJS      = $(addsuffix $(OBJ),$(OFILES))
CC        = gcc
CFLAGS    = $(EXTRA_CFLAGS) $(FDO_FLAGS) $(OPTIMIZE)
#CFLAGS    = -pg $(EXTRA_CFLAGS) $(FDO_FLAGS) $(OPTIMIZE)
FC        = f77
FFLAGS    = $(EXTRA_FFLAGS) $(FDO_FLAGS) $(OPTIMIZE)
LDFLAGS   = $(EXTRA_LDFLAGS) $(FDO_LDFLAGS)
# sm: changed -O to -O2
OPTIMIZE  = $(EXTRA_OPTIMIZE) -O2
OBJOPT    = -c -o $@
OBJ       = .o
SPECSHELL = /bin/sh
ECHO      = echo
BINDIR    = 
#../exe$(TUNE)
EXENAME   = li.exe 
BINARY    = $(EXENAME)

ifeq ($(FDO),pass1)
        FDO_FLAGS=$(PASS1) $(PASS1_CFLAGS)
	FDO_LDFLAGS=$(PASS1_LDFLAGS)
endif
ifeq ($(FDO),pass2)
        FDO_FLAGS=$(PASS2) $(PASS2_CFLAGS)
	FDO_LDFLAGS=$(PASS2_LDFLAGS)
endif


###############################################################################
#	VENDOR		-- where we include any vendor specific information
ifneq ($(NOINC),yes)
	include Makefile.spec
endif


###############################################################################
#	TARGETS		-- Rules to build benchmark executables
error: 
	@$(ECHO) "This makefile is only used for building binaries now"
	@$(ECHO) "to run the SPEC suite please use 'runspec'"
	@$(ECHO)

build: $(EXTRA_TARGETS) $(BINARY)

.SUFFIXES: $(OBJ)
.c$(OBJ):
	$(CC) $(OBJOPT) $(CFLAGS) $<

# template build target, should work for simple builds
$(BINARY): $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $(OBJS) $(LIBS) $(EXTRA_LIBS)

# to clean things up
.PHONY: clean clobber
clean:
ifeq ($(OBJ),.c)
	rm -rf core *.err *.out
else
	rm -rf $(OBJS) core *.err *.out
endif
	rm -rf trial*all.c *allinfer.c *allbox.c *.i *.origi *_ppp.c *.exe *.cil.*

clobber: clean
	rm -rf $(BINARY)

test:
	./testit ./$(BINARY)