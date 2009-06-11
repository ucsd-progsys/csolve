###############################################################################
# SPECtools95 
#   Version 0.2
#   This makefile should NOT be changed other than to customize for a 
#   particular benchmark.  Users may overide variables by placing them
#   in the Makefile.spec file.  Or by using the "runspec" program to 
#   build the Makefile.spec file for them.

###############################################################################
#	BENCHMARK	-- standard definitions for this benchmark

NUMBER    = 099
NAME      = go
EXEBASE   = go
OFILES    = g2 g22 g23 g25 g26 g27a g27b g28 g29 g2eye g2jlib2 g2jos g2list g2reas g2s2 g2s3 g2shp
LIBS      = 
LD        = $(CC) $(CFLAGS)


###############################################################################
#	DEFINES		-- standard definitions for SPEC
#                          these may be overriden in Makefile.spec
BENCHMARK = $(NUMBER).$(NAME)
EXT       = .exe
OBJS      = $(addsuffix $(OBJ),$(OFILES))
CC        = cc
#CFLAGS    = $(EXTRA_CFLAGS) $(FDO_FLAGS) $(OPTIMIZE)
CFLAGS    = -g $(EXTRA_CFLAGS) $(FDO_FLAGS) $(OPTIMIZE)
FC        = f77
FFLAGS    = $(EXTRA_FFLAGS) $(FDO_FLAGS) $(OPTIMIZE)
LDFLAGS   = $(EXTRA_LDFLAGS) $(FDO_LDFLAGS)
#OPTIMIZE  = $(EXTRA_OPTIMIZE) -O
OPTIMIZE  = $(EXTRA_OPTIMIZE) -O2
#OPTIMIZE  = $(EXTRA_OPTIMIZE)
OBJOPT    = -c -o $@
OBJ       = .o
SPECSHELL = /bin/sh
ECHO      = echo
BINDIR    = ../exe$(TUNE)
BINARY    = go.exe

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
	rm -rf $(OBJS) core *.err *.out *.i *.cil.c
endif

clobber: clean
	rm -rf $(BINARY)

.PHONY: test
test:
	./testit ./$(BINARY)