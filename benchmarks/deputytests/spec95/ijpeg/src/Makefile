###############################################################################
# SPECtools95 
#   Version 0.2
#   This makefile should NOT be changed other than to customize for a 
#   particular benchmark.  Users may overide variables by placing them
#   in the Makefile.spec file.  Or by using the "runspec" program to 
#   build the Makefile.spec file for them.

###############################################################################
#	BENCHMARK	-- standard definitions for this benchmark

NUMBER    = 132
NAME      = ijpeg
EXEBASE   = ijpeg
OFILES    = libpbm1 libpbm2 libpbm3 libpbm4 libpbm5 libpgm1 libpgm2 libppm1 libppm2 libppm3 libppm4 libppm5 spec_image spec_jmemdst spec_jmemsrc spec_main rdppm wrppm rdgif wrgif rdtarga wrtarga rdbmp wrbmp jcapi jcparam jdatadst jcmaster jcmarker jcmainct jcprepct jccoefct jccolor jcsample jchuff jcdctmgr jfdctfst jfdctflt jfdctint jdapi jdatasrc jdmaster jdmarker jdmainct jdcoefct jdpostct jddctmgr jidctfst jidctflt jidctint jidctred jdhuff jdsample jdcolor jquant1 jquant2 jdmerge jcomapi jutils jerror jmemmgr jmemnobs 
LIBS      = 
LD        = $(CC) $(CFLAGS)


###############################################################################
#	DEFINES		-- standard definitions for SPEC
#                          these may be overriden in Makefile.spec
BENCHMARK = $(NUMBER).$(NAME)
EXT       = 
OBJS      = $(addsuffix $(OBJ),$(OFILES))
CC        = cc
CFLAGS    = $(EXTRA_CFLAGS) $(FDO_FLAGS) $(OPTIMIZE)
FC        = f77
FFLAGS    = $(EXTRA_FFLAGS) $(FDO_FLAGS) $(OPTIMIZE)
LDFLAGS   = $(EXTRA_LDFLAGS) $(FDO_LDFLAGS)
OPTIMIZE  = $(EXTRA_OPTIMIZE) -O2
OBJOPT    = -c -o $@
OBJ       = .o
SPECSHELL = /bin/sh
ECHO      = echo
BINDIR    = ../exe$(TUNE)
EXENAME   = $(EXEBASE)$(EXT)
# sm: BINARY    = $(BINDIR)/$(EXENAME)
BINARY    = ijpeg.exe

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
	$(CC) -g $(OBJOPT) $(CFLAGS) $<

# template build target, should work for simple builds
# sm: removed $(OPTIMIZE) b/c I want to have non-opt ability
$(BINARY): $(OBJS)
	echo "INPUT (" >linker_script.o
	echo $(OBJS)   >>linker_script.o
	echo ")"       >>linker_script.o
	$(LD) -g $(LDFLAGS) -o $@ --flatten_linker_scripts linker_script.o $(LIBS) $(EXTRA_LIBS)

# to clean things up
.PHONY: clean clobber
clean:
ifeq ($(OBJ),.c)
	rm -rf core *.err *.out
else
	rm -rf $(OBJS) core *.err *.out
endif
	rm -f *.origi *infer.c *cil.c *_comb.c

clobber: clean
	rm -rf $(BINARY)


test: 
	sh -c "time ./ijpeg.exe -image_file ../data/ref/input/vigo.ppm -GO >output"
	@if diff output correct.output ; then \
            echo "Ijpeg got the correct output" ; \
        else \
            echo "Ijpeg got the WRONG output"; exit 1 ; fi