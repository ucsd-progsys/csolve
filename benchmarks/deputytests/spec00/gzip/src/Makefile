###############################################################################
# SPECtools2000
#   Version 0.3
#   This makefile should NOT be changed other than to customize for a 
#   particular benchmark.  Users may overide variables by placing them
#   in the Makefile.spec file.  Or by using the "runspec" program to 
#   build the Makefile.spec file for them.
###############################################################################
#	BENCHMARK	-- standard definitions for this benchmark

NUMBER    = 164
NAME      = gzip
EXEBASE   = gzip
BENCHLANG = C
SOURCES   = bits.c deflate.c gzip.c getopt.c inflate.c lzw.c \
	    spec.c trees.c unlzh.c unlzw.c unpack.c unzip.c util.c zip.c

# Include standard template for this suite
-include ../../../../Makefile.defaults
-include ../../../Makefile.defaults
-include ../../Makefile.defaults
-include ../Makefile.defaults


nomakefiles:
	@echo "*** Whoop!  I didn't find a Makefile.defaults file."
	@echo "*** Your distribution is probably corrupt."
	@echo

test: 
	sh -c "time ./$(EXEBASE).exe"