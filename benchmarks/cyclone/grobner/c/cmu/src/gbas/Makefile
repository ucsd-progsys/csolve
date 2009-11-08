#CFLAGS =  -O -I$(INCLUDEDIR) -Deager_kills -Dfull_kills
#CFLAGS =  -O -I$(INCLUDEDIR) -Deager_kills -DTIME_REDUCTION -DTIME_REDUCTION_1
#CFLAGS =  -O -I$(INCLUDEDIR) -Deager_kills -DTIME_REDUCTION -DTIME_REDUCTION_2
CFLAGS =  -O -I$(INCLUDEDIR) -Deager_kills -DTIME_REDUCTION -DTIME_REDUCTION_3
#CFLAGS =  -O -I$(INCLUDEDIR) -Deager_kills
LIBDIR=../../lib
BINDIR=../../bin
INCLUDEDIR=../../include
MANDIR=../../man
SRC = MPolsetUpdate.c Simplify.c Pairs.c Gbasis.c Util.c
 
libgbas.a : $(SRC:c=o) Reduction.o
	ar cr libgbas.a $(SRC:c=o) Reduction.o
	ranlib libgbas.a

$(SRC:c=o) : gbas.h

Reduction.o : gbas.h Reduction.c

Main.o : Main.c gbas.h
	cc -c $(CFLAGS) Main.c

gbas_p : libgbas.a Main.o gbas.h
	cc  $(CFLAGS) Main.o libgbas.a $(LIBDIR)/libmultpol.a $(LIBDIR)/libcmump.a -ll -o ../../bin/gbas_p  $(LIBDIR)/liballoc.a -lthreads -lmach

GbasisI.o : Gbasis.c gbas.h
	cc -c $(CFLAGS) -DINTERACTIVE Gbasis.c
	mv Gbasis.o GbasisI.o

ReductionI.o : Reduction.c gbas.h
	cc -c $(CFLAGS) -DINTERACTIVE Reduction.c
	mv Reduction.o ReductionI.o

gbas_i : libgbas.a Main.o GbasisI.o ReductionI.o gbas.h
	cc  $(CFLAGS) Main.o GbasisI.o ReductionI.o libgbas.a $(LIBDIR)/libmultpol.a $(LIBDIR)/libcmump.a -ll -o ../../bin/gbas_i  $(LIBDIR)/liballoc.a -lthreads -lmach


cleantd:	space
	-rm -f libgbas.a 
	
install:	libgbas.a $(BINDIR)/gbas_p 
	-cd $(LIBDIR); rm -f libgbas_p.a
	cp libgbas.a $(LIBDIR)/libgbas_p.a
	-rm -f $(INCLUDEDIR)/gbas_p.h
	cp gbas.h $(INCLUDEDIR)/gbas_p.h


space:
	-rm -f *.o *.po *.pgo 



c.o.:	cc -c $<



