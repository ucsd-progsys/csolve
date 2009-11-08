CFLAGS =  -O -I$(INCLUDEDIR)
LIBDIR=../../lib
BINDIR=../../bin
INCLUDEDIR=../../include
SRC = alloc_par.c alloc_par2.c init_alloc.c
 
liballoc.a : $(SRC:c=o) 
	ar cr liballoc.a $(SRC:c=o) 
	ranlib liballoc.a


install:	liballoc.a
	-cd $(LIBDIR); rm -f liballoc.a
	cp liballoc.a $(LIBDIR)/liballoc.a



space:
	-rm -f *.o *.po *.pgo 



c.o.:	cc -c $<



