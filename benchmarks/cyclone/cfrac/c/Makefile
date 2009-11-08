#
# An implementation of the Continued Fraction Algorithm CFRAC
#
# Dave Barrett
#

PDIR 	= .
# removed because I don't want to recompile every time I change header file
#INCF 	= pfactor.h

ifdef DEBUG
CFLAGS  = -g $(EXTRACFLAGS)
else
CFLAGS  = -O3 -DTIMING $(EXTRACFLAGS)
endif

BINSRC  = cfrac.c
HDR	= primes.h seive.h pfactor.h

PSRC	= pops.c pconst.c pio.c \
	  pabs.c pneg.c pcmp.c podd.c phalf.c \
	  padd.c psub.c pmul.c pdivmod.c psqrt.c ppowmod.c \
	  atop.c ptoa.c itop.c utop.c ptou.c errorp.c \
	  pfloat.c pidiv.c pimod.c picmp.c

FSRC	= primes.c pcfrac.c pgcd.c

FOBJS	= $(PSRC:.c=.o)
POBJS	= $(FSRC:.c=.o)
OBJS	= $(BINSRC:.c=.o) $(PSRC:.c=.o) $(FSRC:.c=.o)
build: cfrac

cfrac:	$(OBJS) malloc.o
	$(CC) -o cfrac malloc.o $(OBJS) -lm $(EXTRALIBS)

malloc.o: malloc.c
	$(CC) $(CFLAGS) -c malloc.c
malloc:

bwgc:

SRC	= Makefile $(HDR) $(BINSRC)

pgcd.o:		$(INC)
pcfrac.o:	$(INCF) $(INC) primes.h
primes.o:	primes.h

clean:
		rm -f core
		rm -f $(OBJS) cfrac

cleantarget:
	rm cfrac

# 
# producer only clean
#
pclean:       
	rm -f $(OBJS)

