CFLAGS = -I./include -O3 $(EXTRACFLAGS)

CC = ../../src/cc.sh

OBJ		=	Madd.o Mdiv.o Mgcd.o Mout.o Mmult.o\
			MExpo.o Mutil.o Mfloat.o Mfatal.o Mstrtoul.o

test: test.o $(OBJ)
	$(CC) $< -o test

clean:
	rm *.o *.ssa.c *.vmap *.fq *.autospec liquid.c

lib: $(OBJ)
	ar cruv ../../../lib/libcmump-sun.a $(OBJ)

test.o: test.c
Madd.o: Madd.c
Mdiv.o: Mdiv.c
Mgcd.o: Mgcd.c
Mout.o: Mout.c
Mpow.o: Mpow.c
Msqrt.o: Msqrt.c
Mmult.o: Mmult.c
MExpo.o: MExpo.c
Mutil.o: Mutil.c
Mfloat.o: Mfloat.c
Mstrtoul.o: Mstrtoul.c
Mfatal.o: Mfatal.c
Mmod.o: Mmod.c
