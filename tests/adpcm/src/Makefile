#
# Makefile for adpcm and test programs
# -------------------

#####################################################################
# Added by C. Lee.
#
# On Sun machines, gcc need to be informed whether the machines have
# integer multipliers by specifying this option: -mv8.
#
# Also, on Sun machines, in order to use the SPIXTOOLS and SHADE,
# uncomment this option: -static.
#
ifdef NODEPUTY
CC = ../../../deputy/bin/deputy
else
CC = gcc
endif

# LFLAGS = -mv8
# LFLAGS = $(LFLAGS) -static
#
# End of addition
######################################################################
CFLAGS = -O3

# May need this on a sun:
# CFLAGS=-O -I/usr/demo/SOUND

ALLSRC=README adpcm.2 Makefile adpcm.h libst.h adpcm.c timing.c rawcaudio.c \
	rawdaudio.c suncaudio.c sgicaudio.c sgidaudio.c hello.adpcm.uu

all: adpcm.o rawcaudio.exe rawdaudio.exe timing.exe


adpcm.o: adpcm.h


rawcaudio.exe: rawcaudio.o adpcm.o
	$(CC) $(CFLAGS) rawcaudio.o adpcm.o -o rawcaudio.exe

rawdaudio.exe: rawdaudio.o adpcm.o
	$(CC) $(CFLAGS) rawdaudio.o adpcm.o -o rawdaudio.exe

timing.exe: timing.o adpcm.o
	$(CC) $(CFLAGS) timing.o adpcm.o -o timing.exe

# Added by C. Lee
clean:
	rm -f *.o *.exe
