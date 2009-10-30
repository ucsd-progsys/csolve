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

NOLIQUID= # "yes"

ifdef NOLIQUID
CC = gcc
else
CC = ../../../src/cc.sh
endif

ifdef NOLIQUID
MERGECC = gcc
else
MERGECC = cilly --merge --keepmerged --noPrintLn
endif

# LFLAGS = -mv8
# LFLAGS = $(LFLAGS) -static
#
# End of addition
######################################################################
CFLAGS =

# May need this on a sun:
# CFLAGS=-O -I/usr/demo/SOUND

ALLSRC=README adpcm.2 Makefile adpcm.h libst.h adpcm.c timing.c rawcaudio.c \
	rawdaudio.c suncaudio.c sgicaudio.c sgidaudio.c hello.adpcm.uu

all: adpcm.o rawcaudio.exe rawdaudio.exe timing.exe

.c.o:
	$(CC) $<

adpcm.o: adpcm.h

rawcaudio.exe: rawcaudio.o adpcm.o
	$(MERGECC) $(CFLAGS) rawcaudio.o adpcm.o -o rawcaudio.exe

rawdaudio.exe: rawdaudio.o adpcm.o
	$(MERGECC) $(CFLAGS) rawdaudio.o adpcm.o -o rawdaudio.exe

timing.exe: timing.o adpcm.o
	$(MERGECC) $(CFLAGS) timing.o adpcm.o -o timing.exe

# Added by C. Lee
clean:
	rm -f *.o *.exe
