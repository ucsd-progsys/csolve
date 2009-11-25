# use gmake
# use sun or dec as TARGET

ifdef WINDIR
EXE=.exe
else
EXE=
endif

TARGET	:= sun

EXEC	:= og-$(TARGET)
BINDIR	:= bin
INCDIR	:= include

SRCDIR	:= ../shared

FLAGS	:= -DLOOPING -Doldpair_ -O3 $(EXTRACFLAGS)

SRC	:= Time.c NewPairs.c NewAddPol.c NewRed.c \
	   NewUtil.c og.c 

LIB	:= -Llib -lmultpol-$(TARGET) -lcmump-$(TARGET) $(EXTRALIBS)

# - ----- no need to change the following

INCSTR	:= ${addprefix -I, ${INCDIR}}
LIBSTR	:= ${addprefix -L, ${LIBDIR}}
OBJ	:= ${addprefix ${BINDIR}/, ${addsuffix .o, ${basename ${SRC}}}}

build: ${EXEC}$(EXE)
${EXEC}$(EXE)	:  ${BINDIR} ${OBJ} makelib
	   ${CC} -g -o $@ ${OBJ} ${LIBSTR} ${LIB}

makelib:
	(cd cmu/src/cmump;make EXTRACFLAGS="$(EXTRACFLAGS)")
	-ranlib lib/libcmump-sun.a
	(cd cmu/src/multpol;make EXTRACFLAGS="$(EXTRACFLAGS)")
	-ranlib lib/libmultpol-sun.a

${BINDIR}:
	   mkdir ${BINDIR}

${BINDIR}/%.o: %.c
	   ${CC} ${FLAGS} ${INCSTR} -o $@ -c $*.c

${BINDIR}/%.o: ${SRCDIR}/%.c
	   ${CC} ${FLAGS} ${INCSTR} -o $@ -c ${SRCDIR}/$*.c

clean	: cleantarget
	bash mclean

cleantarget:
	-rm ${EXEC}$(EXE)

tags:
	etags *.c include/*.h cmu/src/cmump/*.c cmu/src/multpol/*.c

malloc:

bwgc:

