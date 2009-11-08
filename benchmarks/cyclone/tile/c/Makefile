ifdef DEBUG
CFLAGS  = -g  -I.. $(EXTRACFLAGS)
else
CFLAGS  = -O3 -I.. $(EXTRACFLAGS)
endif
OBJS=	tile.o main.o token.o common.o
LEX=	flex
GCOBJS = $(addprefix gc,$(OBJS))
build: tile
gctile: $(GCOBJS)
	$(CC) -o gctile $(GCOBJS) $(EXTRALIBS) -lm -L/usr/sww/lib -L$(HOME)/src/cyclone/gc -lfl $(HOME)/src/cyclone/gc/gc.a

tile: $(OBJS)
	$(CC) -o tile $(OBJS) $(EXTRALIBS) -lm -L/usr/sww/lib -lfl

common.c: mkcommon
	sh ./mkcommon > common.c

token.c: token.l
	flex -otoken.c token.l

gccommon.c: mkcommon
	sh ./mkcommon > gccommon.c

gctoken.c: token.l
	flex -ogctoken.c token.l

gc%.o : gc%.c
	gcc $(CFLAGS) -I$(HOME)/src/cyclone/gc/include -c $^ -o $@

clean:
	rm -f *~ *.o tile common.c token.c

cleantarget:
	rm tile
