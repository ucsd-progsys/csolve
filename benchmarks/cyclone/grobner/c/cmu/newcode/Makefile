
# enable hidden dependency checking
.KEEP_STATE:

INCDIR=		~soumen/grob/src/cmump ~soumen/grob/src/gbas ~soumen/grob/src/mutpol
MAIN=		CC
TARGET=		a.out
CC=		gcc
CFLAGS=		-g

SRC=		

OBJ_DIR=	~soumen/grob/obj

OBJS=		$(SRC:%.c=%.o)

$(TARGET):	$(OBJS)
		$(CC) $(CFLAGS) $(OBJS)

%.o:		%.c
		$(CC) $(CFLAGS) -I$(INCDIR) $< -c -o $@


lex.yy.c:	lexpol.l
		lex lexpol.l

y.tab.c:	translate.y
		yacc translate.y


