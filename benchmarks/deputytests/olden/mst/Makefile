# /* For copyright information, see olden_v1.0/COPYRIGHT */

BINARY		= mst.exe
FILES		= main makegraph hash args ssplain

OLDENHOME	= /tmp/olden

ifdef _MSVC
CC    = cl
DEF   = /D
CONLY = /c
OBJOUT = /Fo
EXEOUT = /Fe
OBJ   = .obj

OPTFLAGS	= /Ox
LIBS = 

else

CC		= cilly --merge --stdoutpp
DEF   = -D
CONLY = -c
OBJOUT= -o
EXEOUT= -o

OBJ		= .o

OPTFLAGS	= -g -Wall -O3

LIBS		= -lm
LIBPATH		= 
endif

SRC		= .c
ASM		= .s
EXTRA_CDEFS	= $(DEF)I_TIME $(DEF)I_SYS_TIME $(DEF)ULTRIX
CDEFS		= $(DEF)PLAIN $(DEF)SS_PLAIN
SRCS		= $(addsuffix $(SRC),$(FILES))
OBJS		= $(addsuffix $(OBJ),$(FILES))
ASMS		= $(addsuffix $(ASM),$(FILES))

$(BINARY):	$(OBJS)
	$(CC) --keepmerged $(LDFALGS) $(OPTFLAGS) $(EXEOUT) $@ $(OBJS) $(LIBPATH) $(LIBS)

%$(OBJ) : %$(SRC)
	$(CC) $(CDEFS) $(EXTRA_CDEFS) $(MY_CDEFS) $(OPTFLAGS) $(CONLY) $<

clean:
	rm -f $(BINARY) $(OBJS) *~ *infer.c *cured.c *cil.c *_comb.c *.i

