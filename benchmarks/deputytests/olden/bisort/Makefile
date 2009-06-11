# /* For copyright information, see olden_v1.0/COPYRIGHT */

BINARY		= bisort.exe
FILES		= bitonic swap args

ifdef _MSVC
CC		= cl

SRC		= .c
OBJ		= .obj
ASM		= .s

EXTRA_CDEFS	= /DI_TIME /DI_SYS_TIME /DULTRIX
CDEFS		= /DPLAIN /DSS_PLAIN
OPTFLAGS	= /Ox

LIBS		= 
LIBPATH		= 
else
CC		= gcc

SRC		= .c
OBJ		= .o
ASM		= .s

EXTRA_CDEFS	= -DI_TIME -DI_SYS_TIME -DULTRIX
CDEFS		= -DPLAIN -DSS_PLAIN
OPTFLAGS	= -g -Wall -O3

LIBS		= -lm
LIBPATH		= 
endif

SRCS		= $(addsuffix $(SRC),$(FILES))
OBJS		= $(addsuffix $(OBJ),$(FILES))
ASMS		= $(addsuffix $(ASM),$(FILES))

defaulttarget: $(BINARY)

$(BINARY):	$(OBJS)
	$(CC) $(LDFALGS) $(OPTFLAGS) -o $@ $(OBJS) $(LIBPATH) $(LIBS)

$(SRC)$(OBJ):
	$(CC) $(CDEFS) $(EXTRA_CDEFS) $(MY_CDEFS) $(OPTFLAGS) -c $<

clean:
	rm -f $(BINARY) $(OBJS) *~ *.i *.cil.c

