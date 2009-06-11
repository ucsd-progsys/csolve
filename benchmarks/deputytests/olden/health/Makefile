# /* For copyright information, see olden_v1.0/COPYRIGHT */

BINARY		= health.exe
FILES		= health poisson list args ssplain

# ------- msvc ------
ifdef _MSVC
CC		= cl

CFLAGS		=
CONLY           = /c
SRC		= .c
OBJ		= .obj
ASM		= .s
EXEOUT          = /Fe

EXTRA_CDEFS	= /DI_TIME /DI_SYS_TIME /DULTRIX
CDEFS		= /DPLAIN /DSS_PLAIN
ifdef _DEBUG
OPTFLAGS        = /Zi /MLd
else
OPTFLAGS	= /Ox
endif

LIBS		= 
LIBPATH		= 

# ------- gcc ------
else
CC		= gcc

CFLAGS		= -O3
CONLY           = -c
SRC		= .c
OBJ		= .o
ASM		= .s

EXTRA_CDEFS	= -DI_TIME -DI_SYS_TIME -DULTRIX
CDEFS		= -DPLAIN -DSS_PLAIN
OPTFLAGS	= -Wall -O3
EXEOUT          = -o

LIBS	        = -lm $(EXTRALIBS)
LIBPATH		= 
endif

# ------- common --------
SRCS		= $(addsuffix $(SRC),$(FILES))
OBJS		= $(addsuffix $(OBJ),$(FILES))
ASMS		= $(addsuffix $(ASM),$(FILES))

defaulttarget: $(BINARY)

$(BINARY):	$(OBJS)
	$(CC) $(LDFALGS) $(OPTFLAGS) $(EXEOUT) $@ $(OBJS) $(LIBPATH) $(LIBS)

%$(OBJ) : %.c 
	$(CC) $(CDEFS) $(EXTRA_CDEFS) $(MY_CDEFS) $(OPTFLAGS) $(CONLY) $<

clean:
	rm -f $(BINARY) $(OBJS) *~ 



