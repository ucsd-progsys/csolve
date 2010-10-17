include ../../config.make 

DIRS=-I misc -I src

LIBS=-libs unix,str,cil,graph

IFLAGS=-lflags -I,$(OCAMLGRAPHHOME) \
       -lflags -I,$(CILHOME) \
			 -lflags -I,$(SRCHOME)

CFLAGS=-cflags -dtypes \
       -cflags -I,$(CILHOME) \
       -cflags -I,$(OCAMLGRAPHHOME) \
			 -cflags -I,$(SRCHOME) \
       -cflags -g

BIN=main.native

TARGETS=$(BIN)

all:
	ln -sf ../../external/misc
	ln -sf ../../external/fixpoint
	ln -sf ../../src
	ln -sf ../../external/cil
	ocamlbuild $(DIRS) $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) $(CIL) $(BIN)
	chmod a+x _build/main.native

clean:
	rm -rf *.byte *.native _build _log
