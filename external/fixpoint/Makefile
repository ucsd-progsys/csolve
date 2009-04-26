include ../../config.make

MISCHOME=$(EXTHOME)/misc/_build

LIBS=-libs unix,str

IFLAGS=-lflags -I,$(MISCHOME) \
       -lflags -I,$(OCAMLGRAPHHOME)

LFLAGS=-lflags -cc,g++ \
       -lflags -cclib,-L$(OCAMLLIBHOME) 

CFLAGS=-cflags -dtypes \
       -cflags -I,$(MISCHOME) \
       -cflags -I,$(OCAMLGRAPHHOME)

TARGETS=solve.cma

all:
	ocamlbuild -r $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) $(TARGETS)

clean:
	rm -rf *.byte *.native _build _log
