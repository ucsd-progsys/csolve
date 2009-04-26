include ../../config.make

MISCHOME=$(EXTHOME)/misc/_build

LIBS=-libs unix,str

IFLAGS=-lflags -I,$(MISCHOME) \
       -lflags -I,$(OCAMLGRAPHHOME) \
       -lflags -I,$(Z3HOME)/lib \
       -lflags -I,$(Z3HOME)/ocaml 

LFLAGS=-lflags -cc,g++ \
       -lflags -cclib,-L$(OCAMLLIBHOME) \
       -lflags -cclib,-L$(Z3HOME)/lib \
       -lflags -cclib,-lz3 \
       -lflags -cclib,-lstdc++ \
       -lflags -cclib,-lz3stubs \
       -lflags -cclib,-lcamlidl

CFLAGS=-cflags -dtypes \
       -cflags -I,$(MISCHOME) \
       -cflags -I,$(OCAMLGRAPHHOME) \
       -cflags -I,$(Z3HOME)/ocaml

TARGETS=solve.cma

all:
	ocamlbuild -r $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) $(TARGETS)

clean:
	rm -rf *.byte *.native _build _log
