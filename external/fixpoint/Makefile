include ../../config.make

LIBS=-libs unix,str,z3,graph

DIRS=-I misc

IFLAGS=-lflags -I,$(OCAMLGRAPHHOME) \
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
       -cflags -I,$(Z3HOME)/ocaml \
       -cflags -I,$(OCAMLGRAPHHOME)

OFLAGS=$(DIRS) $(IFLAGS) $(LFLAGS) $(CFLAGS)

all:
	ln -sf ../misc
	ocamlbuild -r $(LIBS) $(OFLAGS) main.native
	ocamlbuild -r $(OFLAGS) fix.cmxa
	cp _build/main.native .

clean:
	rm -rf *.byte *.native _build _log

fixtop:
	ocamlbuild -r $(LIBS) $(OFLAGS) fixtop.native
