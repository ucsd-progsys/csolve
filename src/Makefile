include ../config.make 

DIRS=-I misc -I fixpoint

LIBS=-libs unix,str,cil,z3,graph

IFLAGS=-lflags -I,$(OCAMLGRAPHHOME) \
       -lflags -I,$(CILHOME) \
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
       -cflags -I,$(CILHOME) \
       -cflags -I,$(Z3HOME)/ocaml \
       -cflags -I,$(OCAMLGRAPHHOME) \
       -cflags -g

TARGETS=main.native sinfer.native recurcheck.native specparse.native

all:
	ln -sf ../external/misc
	ln -sf ../external/fixpoint
	for t in $(TARGETS); \
	 do ocamlbuild $(DIRS) $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) $(CIL) $(Z3) $$t; \
	 done

clean:
	rm -rf *.byte *.native _build _log
