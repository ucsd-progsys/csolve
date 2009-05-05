include ../config.make 

DIRS=-I misc -I fixpoint

LIBS=-libs unix,str,cil,z3

IFLAGS=-lflags -I,$(CILHOME) \
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
       -cflags -I,$(Z3HOME)/ocaml

TARGETS=consgen.cmo #ssa_transform.cmo #guards.cmo main.native

all:
	ln -sf ../external/misc
	ln -sf ../external/fixpoint
	ocamlbuild $(DIRS) $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) $(CIL) $(Z3) $(TARGETS) 

clean:
	rm -rf *.byte *.native _build _log
