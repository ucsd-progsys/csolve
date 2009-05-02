include ../config.make 

LIBS=-libs unix,str,cil,z3

IFLAGS=-lflags -I,$(CILHOME) \
       -lflags -I,$(Z3HOME)/lib \
       -lflags -I,$(Z3HOME)/ocaml \
			 -lflags -I,$(EXTHOME)/misc/_build \

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

all:
	ocamlbuild $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) $(CIL) $(Z3) main.native

clean:
	rm -rf *.byte *.native _build _log
