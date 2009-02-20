HOME=$(PWD)/../
#Z3HOME=$(HOME)/external/z3
Z3HOME=/home/rjhala/research/dsolve/external/z3
CILHOME=$(HOME)/external/cil/obj/x86_LINUX
OCAMLLIBHOME=/home/rjhala/local/lib/ocaml

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

all:
	ocamlbuild $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) $(CIL) $(Z3) main.native

clean:
	rm -rf *.byte *.native _build _log
