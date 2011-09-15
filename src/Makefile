include ../config.make 

DIRS=-I misc -I fixpoint

LIBS=-libs unix,str,cil,z3,graph

IFLAGS=-lflags -I,$(OCAMLGRAPHHOME) \
       -lflags -I,$(CILHOME) \
       -lflags -I,$(Z3HOME)/lib \
       -lflags -I,$(Z3HOME)/ocaml

LFLAGS=-lflags -cc,g++ \
       -lflags -cclib,-L$(Z3HOME)/lib \
       -lflags -cclib,-lz3 \
       -lflags -cclib,-lstdc++ \
       -lflags -cclib,-lz3stubs \
       -lflags -cclib,-lcamlidl

CFLAGS=-cflags -dtypes \
       -cflags -I,$(CILHOME) \
       -cflags -I,$(Z3HOME)/ocaml \
       -cflags -I,$(OCAMLGRAPHHOME) \
       -cflags -g \
       -cflags "-w a" \
			 -cflags -thread

all: ../lib/libliquidc.a liquidc.native indexSolver.native

.PHONY: %.native
%.native:
	ln -sf ../external/misc
	ln -sf ../external/fixpoint
	ocamlbuild $(DIRS) $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) -tags thread $(CIL) $(Z3) $@ || exit 1

%.a: %.o
	ar rcs $@ $<

%.o: %.c
	gcc -c $< -o $@

clean:
	rm -rf *.byte *.native _build _log

vitags:
	otags -vi *.ml fixpoint/*.ml misc/*.ml ../external/cil/src/*.ml
