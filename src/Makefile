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
       -lflags -cclib,-lperfcount \
       -lflags -cclib,-lgomp \
       -lflags -cclib,-lstdc++ \
       -lflags -cclib,-lz3stubs \
       -lflags -cclib,-lcamlidl \
       -lflags -cclib,-fopenmp

CFLAGS=-cflags -dtypes \
       -cflags -I,$(CILHOME) \
       -cflags -I,$(Z3HOME)/ocaml \
       -cflags -I,$(OCAMLGRAPHHOME) \
       -cflags -g \
       -cflags "-w aevyz" \
       -cflags -thread

TARGETS=csolve.native

all: ../lib/libcsolve.a misc fixpoint
	ocamlbuild -no-links $(DIRS) $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) -tags thread $(CIL) $(Z3) $(TARGETS)

misc:
	ln -s ../external/misc

fixpoint:
	ln -s ../external/fixpoint

csolve.byte: misc fixpoint
	ocamlbuild $(DIRS) $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) -tags thread $(CIL) $(Z3) csolve.byte || exit 1

csolve.cma: csolve.byte
	ocamlbuild $(DIRS) $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) -tags thread $(CIL) $(Z3) csolve.cma || exit 1

csolve.top: csolve.cma
	ocamlmktop -I $(Z3HOME)/ocaml/ -I $(CILHOME) -o csolve.top _build/csolve.cma

%.a: %.o
	ar rcs $@ $<

%.o: %.c
	gcc -c $< -o $@

clean:
	rm -rf *.byte *.native _build _log

vitags:
	otags -vi *.ml fixpoint/*.ml misc/*.ml ../external/cil/src/*.ml
etags:
	otags *.ml fixpoint/*.ml misc/*.ml ../external/cil/src/*.ml
