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
       -lflags -cclib,-lcamlidl \
			 -lflags -cclib,-fopenmp

CFLAGS=-cflags -dtypes,-annot \
       -cflags -I,$(Z3HOME)/ocaml \
       -cflags -I,$(OCAMLGRAPHHOME) \
			 -cflags -thread

OFLAGS=$(DIRS) $(IFLAGS) $(LFLAGS) $(CFLAGS)

all:
	ln -sf ../misc
	ocamlbuild -r $(LIBS) $(OFLAGS) -tags thread fixpoint.native
	ocamlbuild -r $(OFLAGS) fix.cmxa
	cp _build/fixpoint.native .

clean:
	rm -rf *.byte *.native _build _log

fixtop:
	ocamlbuild -r $(LIBS) $(OFLAGS) fixtop.native

horn:
	ocamlbuild -r $(LIBS) $(OFLAGS) hornToInterproc.native

toplevel:
	ocamlbuild $(OFLAGS) misc/errorline.cmo misc/fixMisc.cmo misc/constants.cmo misc/bNstats.cmo misc/fcommon.cmo ast.cmo qualifier.cmo fixConstraint.cmo fixLex.cmo fixParse.cmo fixtop.cmo 
	ocamlmktop str.cma unix.cma _build/misc/fixMisc.cmo _build/misc/errorline.cmo _build/misc/constants.cmo _build/misc/bNstats.cmo ../ocamlgraph/graph.cmo _build/misc/fcommon.cmo _build/ast.cmo _build/qualifier.cmo _build/fixConstraint.cmo _build/fixLex.cmo _build/fixParse.cmo _build/fixtop.cmo -o _build/fixpoint.top

