####### Tweak these
OCAMLLIBHOME=/home/rjhala/local/lib/ocaml
EXTHOME=/home/rjhala/research/liquidc/external
CILHOME=$(EXTHOME)/cil/obj/x86_LINUX
OCAMLGRAPHHOME=/home/rjhala/research/dsolve/external/ocamlgraph
####### End tweak

IFLAGS=-lflags -I,$(CILHOME) \
       -lflags -I,$(OCAMLGRAPHHOME) 

CFLAGS=-cflags -dtypes \
       -cflags -I,$(CILHOME) \
       -cflags -I,$(OCAMLGRAPHHOME)

TARGETS=misc.cma misc.cmxa \
	bstats.cma bstats.cmxa \
	constants.cma constants.cmxa \
	common.cma common.cmxa \
	heap.cma heap.cmxa
all:
	ocamlbuild -r $(LIBS) $(IFLAGS) $(CFLAGS) $(TARGETS)

clean:
	rm -rf *.byte *.native _build _log
