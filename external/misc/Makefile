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
