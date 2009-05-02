include ../../config.make

IFLAGS=-lflags -I,$(CILHOME) \
       -lflags -I,$(OCAMLGRAPHHOME) 

CFLAGS=-cflags -dtypes \
       -cflags -I,$(OCAMLGRAPHHOME)

TARGETS=misc.cma misc.cmxa \
	bstats.cma bstats.cmxa \
	constants.cma constants.cmxa \
	common.cma common.cmxa \
	heap.cma heap.cmxa \
	errormsg.cma errormsg.cmxa

all:
	ocamlbuild $(LIBS) $(IFLAGS) $(CFLAGS) $(TARGETS)

clean:
	rm -rf *.byte *.native _build _log
