include ../../config.make

IFLAGS=-lflags -I,$(CILHOME) \
       -lflags -I,$(OCAMLGRAPHHOME) 

CFLAGS=-cflags -dtypes \
       -cflags -I,$(OCAMLGRAPHHOME)

TARGETS=misc.cma misc.cmxa \
	bNstats.cma bNstats.cmxa \
	constants.cma constants.cmxa \
	fcommon.cma fcommon.cmxa \
	heaps.cma heaps.cmxa \
	errorline.cma errorline.cmxa

all:
	ocamlbuild -r $(LIBS) $(IFLAGS) $(CFLAGS) $(TARGETS)

clean:
	rm -rf *.byte *.native _build _log
