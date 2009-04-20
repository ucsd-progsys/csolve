####### Tweak these
OCAMLLIBHOME=/home/rjhala/local/lib/ocaml
EXTHOME=/home/rjhala/research/liquidc/external
CILHOME=$(EXTHOME)/cil/obj/x86_LINUX
####### End tweak

IFLAGS=-lflags -I,$(CILHOME) 

CFLAGS=-cflags -dtypes \
       -cflags -I,$(CILHOME) 

TARGETS=misc.cma misc.cmxa bstats.cma bstats.cmxa

all:
	ocamlbuild -r $(LIBS) $(IFLAGS) $(CFLAGS) $(TARGETS)

clean:
	rm -rf *.byte *.native _build _log
