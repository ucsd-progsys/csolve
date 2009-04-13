####### Tweak these
OCAMLLIBHOME=/home/rjhala/local/lib/ocaml
EXTHOME=/home/rjhala/research/liquidc/external
CILHOME=$(EXTHOME)/cil/obj/x86_LINUX
####### End tweak

IFLAGS=-lflags -I,$(CILHOME) 

CFLAGS=-cflags -dtypes \
       -cflags -I,$(CILHOME) 

all:
	ocamlbuild -r $(LIBS) $(IFLAGS) $(CFLAGS) misc.cma misc.cmxa

clean:
	rm -rf *.byte *.native _build _log
