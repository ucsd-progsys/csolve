####### Tweak these
OCAMLLIBHOME=/home/rjhala/local/lib/ocaml
EXTHOME=/home/rjhala/research/liquidc/external
####### End tweak

MISCHOME=$(EXTHOME)/misc/_build

LIBS=-libs unix,str

IFLAGS=-lflags -I,$(MISCHOME) 

LFLAGS=-lflags -cc,g++ \
       -lflags -cclib,-L$(OCAMLLIBHOME) 

CFLAGS=-cflags -dtypes \
       -cflags -I,$(MISCHOME) 

all:
	ocamlbuild -r $(LIBS) $(IFLAGS) $(LFLAGS) $(CFLAGS) ast.cmo

clean:
	rm -rf *.byte *.native _build _log
