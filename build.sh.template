########################## Fill in these fields
OCAMLLIB=/home/rjhala/local/lib/ocaml/
##########################
cd external/cil/; ./configure; make; make check; cd ../../
cd external/ocamlgraph/; ./configure; make; make install; cd ../../
cd external/z3/ocaml; ./build.sh $OCAMLLIB; cd ../../../
cd src; make; cd ../
