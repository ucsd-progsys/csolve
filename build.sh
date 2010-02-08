########################## Fill in these fields
OCAMLLIB=/usr/lib/ocaml/3.10.2
##########################
cd external/cil/; ./configure; make; make check; cd ../../
cd external/ocamlgraph/; ./configure; make; make install; cd ../../
cd external/z3/ocaml; ./build.sh /usr/lib/ocaml/3.10.2; cd ../../../
cd src; make; cd ../
