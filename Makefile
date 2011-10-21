all:
	./build.sh

clean:
	cd src && make clean
	cd external/cil && make clean
	cd external/misc && make clean
	cd external/z3/ocaml && ./clean.sh
	./cleanup
