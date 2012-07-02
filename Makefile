all:
	./build.sh

configure:
	./configure
	cd external/gnu-coreutils && ./csolve.configure && cd ../../

test:
	cd src && ./regrtest.py -t 24

clean:
	cd src && make clean
	cd external/cil && make clean
	cd external/misc && make clean
	cd external/z3/ocaml && ./clean.sh
	./cleanup

distclean:
	rm -rf demo/ benchmarks/
	rm -rf external/fixpoint/benchmarks/ external/fixpoint/tests/
	rm src/TODO
	rm -rf doc/
	rm -rf .git/
	rm Makefile
