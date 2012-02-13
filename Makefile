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
	rm commit.sh init-submodules.sh pull.sh push.sh reset.sh
	rm -rf demo/ benchmarks/ git-hooks/
	rm -rf lib/cpj.h tests/cpj/ tests/postests/cpj/
	rm -rf external/gnu-coreutils/ tests/postests/coreutils/ tests/all/
	rm -rf external/fixpoint/benchmarks/ external/fixpoint/tests/
	rm src/TODO
	rm -rf doc/
	find . -name '.git*' -exec rm -rf {} ";"
	rm Makefile
