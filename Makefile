all:
	./build.sh

clean:
	cd src && make clean
	cd external/cil && make clean
	cd external/misc && make clean
	cd external/z3/ocaml && ./clean.sh
	./cleanup

distclean:
	rm commit.sh init-submodules.sh pull.sh push.sh reset.sh
	rm -rf demo/ benchmarks/ git-hooks/
	rm lib/cpj.h
	rm -rf external/gnu-coreutils/ tests/postests/coreutils/
	rm -rf external/fixpoint/benchmarks/ external/fixpoint/tests/
	rm src/TODO
	rm -rf doc/
	find . -name '.git*' -exec rm -rf {} ";"
	rm Makefile
