all:
	./build.sh

clean:
	cd src && make clean && cd ..
	cd external
	cd cil && make clean && cd ..
	cd misc && make clean && cd ..
	cd z3 && ./clean.sh && cd ..
