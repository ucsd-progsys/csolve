CC=gcc

build: 
	$(MAKE) -C src CC="$(CC)"

clean: 
	$(MAKE) -C src clean


test: 
	time sh -c "src/epic.exe data/test_image.pgm -b 25 && src/unepic.exe data/test_image.pgm.E"
