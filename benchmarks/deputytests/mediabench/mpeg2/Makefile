
.PHONY: build verify clean test
build: 
	$(MAKE) -C src/mpeg2enc build
	$(MAKE) -C src/mpeg2dec build

mpeg2encode:
	cd src/mpeg2enc; make 'CC=$(CC)' 'CFLAGS=$(CFLAGS)'

clean:
	$(MAKE) -C src/mpeg2dec clean
	$(MAKE) -C src/mpeg2enc clean

verify:
	$(MAKE) -C verify verify

test: 
	cd src; time sh -c "mpeg2enc/mpeg2encode.exe ../data/options.par ../data/out.m2v && mpeg2dec/mpeg2decode.exe -b ../data/mei16v2.m2v -r -f -o0 ../data/tmp%d"

