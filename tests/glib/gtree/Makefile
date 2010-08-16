include ../Makefile.config

gtree: driver.o gtree.o
	$(CC) driver.o gtree.o -o test

gtree.o: gtree.c gtree.h
	$(CC) $(CFLAGS) -c gtree.c

driver.o: driver.c gtree.h
	$(CC) $(CFLAGS) -c driver.c

clean:
	rm *.o liquid.c liquid.c.(tags|vmap|autospec|in.fq|out.fq|ssa.c|annot) test
