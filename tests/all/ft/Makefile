PROG=ft.exe
LOCAL_CFLAGS=
LOCAL_LIBS=

OBJS = ft.o graph.o item.o Fheap.o random.o

include Makefile.ptrdist

test:
	$(SIM) ./ft.exe 10000 15000 > FOO
	$(DIFF) FOO ft-1k2k.OUT
	rm -f FOO

