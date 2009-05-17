PROG=ft
LOCAL_CFLAGS=
LOCAL_LIBS=

OBJS = ft.o graph.o item.o Fheap.o random.o

include ../Makefile

test:
	$(SIM) ./ft 100 100 > FOO
	$(DIFF) FOO ft-1c1c.OUT
	rm -f FOO
	$(SIM) ./ft 1000 2000 > FOO
	$(DIFF) FOO ft-1k2k.OUT
	rm -f FOO

