
CC=../../../../src/cc.sh

ifdef DEBUG
FLAGS=-g $(EXTRACFLAGS)
else
FLAGS=-O3 $(EXTRACFLAGS)
endif

build: encode$(EXE) decode$(EXE)

encode$(EXE): bit_output.o arithmetic_encode.o adaptive_model.o encode.o
	$(CC) $(FLAGS) -o $@ $^

decode$(EXE): bit_input.o arithmetic_decode.o adaptive_model.o decode.o
	$(CC) $(FLAGS) -o $@ $^

%.o : %.c
	$(CC) $(FLAGS) -c $< 

clean:
	rm -f *.o encode$(EXE) decode$(EXE) *~
