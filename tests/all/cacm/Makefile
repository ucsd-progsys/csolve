
CC=../../../src/csolve --merge

ifdef DEBUG
FLAGS=-g $(EXTRACFLAGS)
else
FLAGS=$(EXTRACFLAGS) -I../../../include/ -U __USE_EXTERN_INLINES
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
