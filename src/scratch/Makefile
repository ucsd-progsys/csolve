OCAMLLIBHOME = /home/rjhala/local/lib/ocaml
LIQUIDCHOME=$(shell pwd)/../

#Z3HOME  = $(LIQUIDCHOME)/external/z3
Z3HOME  = /home/rjhala/research/dsolve/external/z3

CILHOME = $(LIQUIDCHOME)/external/cil/obj/x86_LINUX
Z3_INC  = -I $(Z3HOME)/ocaml -I $(Z3HOME)/lib

# GPROF = 1
DEBUG = 1
# OCAML_BYTECODE_CINTF = 1
# Configuration

include $(LIQUIDCHOME)/src/config.make

CPPFLAGS := $(C_CPP_FLAGS) -Wall

ifdef PROFILING
OCAML_C = $(OCAML_CP) -p f
endif

OCAML_LIB      = libliquidc
OCAML_EXE      = liquidc
C_CPP_LIB      = libcliquidc
OCAML_CINTF    = liquidc_c_runtime
OCAML_CINTF_LIB = libliquidccintf

OCAML_LIB_OBJ  = misc \
		 message \
		 ast \
		 guards \
		 ssa_transform \
		 consgen \
		 theoremProver \
		 solver 
OCAML_EXE_OBJ  = $(OCAML_LIB_OBJ) \
		 main
OCAML_CINTF_OBJ = $(OCAML_LIB_OBJ) 

C_CPP_EXE_OBJ = #ml_sat_intf satsolvercore ml_cong_intf congclos ml_simplex_intf ml_alloc lp_rat simplex
C_CPP_LIB_OBJ = $(C_CPP_EXE_OBJ) 
C_CPP_CINTF_OBJ = $(C_CPP_EXE_OBJ) #foci_c_intf foci_cpp_intf

C_CPP_INCLUDES = -I $(LIQUIDCHOME)/include -I $(CILHOME) $(Z3_INC) $(OCAML_C_CPP_INC)
OCAML_INCLUDES = -I $(CILHOME) $(Z3_INC) 

C_CPP_LD_FLAGS = -L $(LIQUIDCHOME)/lib -L $(LIQUIDCHOME)/external/z3/lib -L $(OCAMLLIBHOME)
C_CPP_LD_LIBS  = -lnums -lstr -lunix -lutil -lz3 -lstdc++ -lz3stubs -lcamlidl

OCAML_LD_FLAGS = -I $(CILHOME) $(Z3_INC)
OCAML_LD_LIBS  = nums str unix z3 cil

MODULES        = $(OCAML_EXE_OBJ) fociCIntf
MLYS           =  
MLLS           = 


TODO           = $(OCAML_EXE).opt\
                 $(OCAML_LIB).cma $(OCAML_LIB).cmxa $(C_CPP_LIB).a $(OCAML_CINTF_LIB).a


all:: bin 

bin: $(TODO)
	mkdir -p $@
	cd bin; ln -fs ../$(OCAML_EXE).{byte,opt} .

lib: $(TODO)
	mkdir -p $@
	cd lib; ln -fs ../{$(OCAML_LIB).{cma,cmxa,a},$(C_CPP_LIB).a,$(OCAML_CINTF_LIB).a} .

clean: default-clean

distclean: clean
	rm -f *.cmi $(TODO) $(MLYS:%.mly=%.mli) $(MLYS:%.mly=%.ml) \
	$(MLLS:%.mll=%.ml)

srcdist:		
	cd ..; tar czvf foci.tar.gz ./foci/{*.ml,*.mli,*.mly,*.mll,Makefile,*.c,*.h,*.cpp,*.make} ./foci/README 
bindist:		
	tar czvhf liquidc-1.0b-i386.tar.gz README LICENSE bin lib include

.SECONDARY: $(MLYS:%.mly=%.mli) $(MLYS:%.mly=%.ml) $(MLLS:%.mll=%.ml)

.PHONY : all clean distclean


# Implicit rules

include $(LIQUIDCHOME)/src/rules.make


# Dependencies

-include $(MODULES:%=.%.ml.depend) $(MODULES:%=.%.mli.depend)
