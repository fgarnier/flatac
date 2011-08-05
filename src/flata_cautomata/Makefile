
SSL_SRC = /home/florent/REPOSITORIES/flatac/src/SSL
GLOBAL_MEM = /home/florent/REPOSITORIES/flatac/src 
F_SRC:= /home/florent/FRAMAC/frama-c-Carbon-20110201

FRAMA_C_INCLUDES=       -I $(F_SRC)/src/kernel \
                        -I $(F_SRC)/src/project \
                        -I $(F_SRC)/cil/ocamlutil \
                        -I $(F_SRC)/cil/src/logic \
                        -I $(F_SRC)/cil/src \
                        -I $(F_SRC)/cil/src/ext \
                        -I $(F_SRC)/src/lib \
                        -I . \
                        -I $(F_SRC)/src/lib/fc

SSL_INCLUDES 	= -I $(SSL_SRC)
GLOBAL_MEM_INCLUDE = -I $(GLOBAL_MEM)$

intermediate_language.cmo : intermediate_language.ml
	ocamlc $(FRAMA_C_INCLUDES)  -c intermediate_language.ml

validity.cmo : intermediate_language.cmo validity.ml
	ocamlc $(FRAMA_C_INCLUDES) $(SSL_INCLUDES) -c validity.ml

cnt_interpret.cmo : intermediate_language.cmo validity.cmo cnt_interpret.ml
	ocamlc $(GLOBAL_MEM_INCLUDE)$ $(FRAMA_C_INCLUDES) $(SSL_INCLUDES)$ -c cnt_interpret.ml

clean :
	rm -f *.cmo *.cmi
