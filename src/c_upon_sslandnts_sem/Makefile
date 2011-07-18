
FLATAC_SRC_ROOT = /home/florent/REPOSITORIES/flatac/src/
SSL_SRC = $(FLATAC_SRC_ROOT)/SSL
F_SRC:= /home/florent/FRAMAC_LATTEST/frama-c-Carbon-20110201

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
GLOBAL_MEM = -I $(FLATAC_SRC_ROOT)/global_mem


c_upon_ssl_domain.cmo : 
	ocamlc $(FRAMA_C_INCLUDES) $(GLOBLAL_MEM) $(SSL_INCLUDES) -c c_upon_ssl_domain.ml

clean :
	rm -f *.cmo *.cmi
