include ../makefile.conf

OBJS = c_upon_ssl_domain.cmo c_upon_ssl_domain.cmx

all : $(OBJS)

c_upon_ssl_domain.cmo : 
	ocamlc $(FRAMA_C_INCLUDES) $(GLOBAL_MEM_INCLUDES) $(SSL_INCLUDES) -c c_upon_ssl_domain.ml

c_upon_ssl_domain.cmx : 
	ocamlopt $(FRAMA_C_INCLUDES) $(GLOBAL_MEM_INCLUDES) $(SSL_INCLUDES) -c c_upon_ssl_domain.ml

clean :
	rm -f *.cmo *.cmi *.o *.cmx

install_lib_flatac : all
	cp *.cmo ../objs && cp *.cmx ../objs && cp *.cmi ../objs
