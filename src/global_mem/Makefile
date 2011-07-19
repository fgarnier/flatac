include ../makefile.conf

OBJS	= global_mem.cmo global_mem.cmx 

all : $(OBJS)

install_lib_flatac : all
	cp *.cmo ../objs && cp *.cmi ../objs && cp *.cmx ../objs && cp *.o ../objs

global_mem.cmo : global_mem.ml
	@echo "compiling $< to $@"  
	@ocamlc $(FRAMA_C_INCLUDES) $(SSL_INCLUDES) -c global_mem.ml
 

global_mem.cmx : global_mem.ml
	@echo "compiling $< to $@"
	@ocamlopt $(FRAMA_C_INCLUDES) $(SSL_INCLUDES) -c global_mem.ml

clean :
	rm -f *.cmo *.cmi *.cmx *.o

