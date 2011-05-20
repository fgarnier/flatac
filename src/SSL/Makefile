

COMPINTERFACES =  ssl_types.cmo ssl.cmo


all : $(COMPINTERFACES)

.ml.cmo : $(COMPINTERFACES)
	ocmalc -c $*.ml


tests :  ssl_types.cmo ssl.cmo tests.ml 
	ocamlc -I ./ -o test ssl_types.cmo ssl.cmo tests.ml

clean : 
	rm -f *.o *.cmo *.cmi
