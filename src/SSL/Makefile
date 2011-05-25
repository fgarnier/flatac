

COMPINTERFACES =  ssl_types.cmo ssl.cmo


all : $(COMPINTERFACES)

.ml.cmo : $(COMPINTERFACES)
	ocmalc -c $*.ml

ssl_types.cmo : ssl_types.ml
	ocamlc -annot -c ssl_types.ml

ssl.cmo : ssl.ml ssl_types.cmo
	ocamlc -annot -c ssl.ml

union_find.cmo : ssl_types.cmo ssl.cmo union_find.ml
	ocamlc -annot -c union_find.ml

tests :  ssl_types.cmo ssl.cmo tests.ml 
	ocamlc -I ./ -annot -o test ssl_types.cmo ssl.cmo tests.ml

clean : 
	rm -f *.o *.cmo *.cmi
