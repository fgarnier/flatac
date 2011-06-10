

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

ssl_substitution.cmo :  ssl_types.cmo ssl.cmo union_find.cmo 
	ocamlc -annot -c ssl_substitution.ml 

debug_printers.cmo : ssl_types.cmo ssl.cmo debug_printers.ml
	ocamlc -annot -c debug_printers.ml


ssl_normalization.cmo : ssl_types.cmo ssl.cmo union_find.cmo ssl_substitution.cmo
		ocamlc -annot -c ssl_normalization.ml

tests :  ssl_types.cmo ssl.cmo union_find.cmo  ssl_substitution.cmo tests.ml debug_printers.cmo 
	ocamlc -I ./ -annot -o test ssl_types.cmo ssl.cmo debug_printers.cmo union_find.cmo ssl_substitution.cmo tests.ml


html_api : ssl_types.cmo ssl.cmo union_find.cmo  ssl_substitution.cmo debug_printers.cmo
	ocamldoc -html -d ./api ssl_types.ml ssl.ml union_find.ml debug_printers.ml ssl_substitution.ml
clean : 
	rm -f *.o *.cmo *.cmi > /dev/null ; \
	cd api &&\
	rm -f *.html
