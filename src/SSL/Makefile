
CFLAGS = -g -annot

COMPINTERFACES = ssl_types.cmo ssl.cmo union_find.cmo ssl_substitution.cmo ssl_normalization.cmo ssl_decision.cmo ssl_entailement.cmo ssl_biabduction.cmo

COMP_OPT_OBJS =  ssl_types.cmx ssl.cmx union_find.cmx ssl_substitution.cmx ssl_normalization.cmx ssl_decision.cmx  ssl_entailement.cmx ssl_biabduction.cmx


all : $(COMPINTERFACES) $(COMP_OPT_OBJS)

install_lib_flatac : all
	cp *.cmo ../objs && cp *.cmx ../objs && cp *.cmi ../objs 

.ml.cmo : $(COMPINTERFACES)
	ocmalc $(CFLAGS) $*.ml

ssl_types.cmo : ssl_types.ml
	ocamlc $(CFLAGS)  -c ssl_types.ml

ssl.cmo : ssl.ml ssl_types.cmo
	ocamlc $(CFLAGS)  -c ssl.ml

union_find.cmo : ssl_types.cmo ssl.cmo union_find.ml
	ocamlc $(CFLAGS)  -c union_find.ml

ssl_substitution.cmo :  ssl_types.cmo ssl.cmo union_find.cmo 
	ocamlc $(CFLAGS)  -c ssl_substitution.ml 

debug_printers.cmo : ssl_types.cmo ssl.cmo  ssl_entailement.cmo debug_printers.ml
	ocamlc $(CFLAGS)  -c debug_printers.ml

ssl_normalization.cmo : ssl_types.cmo ssl.cmo union_find.cmo ssl_substitution.cmo
		ocamlc $(CFLAGS)  -c ssl_normalization.ml

ssl_decision.cmo :  ssl_types.cmo ssl.cmo union_find.cmo ssl_substitution.cmo ssl_normalization.cmo 
	ocamlc $(CFLAGS)  -c ssl_decision.ml

ssl_entailement.cmo : ssl_types.cmo ssl.cmo union_find.cmo ssl_substitution.cmo ssl_normalization.cmo ssl_decision.cmo ssl_entailement.ml 
	ocamlc $(CFLAGS)  -c ssl_entailement.ml

ssl_biabduction.cmo : ssl_types.cmo ssl.cmo union_find.cmo ssl_substitution.cmo ssl_normalization.cmo ssl_decision.cmo  ssl_entailement.cmo debug_printers.cmo ssl_biabduction.ml
	ocamlc $(CFLAGS)  -c ssl_biabduction.ml


ssl_types.cmx : ssl_types.ml
	ocamlopt $(CFLAGS)  -c ssl_types.ml

ssl.cmx : ssl.ml ssl_types.cmx
	ocamlopt $(CFLAGS)  -c ssl.ml

union_find.cmx : ssl_types.cmx ssl.cmx union_find.ml
	ocamlopt $(CFLAGS)  -c union_find.ml

ssl_substitution.cmx :  ssl_types.cmx ssl.cmx union_find.cmx 
	ocamlopt $(CFLAGS)  -c ssl_substitution.ml 

debug_printers.cmx : ssl_types.cmo ssl.cmx  ssl_entailement.cmx debug_printers.ml
	ocamlopt $(CFLAGS)  -c debug_printers.ml


ssl_normalization.cmx : ssl_types.cmx ssl.cmx union_find.cmx ssl_substitution.cmx
		ocamlopt $(CFLAGS)  -c ssl_normalization.ml

ssl_decision.cmx :  ssl_types.cmx ssl.cmx union_find.cmx ssl_substitution.cmx ssl_normalization.cmx 
	ocamlopt $(CFLAGS)  -c ssl_decision.ml

ssl_entailement.cmx : ssl_types.cmx ssl.cmx union_find.cmx ssl_substitution.cmx ssl_normalization.cmx ssl_decision.cmx ssl_entailement.ml 
	ocamlopt $(CFLAGS)  -c ssl_entailement.ml

ssl_biabduction.cmx : ssl_types.cmx ssl.cmx union_find.cmx ssl_substitution.cmx ssl_normalization.cmx ssl_decision.cmx  ssl_entailement.cmx debug_printers.cmx ssl_biabduction.ml
	ocamlopt $(CFLAGS)  -c ssl_biabduction.ml


tests :  ssl_types.cmo ssl.cmo union_find.cmo  ssl_substitution.cmo ssl_normalization.cmo tests.ml debug_printers.cmo ssl_decision.cmo ssl_entailement.cmo ssl_biabduction.cmo
	ocamlc -I ./ $(CFLAGS) -o test ssl_types.cmo ssl.cmo debug_printers.cmo union_find.cmo ssl_substitution.cmo ssl_normalization.cmo ssl_decision.cmo ssl_entailement.cmo ssl_biabduction.cmo tests.ml

test_abd :  ssl_types.cmo ssl.cmo union_find.cmo  ssl_substitution.cmo ssl_normalization.cmo tests.ml debug_printers.cmo ssl_decision.cmo ssl_entailement.cmo ssl_biabduction.cmo
		ocamlc -I ./  $(CFLAGS)  -o test_biabduction  ssl_types.cmo ssl.cmo debug_printers.cmo union_find.cmo ssl_substitution.cmo ssl_normalization.cmo ssl_decision.cmo ssl_entailement.cmo ssl_biabduction.cmo test_biabduction.ml


debug_r4 :  ssl_types.cmo ssl.cmo union_find.cmo  ssl_substitution.cmo ssl_normalization.cmo tests.ml debug_printers.cmo ssl_decision.cmo ssl_entailement.cmo ssl_biabduction.cmo debug_r4.ml
		ocamlc -I ./  $(CFLAGS)  -o debug_r4  ssl_types.cmo ssl.cmo debug_printers.cmo union_find.cmo ssl_substitution.cmo ssl_normalization.cmo ssl_decision.cmo ssl_entailement.cmo ssl_biabduction.cmo debug_r4.ml

test_big_relation : ssl_types.cmo ssl.cmo union_find.cmo  ssl_substitution.cmo ssl_normalization.cmo tests.ml debug_printers.cmo ssl_decision.cmo ssl_entailement.cmo ssl_biabduction.cmo
	ocamlc -I ./  $(CFLAGS)  -o test_big_realtions  ssl_types.cmo ssl.cmo debug_printers.cmo union_find.cmo ssl_substitution.cmo ssl_normalization.cmo ssl_decision.cmo ssl_entailement.cmo ssl_biabduction.cmo test_big_relations.ml

html_api : ssl_types.cmo ssl.cmo union_find.cmo  ssl_substitution.cmo  ssl_normalization.cmo ssl_decision.cmo ssl_entailement.cmo debug_printers.cmo ssl_biabduction.cmo 
	ocamldoc -html -d ./api ssl_types.ml ssl.ml union_find.ml debug_printers.ml ssl_substitution.ml ssl_normalization.ml ssl_entailement.ml ssl_decision.ml ssl_biabduction.ml

clean : 
	rm -f *.o *.cmo *.cmi *.cmx *.annot > /dev/null ; \
	rm test > /dev/null ; \
	rm test_biabduction > /dev/null; \
	cd api &&\
	rm -f *.html\
