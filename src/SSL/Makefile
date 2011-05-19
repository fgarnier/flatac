

COMPINTERFACES =  ssl_types.cmo ssl.cmo


all : $(COMPINTERFACES)

.ml.cmo : $(COMPINTERFACES)
	ocmalc -c $*.ml
