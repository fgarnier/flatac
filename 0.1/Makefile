##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2010                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

# Example of Makefile for dynamic plugins
#########################################

# Frama-c should be properly installed with "make install" 
# before any use of this makefile

LIB_OCAML_PATH = /opt/local/lib/ocaml/


FRAMAC_SHARE  	:=$(shell frama-c.byte -print-path)
FRAMAC_LIBDIR 	:=$(shell frama-c.byte -print-libpath)



SOURCES_FILES   := cautomata.ml c2cautomatanalys.ml  print_fun.ml fun_info.ml intermediate_language.ml
F_SRC		:=~/FRAMAC

PLUGIN_NAME	= Pfun
PLUGIN_CMO	=  intermediate_language cautomata c2cautomatanalys print_fun fun_info

include $(FRAMAC_SHARE)/Makefile.dynamic

FRAMA_C_DOC_DIR	=	-I $(LIB_OCAML_PATH))\
		 	-I $(F_SRC)/src/kernel \
			-I $(F_SRC)/src/project \
			-I $(F_SRC)/cil/ocamlutil \
			-I $(F_SRC)/cil/src/logic \
			-I $(F_SRC)/cil/src \
			-I $(F_SRC)/src/lib \
			-I .

DOC_OPTIONS=-g $(F_SRC)/doc/code/docgen.cmo

STDLIB_FILES:=printf c2cautomatanalys list string  cautomata       
STDLIB_FILES:=$(patsubst %, $(OCAMLLIB)/%.mli, $(STDLIB_FILES))

doc_html:
	@ echo 'Entering directory' ${PWD} 
	ocamldoc -html -v -d doc \
	        $(DOC_OPTIONS) \
		$(addprefix -stdlib , $(STDLIB_FILES)) \
		$(FRAMA_C_DOC_DIR) \
		$(SOURCES_FILES)



