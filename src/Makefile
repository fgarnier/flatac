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

FRAMAC_SHARE  :=$(shell frama-c.byte -print-path)
FRAMAC_LIBDIR :=$(shell frama-c.byte -print-libpath)

SOURCES_FILES   := ast_goodies.ml cautomata.ml c2cautomatanalys.ml flatac_vistor.ml intermediate_language.ml flata_language.ml flatac_plugin.ml
F_SRC:= ~/FRAMAC

PLUGIN_NAME	= Flatac
PLUGIN_CMO	= self semAndLogicFrontEnd trueLogicFrontEnd ecfg flatac_ep 

include $(FRAMAC_SHARE)/Makefile.dynamic

FRAMA_C_DOC_DIR=-I /usr/lib/ocaml \
	-I $(F_SRC)/src/kernel \
	-I $(F_SRC)/src/project \
	-I $(F_SRC)/cil/ocamlutil \
	-I $(F_SRC)/cil/src/logic \
	-I $(F_SRC)/cil/src \
	-I $(F_SRC)/cil/src/ext \
	-I $(F_SRC)/src/lib \
	-I $(F_SRC)/src/lib/fc

DOC_OPTIONS= -g $(F_SRC)/doc/code/docgen.cmo

STDLIB_FILES:=printf list string 
STDLIB_FILES:=$(patsubst %, $(OCAMLLIB)/%.mli, $(STDLIB_FILES))

html_doc: 
	ocamldoc -html -d doc/html/ $(FRAMA_C_DOC_DIR) self.ml semAndLogicFrontEnd.ml trueLogicFrontEnd.ml ecfg.ml flatac_ep.ml

latex_doc: 
	ocamldoc -latex -d doc/latex/ $(FRAMA_C_DOC_DIR) self.ml semAndLogicFrontEnd.ml trueLogicFrontEnd.ml ecfg.ml flatac_ep.ml
