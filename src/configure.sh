#!/bin/bash
if [ ! -d "objs" ]; then
	mkdir objs
fi
echo "FLATAC_SRC_ROOT  = " `pwd` > makefile.conf && cat makefile_conf.generic >> makefile.conf 
