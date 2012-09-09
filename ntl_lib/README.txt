



This folder contains the files that define the OCAML version
of the Numerical Transition Systel Library, which sepecification
is available here : http://richmodels.epfl.ch/ntscomp.

Requirements :
 _ Ocaml compiler v3.12 or over.
 _ Python interpreter. 

The files nts_functor.ml and nts_functor.mli define a fonctorial 
interface which allows to deal with a wide range of numerical 
transitions systems. 

In its current state of developpement, this library contains :

 _ A parser : (Provides a function that returns a numerical transition
	system from a text input, developped using ocamllex and ocamlyacc.
	The current parser is defined for the nts which control states 
	are labelled using integers.
	)

 _ A pretty printer.
 _ Syntactic and type checking : Developpment in progress.

To build and test the library :

 make -f makefileparser all

The api can be genrated using the targer docgen of the aforementionned
makefile.

 make -f makefileparser docgen
 

(c) Verimag 2012.
 For any question or remark, please write to florent dot garnier at imag dot fr 
