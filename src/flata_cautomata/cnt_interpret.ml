(**********************************************************************)
(* 
This file contains the functions that translates the arithmetics
expression from CIL into counter automata based one. The focus is maid
on pointer arithmetic, especialy for int * pointer arithmetics

Questions and/or remarks : mail to florent dot garnier At imag dot fr
*) 

open Cil_types
open Intermediate_language
open Ssl_types
open Ssl
open SSL_lex


type 

let rec interpret_c_scal_to_cnt
