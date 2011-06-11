open Union_find
open List
open Hashtbl
open Ssl_types
open Ssl
open Ssl_types.SSL_lex
open Ssl_substitution
open Ssl_normalization

exception SSL_unsat

(** A SSL formula is sat iff 
_ Each loc var that appears on the heap appears once
_ There is no x->l and x->nil and Alloc(l) on the heap
*)
let sat_ssl (sslf : ssl_formula ) =
  let fold_spacial _ occurences _ =
    if occurences > 1 then raise SSL_unsat
    else true
  in
  try
    match sslf.space with
	Space (space_f) ->
	  let heap_sat = ( Hashtbl.fold fold_spacial space_f true )
	  in heap_sat
      | Top_heap -> false
  with
      SSL_unsat -> false
	

