open Union_find
open List
open Hashtbl
open Ssl_types
open Ssl
open Ssl_types.SSL_lex





(** Removal of uninstanciated existancially quantified variables of
a SSL formula *)

let var_elim ( sslf : ssl_formula ) =
  let iter_elim lvar () =
    if (ssl_contains_locvar lvar sslf  ) == false
    then Hashtbl.remove sslf.quant_vars lvar
    else ()
  in
  Hashtbl.iter iter_elim sslf.quant_vars
    

 
