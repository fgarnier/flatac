open union_find
open List
open Hashtbl
open Ssl_types
open Ssl
open Ssl_types.SSL_lex





(** Removal of uninstanciated existancially quantified variables of
a SSL formula *)

let qelim ( sslf : ssl_formula ) =
  
