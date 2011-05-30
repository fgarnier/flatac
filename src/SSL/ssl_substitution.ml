(** In this file we define the type of a substitution as well
as how to transfor the syntax of a SSL formula upon a substitution.*)


open union_find
open List
open Hashtbl
open Ssl_types
open Ssl
open Ssl_types.SSL_lex

(* Keys : Domain of the substitutionm and values are the range *)
type loc_subst =  Subst of (locvar , locvar ) t

let subst_from_partition (part : Union_find.partition ) =
  match part with 
      Partition (table_part ) ->
