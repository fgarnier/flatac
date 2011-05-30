(** In this file we define the type of a substitution as well
as how to transfor the syntax of a SSL formula upon a substitution.*)


open Union_find
open List
open Hashtbl
open Ssl_types
open Ssl
open Ssl_types.SSL_lex

(* Keys : Domain of the substitutionm and values are the range *)
type loc_subst =  Subst of (locvar , locvar ) t

let eq_class_inversor  (repres : SSL_lex.locvar ) (lvars : SSL_lex.locvar )
    () (tble : (locvar , locvar) t) =
  Hashtbl.add tble lvars repres; tble 


let subst_from_partition (part : Union_find.partition ) =
  let inverse_image_folder (locv : SSL_lex.locvar) (eq_c : Union_find.eqclass)
      (table_subst : (locvar , locvar) t) =
    (Hashtbl.fold ( eq_class_inversor eq_c.repres ) eq_c.members table_subst)
   
  in
      
  match part with 
      Partition (table_part ) -> Hashtbl.fold  inverse_image_folder table_part (Hashtbl.create SSL_lex.size_hash)


