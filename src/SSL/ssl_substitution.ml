(** In this file we define the type of a substitution as well
as how to transfor the syntax of a SSL formula upon a substitution.

Athor : Florent Garnier.
write to first_name-dot-name-at-imag-dot-fr for question and/or comments.
*)


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




(* This fonction shall not appear in the ml-interface file *)

let subst_against_affectation (subst : loc_subst )(affect_table : ((SSL_lex.ptvar , (SSL_lex.locvar , unit) t ) t)) =
  let subst_map (subst_table : ((SSL_lex.locvar , SSL_lex.locvar ) t)) (current_table: (SSL_lex.locvar , unit) t ) (lvar : SSL_lex.locvar) () =
    if (( Hashtbl.mem subst_table lvar ) == true )
    then 
      begin
	Hashtbl.remove current_table lvar;
	let rvar =  Hashtbl.find subst_table lvar in 
	Hashtbl.add current_table rvar ()
      end
    else ()
  in
  let affect_table_iterator subst_table pvar lvar_table =
    Hashtbl.iter ( subst_map subst_table lvar_table ) lvar_table
  in
  match subst with 
      Subst ( table_subst ) ->
	Hashtbl.iter (affect_table_iterator table_subst ) affect_table
	


