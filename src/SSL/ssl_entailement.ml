(** This files contains the fonctions and the definitions required
to decide the entailement property. *)

open Hashtbl
open List
open Ssl_types 
open Ssl
open Ssl_normalization
open SSL_lex



type entail_problem = {left : ssl_formula ; right : ssl_formula ;}


(** On elimine les x ->l  ... |- x-> l
*)

let fresh_locvar_name (etp : entail_problem ) =
  

let reduce_r1  ( etp : entail_problem ) = 
  let varname_folder lvar () lvar_arg =
    if cmp_lex_lvar lvar lvar_arg then
      lvar 
    else lvar_arg
  in
  let r1_iterator pvar loctable  =
    if Hashtbl.mem etp.right.pure.affectations pvar then
    let lvar_rel = Hashtbl.fold varname_folder loctable (LVar("")) in
    let pvar_right = Hashtbl.find etp.right.pure.affectations pvar in
    if Hashtbl.mem pvar_right lvar_rel then
      if  ( not ( Hashtbl.mem etp.right.quant_vars lvar_rel )  ) &&  ( not ( Hashtbl.mem etp.left.quant_vars lvar_rel ) = false ) 
      then
	begin
	  Hashtbl.remove etp.right.pure.affectations pvar; 
	  Hashtbl.remove etp.left.pure.affectations pvar
	end
      else 
	if  ( Hashtbl.mem etp.right.quant_vars lvar_rel ) && (  Hashtbl.mem etp.left.quant_vars lvar_rel )
	then
	  
    else ()
  in
  Hashtbl.iter r1_iterator etp.left.pure.affectations
  
  
