open Ssl_types
open Ssl
open SSL_lex
open Ssl_valid_abs_dom_types
open Var_validity_types
open Var_validity
open Cil_types
 

let create_validity_abstdomain = 
  let sslf = create_ssl_f () in
  let val_map = new_valid_map () in
  {
    ssl_part = sslf ;
    validinfos = val_map ;
  }


let add_atomic_affect_to_validity_abstdomain  (equ : SSL_lex.affect) (domain :ssl_validity_absdom) =
  Ssl.and_atomic_affect equ domain.ssl_part 

(** the fiel ssl_part is mutable and peristant, whereas validinfo isn't.*)
let copy_validity_absdomain (v : ssl_validity_absdom ) =
  let sslf = Ssl.copy v.ssl_part in
  let ret_val = {
    ssl_part =  sslf;
    validinfos = v.validinfos ;
  }
  in 
  ret_val


(* This function returns a new domain value with the validity od vinfo updated
to valid. *)
let set_var_validity_in_absdomain  (domain : ssl_validity_absdom) ( vinfo : Cil_types.varinfo )(off : Cil_types.offset option) (valid : var_valid) =
  {
    ssl_part = domain.ssl_part ;
    validinfos = (set_validity_in domain.validinfos vinfo off valid); 
  }


(* Registers the set of local variables in the validity table*)
let register_slocals mid (funinfos : Cil_types.fundec ) ( absdom_param : ssl_validity_absdom ) =
   let slocals_register_folder absdom sform =
    match sform.vtype with 
      | TPtr(_,_) ->
	begin
	  let fresh_lvar = mid#get_fresh_lvar in
	  let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
	  add_atomic_affect_to_validity_abstdomain atom_aff absdom;
	  absdom
	end 
      | _ -> absdom
  in
let absdom_param = List.fold_left slocals_register_folder absdom_param funinfos.slocals in
  List.fold_right ( fun vinf_slocal absdom -> set_var_validity_in_absdomain absdom vinf_slocal None FalsevarValid ) (funinfos.slocals) absdom_param



(* Registers the set of local variables in the validity table.
For each pointer variable x, one need to associate a location var

l such that x->l in the ssl part.
*)
    
let register_sformals mid (funinfos : Cil_types.fundec ) 
    ( absdom_param : ssl_validity_absdom ) =
  let formal_register_folder absdom sform =
    match sform.vtype with 
      | TPtr(_,_) ->
	begin
	  let fresh_lvar = mid#get_fresh_lvar in
	  let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
	  add_atomic_affect_to_validity_abstdomain atom_aff absdom;
	  absdom
	end 
      | _ -> absdom
  in
let absdom_param = List.fold_left formal_register_folder absdom_param funinfos.sformals in
  List.fold_right ( fun vinf_slocal absdom -> set_var_validity_in_absdomain absdom vinf_slocal None  DKvarValid ) (funinfos.sformals) absdom_param
