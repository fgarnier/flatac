open Ssl_types
open Ssl
open Ssl_types.SSL_lex
open Ssl_types
open Ssl_valid_abs_dom_types
open Var_validity_types
open Var_validity
open Cil_types
open Composite_type_types
open Composite_types
open New_composite_type_upon_ssl

 
exception Debug_info of string

let create_validity_abstdomain () = 
  let sslf = create_ssl_f () in
  let val_map = new_valid_map () in
  let index_types = Composite_types.create_index_of_composite_types () in
  {
    ssl_part = sslf ;
    validinfos = val_map ;
    composite_types_infos =  index_types ;
  }

let add_alloc_cell_to_validity_abstdomain  (lvar : locvar) (domain :ssl_validity_absdom) =
  Ssl.add_alloc_cell lvar domain.ssl_part 


let create_errorstate_validity_abstdomain () =
  let v = create_validity_abstdomain () in
    set_heap_to_top v.ssl_part; v

let add_atomic_affect_to_validity_abstdomain  (equ : SSL_lex.affect) (domain :ssl_validity_absdom) =
  Ssl.and_atomic_affect equ domain.ssl_part 

(** the fiel ssl_part is mutable and peristant, whereas validinfo isn't.*)
let copy_validity_absdomain (v : ssl_validity_absdom ) =
  let sslf = Ssl.copy v.ssl_part in
  let itable = 
    Composite_types.copy_index_of_composite_types v.composite_types_infos
  in
  let ret_val = {
    ssl_part =  sslf ;
    validinfos = v.validinfos ;
    composite_types_infos = itable ;
  }
  in 
  ret_val


(* This function returns a new domain value with the validity od vinfo updated
to valid. *)
let set_var_validity_in_absdomain  (domain : ssl_validity_absdom) ( vinfo : Cil_types.varinfo )(off : Cil_types.offset option) (valid : var_valid) =
  {
    ssl_part = domain.ssl_part ;
    validinfos = (set_validity_in domain.validinfos vinfo off valid) ;
    composite_types_infos = domain.composite_types_infos ;
  }

let set_pvar_validity_in_absdomain (domain : ssl_validity_absdom) ( pvar : ptvar ) (valid : var_valid) (locality : var_cathegory) =
  let ptvar_name = Ast_goodies.string_of_ptvar pvar in
  let valid_infos = (set_validity_in_by_name domain.validinfos ptvar_name valid locality) in
  {
    ssl_part = domain.ssl_part ;
    validinfos = valid_infos ;
    composite_types_infos = domain.composite_types_infos ;
  }


(* This function subsumes the behaviour of set_var_validity_in_absdomain
and is able to deal with lvalues, which is needed to consider the case
where the considered pointer are subfields of referenced structures, b->out 
for instance.
*)

(*
let set_pvar_of_lval_validity_in_absdomain (domain : ssl_validity_absdom) ( (lv,off): Cil_types.lval ) (valid : var_valid) =
  let pvar_name = Ast_goodies.get_pvar_from_exp_node (LVal(lv,off)) in
  
*)
  
  
    




