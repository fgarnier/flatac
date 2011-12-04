open Cil
open Cil_types
open Ssl_types
open SSL_lex
open Ssl
open Composite_type_types
open Global_mem
open Var_validity_types
open Var_validity

(*
This function computes the impact of the declaration of an element of a composite type, when it is performed on the stack.

Basically, one translate every pointer field of the type into a pointer
variable within the SSL formula.

The sslf parameter is a mutable field and the function has unit for returned
value type.
*)

(* This fonctions states that each pointer fields of a given instance
of a struc type refers to some existentially quantified location variable,*)

let ptrvar_adder_iterator (struct_var_name : string ) sslf mid (field_name : string ) _ =
  (* add exists fresh l  and PVar(struct_var_name.field_name)-> l in sslf.
  *)
  let new_locvar = mid#get_fresh_lvar in
  let new_pvar = PVar (struct_var_name^"."^field_name) in
  Ssl.add_quant_var new_locvar sslf;
  let affectation = Pointsto(new_pvar,new_locvar) in
  Ssl.and_atomic_affect affectation sslf
 

let new_struct_on_stack ( struct_varinfo : varinfo ) (sslf: ssl_formula ) 
    ( typedef_index : index_of_composite_types) mid =
  
  let struct_vname = struct_varinfo.vname in
  let struct_type = struct_varinfo.vtype in
  let type_name_of_var =  
    Typename_of_cil_types.typename_of_ciltype struct_type in
  try
    begin
      match typedef_index with
	  IndexCompositeTypes(index_table) -> 
	    let pvar_collection_path = 
	      Hashtbl.find index_table type_name_of_var  
	    in 
	    let parametrized_iterator = 
	      ptrvar_adder_iterator struct_vname sslf mid in
	    Hashtbl.iter parametrized_iterator pvar_collection_path 
    end
 with
    | Not_found -> raise Not_found




let new_struct_pointer_on_stack ( struct_varinfo : varinfo ) (sslf: ssl_formula ) ( typedef_index : index_of_composite_types) mid =
  
  let ptrvar_adder_iterator_fponstack (struct_var_name : string ) sslf mid (field_name : string ) _ =
    (* add exists fresh l  and PVar(struct_var_name.field_name)-> l in sslf.
    *)
    let new_locvar = mid#get_fresh_lvar in
    let new_pvar = PVar (struct_var_name^"->"^field_name) in
      Ssl.add_quant_var new_locvar sslf;
      let affectation = Pointsto(new_pvar,new_locvar) in
	Ssl.and_atomic_affect affectation sslf
  in
  
  let struct_vname = struct_varinfo.vname in
  let struct_type = struct_varinfo.vtype in
  let type_name_of_var =  
    Typename_of_cil_types.typename_of_ciltype struct_type in
    try
      begin
	match typedef_index with
	    IndexCompositeTypes(index_table) -> 
	      let pvar_collection_path = 
		Hashtbl.find index_table type_name_of_var  
	      in 
	      let parametrized_iterator = 
		ptrvar_adder_iterator_fponstack struct_vname sslf mid in
	      Hashtbl.iter parametrized_iterator pvar_collection_path 
      end
    with
      | Not_found -> raise Not_found
	  

