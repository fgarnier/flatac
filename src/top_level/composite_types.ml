open Composite_type_types
open Cil_types

exception CType_not_found of string

let pprint_composite_type_name t =
  match t with 
      CTypeName(name) -> name
    

let pprint_index_of_type_pointer_path (i : index_of_composite_types) =
  let pprint_path_folder (path : string ) (ciltype : Cil_types.typ ) 
      (prefix : string) =
    let prefix = prefix^path^":"^( Ast_goodies.pprint_ciltypes ciltype)^"\n"
    in prefix
  in
  let pprint_type_structure_folder (type_name : c_type_name )  path_table 
      (prefix : string) =
    match type_name  with
        CTypeName (name_type)   ->
	  let ret = prefix^"\n"^name_type^": \n" in
	  let ret = Hashtbl.fold pprint_path_folder 
	    path_table.pointers ret  in
	  ret 
  in
  match i with 
      IndexCompositeTypes (index_table ) ->
	Hashtbl.fold pprint_type_structure_folder index_table ""


      


let create_index_of_composite_types =
  let table = Hashtbl.create 97 in
  IndexCompositeTypes(table)


let copy_index_of_composite_types ( i : index_of_composite_types ) =
  match i with 
       IndexCompositeTypes(table) ->
	 let table_bis = Hashtbl.copy table in
	 IndexCompositeTypes(table_bis)


let get_index_of_pointer_by_type_name (i: index_of_composite_types ) (ctype : c_type_name ) =
  match i with
	IndexCompositeTypes (table) ->
	  begin
	    try
	      Hashtbl.find table ctype
	    with
	      | Not_found -> 
		begin
		  match ctype with 
		      CTypeName(typename) ->
			let msg= Format.sprintf "No description for type [%s] in %s \n" typename (pprint_index_of_type_pointer_path i)
			in 
			raise (CType_not_found(msg))
		end
	  end




(** This function checkes whether a type --composite, named or anonymous--
 is an alias for an integer type.

Must check whether a structure that contains only one integer
type argument can be considered as an integer type.
*)

let rec is_integer_type ( cilt : Cil_types.typ ) =
  match cilt with
      TInt(_,_) | TEnum(_,_) -> 
	let type_name = Ast_goodies.pprint_ciltypes cilt
	in 
	Some(CTypeName(type_name))
    | TFun(_,_,_,_) | TVoid (_)  
    | TPtr(_,_)  | TFloat (_,_) | TArray (_,_,_,_) |
	TBuiltin_va_list (_) | TComp(_) -> None

    | TNamed(tinfo,_) -> 
      begin
	let eval_arg = is_integer_type tinfo.ttype in
	match eval_arg with
	    Some (tval) -> 
	      let alias_c_name =CTypeName (tinfo.torig_name) in   
	      Some(alias_c_name)
		
	  | None -> None
      end
	

