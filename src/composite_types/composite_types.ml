open Composite_type_types

exception CType_not_found of string

let pprint_index_of_type_pointer_path (i : index_of_composite_types) =
  let pprint_path_folder (path : string ) (ciltype : Cil_types.typ ) 
      (prefix : string) =
    let prefix = path^":"^( Ast_goodies.pprint_ciltypes ciltype)^"\n"^prefix
    in prefix
  in
  let pprint_type_structure_folder (type_name : c_type_name )  path_table 
      (prefix : string) =
    match type_name  with
        CTypeName (name_type)   ->
	  let ret = prefix^"\n"^name_type^" : " in
	  let ret = Hashtbl.fold pprint_path_folder 
	    path_table ret  in
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


