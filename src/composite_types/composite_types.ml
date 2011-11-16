open Composite_type_types



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
	 Hashtbl.find table ctype



