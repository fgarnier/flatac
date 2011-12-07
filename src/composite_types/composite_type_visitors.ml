open Cil
open Cil_types
open Ssl_types
open Ssl
open Types_2_pvars
open Visitor
open Ast_goodies 
open Composite_type_types


exception Not_visited_exception






let pprint_composite_type_pointer_table (type_name :c_type_name ) ( c_type_tables ) (prefix : string) =
  let path_name_folder (path_name : string ) (t : Cil_types.typ) 
      (prefix_it : string) =
    prefix_it^(path_name^" : "^(Ast_goodies.pprint_ciltypes t)^" \n")
  in
  match type_name with
      CTypeName(tname) ->
	prefix^"Global type "^tname^": \n"^(Hashtbl.fold path_name_folder c_type_tables.pointers "")^"\n"


let pprint_composite_type_int_val_table (type_name :c_type_name ) ( c_type_tables ) (prefix : string) =
  let path_name_folder (path_name : string ) (t : Cil_types.typ) 
      (prefix_it : string) =
    prefix_it^(path_name^" : "^(Ast_goodies.pprint_ciltypes t)^" \n")
  in
  match type_name with
      CTypeName(tname) ->
	prefix^"Global type "^tname^": \n"^(Hashtbl.fold path_name_folder c_type_tables.integer_values "")^"\n"

(** This visitors aims at a recovering information concerning the
composite types defind in C files, e.g. structs, typedefed structures,
unions etc ... *)

class global_composite_types_visitor (prj : Project.t) = object (self)
  inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())

  val mutable is_computed = false
  val var_names_of_composites_types =  
    Composite_types.create_index_of_composite_types ()
 (*   (Hashtbl.create 97 : (( c_type_name , composite_type ) Hashtbl.t)) *)
 


  method private add_entry_to_ctype_table e =
    match var_names_of_composites_types, e with
	(IndexCompositeTypes(table) , (typename , path_collection )  ) ->
	  Hashtbl.add table typename path_collection 


  method vglob_aux ( g : Cil_types.global ) =
    is_computed <-true ;
    begin
      try
	let fields_of_visited_type = 
	  Types_2_pvars.get_fields_of_cil_global_type g in
	self#add_entry_to_ctype_table fields_of_visited_type
	  
	  (*match fields_of_visited_type with
	      (typename , path_collection ) ->
		Hashtbl.add pvar_names_of_composites_types 
		  typename path_collection *)
       
	

      with 
	| Not_a_composite_type -> ()
	| Forward_declaration_not_yet_handled ->()
    end;
    DoChildren
 
 
  method pprint_pvars_of_comp_types () =
    match var_names_of_composites_types with
	IndexCompositeTypes(table) -> 
	   
	  if is_computed then
	    let out_put = "Pointers : \n" in
	    let out_put =
	      Hashtbl.fold pprint_composite_type_pointer_table table out_put 
	    in
	    let out_put = 
	      Hashtbl.fold pprint_composite_type_pointer_table table
		(out_put^"\nInteger values : \n")
	    in
	    out_put					 
	  else raise Not_visited_exception 

  
  method get_index_of_composite () =
    if is_computed then
      var_names_of_composites_types
    else
      raise Not_visited_exception
	
end;;  
