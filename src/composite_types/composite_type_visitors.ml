open Cil_types
open SSL_types
open Ssl
open Types_2_pvars
open Visitors
open Ast_goodies 



(** This visitors aims at a recovering information concerning the
composite types defind in C files, e.g. structs, typedefed structures,
unions etc ... *)

class global_composite_types_visitor (prj : Project.t) = object (self)
  inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())

  val mutable is_computed = false
  val composite_types = Hashtbl.create 97
  val (pvar_names_of_composites_types :(Cil_types.typ , (string , Cil_types.typ) Hashtbl.t )) = Hashtbl.create 97
 


  method vlog_aux ( g : Cil_types.global ) =
    is_computed <-true ;
    let root_pathname = "" in
    begin
      let ptr_set_of_visited_type = Hashtbl.create 97 in
      try
	Types_2_pvars.get_ptr_fields_of_cil_type 
      with 
	| Not_a_composite_type -> ()
    end;
    Hashtbl.add pvar_names_of_composite_types  
      | _ -> DoChildren
 

end;;  

