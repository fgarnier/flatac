open Cil_types
open SSL_types
open Ssl
open Types_2_pvars
open Visitors


(** This visitors aims at a recovering information concerning the
composite types defind in C files, e.g. structs, typedefed structures,
unions etc ... *)

class global_composite_types_visitor (prj : Project.t) = object (self)
  inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())

  val mutable is_computed = false
  val composite_types = Hashtbl.create 97
  val (pointers_of_composites_types :(Cil_types.typ , (string , Cil_types.typ) Hashtbl.t )) = Hashtbl.create 97
 
  method vlog_aux ( g : Cil_types.global ) =
    is_computed <-true ;
    let root_pathname = "" in
    let 
    Types_2_pvars.get_ptr_field
	    
      | _ -> DoChildren
 

end;;  

