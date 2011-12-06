exception Cant_compute_ciltype_name

type c_type_name = CTypeName of string

(* The key corresponds to the type name, and the second realtion contains
the path of all fields which type is a pointer type. *)

type composite_type = {
      pointers :(string , Cil_types.typ) Hashtbl.t ;
      integer_values : (string , Cil_types.typ) Hashtbl.t ;
}

type index_of_composite_types = IndexCompositeTypes of  
    (( c_type_name , composite_type ) Hashtbl.t)

type integer_alias_type = IntTypeDefAs of c_type_name * Cil_types.typ

