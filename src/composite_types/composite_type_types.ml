exception Cant_compute_ciltype_name

type c_type_name = CTypeName of string

(* The key corresponds to the type name, and the second realtion contains
the path of all fields which type is a pointer type. *)

type index_of_composite_types = IndexCompositeTypes of  
    (( c_type_name , (string , Cil_types.typ) Hashtbl.t ) Hashtbl.t)
