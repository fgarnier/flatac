open Cil
open Cil_types
open Ssl_types
open Ssl
open Composite_type_types




(**
This function computes the impact of the declaration of an element of a composite type, when it is performed on the stack.

Basically, one translate every pointer field of the type into a pointer
variable within the SSL formula.

The sslf parameter is a mutable field and the function has unit for returned
value type.
*)


(*
let 

let new_struct_on_stack ( struct_varinfo : varinfo ) (ssl : sslf ) 
    ( typedef_index : index_of_composite_types) =
  
  let struct_vname = struct_varinfo.vname in
  let struct_type = struct_varinfo.vtype in
  let type_name_of_var = 
    Typename_of_cil_types.typename_of_ciltype struct_type in
  try
    let pvar_collection_path = Hashtbl.find typedef_index ()  
    in
  with
*)

