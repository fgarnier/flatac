open Cil_types
open SSL_types
open Ssl


exception Not_a_composite_type

(**
Given a composite type definition, named or unamed, this function 
stores in the "path_collection" hashtable, the collection of all the subfield/
leaves that have a pointer type.
*)

let rec get_ptr_fields_of_cil_type (t : Cil_types.typ ) (path : string ) (path_collection : (string , Cil_types.typ ) Hashtbl.t) =
  match t with
     TFun(_,_,_,_) | TVoid (_) | TInt(_,_) |
	 TFloat (_,_) | TArray (_,_,_,_)-> ()
   
    | TPtr(ptrtype,_) -> Hashtbl.add path_collection path ptrtype
    | TNamed(tinfo,_) -> get_ptr_fields_of_cil_type t.ttype path path_collection
    | TComp (cinfo , _ , _ ) -> unfold_compinfo cinfo path path_collection
    | TEnum (_,_) -> ()
      

and unfold_compinfo (cinfo : Cil_types.compinfo ) (path : string ) (path_collection : (string , Cil_types.typ ) Hashtbl.t) =

  let cfields_iterator (field_it : Cil_types.fieldinfo ) =
  let current_path = path^"."^field_it.forig_name in
  get_ptr_fields_of_cil_type field_it.ftype current_path path_collection
  in
  List.iter cfields_iterator cinfo.cfields
 


let get_ptr_fields_of_cil_global_type  ( t : Cil_types.global ) (path : string ) (path_collection : (string , Cil_types.typ ) Hashtbl.t) =
  match t with
      GType(tinfo , _ ) ->  get_ptr_fields_of_cil_type 
	tinfo path path_collection
    
    | GCompTag (cinfo, _ ) -> get_ptr_fields_of_cil_type
      cinfo path path_collection

    | GCompTagDecl  (cinfo , _ ) -> get_ptr_fields_of_cil_type 
      cinfo path path_collection
      
    | GEnumTagDecl ( einfo, _ ) ->
       einfo path path_collection
 
    | _ -> raise Not_a_composite_type
              (* Not a composite type, hence needn't to be analysed
		 or stored. *)
      
