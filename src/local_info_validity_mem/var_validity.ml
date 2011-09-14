open Var_validity_types
open Cil.types

exception Neither_intvar_nor_ptvar
exception Unregistered_var

let compute_var_cathegory ( v : Cil_types.varinfo ) =
  if v.vinfo.vformal then ParameterVar
  else if v.vinfo.vglobal then GlobalVar
  else LocalVar

let is_intvar_or_ptvar ( v : Cil_types.vinfo ) =
  match v.vtype with
      TInt(_,_) | TPtr(_,_) -> true
    | _ -> false


let validity_of_byname ( loc_map : validity_loc_map ) ( varname : string ) =
  match loc_map with 
      Validlocmap( var_name_map ) ->
	begin
	  try
	    let res = Validvarmap.find varname var_name_map varname in
	      res
	  with
	      Not_found -> raise Not_found
	end
	  
let validity_of  ( loc_map : validity_loc_map ) (v : Cil_types.vinfo ) =
  if not is_intvar_or_ptvar v then
    raise Neither_intvar_nor_ptvar
  else
    match loc_map with
	Validvarmap(var_map) ->
	  begin
	    try
	      let res = Validvarmap.find var_map v.vname  
	      in res.validity
	    with
		Not_found -> raise Underistered_var
	  end


(**  return a new validity mapping *)
(** If a binding of v.vname already exists in the map, then it is replaced
by the new validity information. *)
let set_validity_in (loc_map : validity_loc_map ) ( v : Cil_types.vinfo ) 
  (valid : var_validity) =
  match loc_map with 
      Validlocmap( var_name_map ) ->
	let loc_of_var =  compute_var_cathegory v in
	let new_valid_info = { validity = valid ;
			       loc_of_var ;
			     } in
	let res = Validvarmap.add v.vname var_name_map new_valid_info in
	  res
	 
	
	
