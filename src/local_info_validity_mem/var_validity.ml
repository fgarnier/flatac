open Var_validity_types
open Cil_types

exception Neither_intvar_nor_ptvar
exception Unregistered_var

let compute_var_cathegory ( v : Cil_types.varinfo ) =
  if v.vformal then ParameterVar
  else if v.vglob then GlobalVar
  else LocalVar

let is_intvar_or_ptvar ( v : Cil_types.varinfo ) =
  match v.vtype with
      TInt(_,_) | TPtr(_,_) -> true
    | _ -> false

(** Creates an empty table*)
let new_valid_map _ =
  let map_content = Validvarmap.empty in
    Validlocmap( map_content )

  

let validity_of_byname ( loc_map : validity_loc_map ) ( varname : string ) =
  match loc_map with 
      Validlocmap( var_name_map ) ->
	begin
	  try
	    let res = Validvarmap.find varname var_name_map  in
	      res
	  with
	      Not_found -> raise Not_found
	end
	  
(**  return a new validity mapping *)
(** If a binding of v.vname already exists in the map, then it is replaced
by the new validity information. *)
let set_validity_in (loc_map : validity_loc_map ) ( v : Cil_types.varinfo ) 
  (valid : var_valid) =
  match loc_map with 
      Validlocmap( var_name_map ) ->
	let loc_of_var =  compute_var_cathegory v in
	let new_valid_info = { validity = valid ;
			       location = loc_of_var ;
			     } in
	let res = Validvarmap.add v.vname new_valid_info var_name_map  in
	  Validlocmap(res)


let validity_of  ( loc_map : validity_loc_map ) (v : Cil_types.varinfo ) =
  if not (is_intvar_or_ptvar v) then
    raise Neither_intvar_nor_ptvar
  else
    match loc_map with
	Validlocmap(var_map) ->
	  begin
	    try
	      let res = Validvarmap.find v.vname var_map 
	      in res.validity
	    with
		Not_found -> raise Unregistered_var
	  end


let and_sym_validity (vg : var_valid )( vd : var_valid) =
  match vg , vd with
      ( TruevarValid, TruevarValid) -> TruevarValid
    | (FalsevarValid,_) -> FalsevarValid
    | (_,FalsevarValid) -> FalsevarValid
    | (_,_) -> DKvarValid (* In this case, we have a TruevarValid and a
			     DKvarValid or two DKvarvalid, hence a Don't 
			     know*)


(** Determines wheter an arithmetic pointer expression evaluates
to TruevarValid, FalsevarValid or DKvarValid.*)

let rec valid_sym_cscal ( loc_map : validity_loc_map ) (sslf : ssl_formula )
 ( scal : c_scal) =
  match scal with
      LiVar(_ , LiIntVar(vname)) -> validity_of_byname loc_map vname
    | LiConst(_) -> TruevarValid
    | LiSymConst(_) -> TruevarValid
    
    | LiProd ( cscalg, cscald ) ->
	begin
	  let fg = valid_sym_cscal loc_map sslf cscalg in
	  let fd = valid_sym_cscal loc_mapp sslf cscald in
	    and_sym_valid fg fd
	end
    
    | LiSum (cscalg , cscald ) -> 
	begin
	  let fg = valid_sym_cscal loc_map sslf cscalg in
	  let fd = valid_sym_cscal loc_map sslf cscald in
	    and_sym_valid fg fd
	end	  
    
    | LiMinus (cscalg , cscald ) -> 
	begin
	  let fg = valid_sym_cscal loc_map sslf cscalg in
	  let fd = valid_sym_cscal loc_map sslf cscald in
	    and_sym_valid fg fd
	end
    
    |  LiUnMin (cscalg) -> valid_sym_cscal loc_map sslf cscalg
    
    |  LiMod(cscalg, cscald ) ->
	 begin
	  let fg = valid_sym_cscal loc_map sslf cscalg in
	  let fd = valid_sym_cscal loc_map sslf cscald in
	    and_sym_valid fg  fd 
	 end 
	 
    | LiMinusPP ( ptrexpg , ptrexpd, _ ) ->
	begin
	  if not ( (base_ptrexp sslf ptrexpg)==(base_ptrexp sslf ptrexpd) )
	  then FalseValid
	  else 
	    begin
	      let fg = valid_ptrexp sslf ptrexpg in
	      let fd = valid_ptrexp sslf ptrexpd in
		and_valid fg fd 
	    end
	end
	  
and valid_ptrexp (sslf : ssl_formula ) ( ptrexp :  c_ptrexp ) =
  match ptrexp with 
      LiPVar ( _ , LiIntPtr(vname), _ ) ->  (PtValid(vname)) 
    | LiPlusPI ( ptrexpprime , cscal , _) -> 
	begin
	  let fg = valid_ptrexp sslf ptrexpprime in
	  let fd = valid_cscal sslf cscal in
	    and_valid fg fd 
	end
	  
    | LiIndexPI ( ptrexpprime , cscal , _) -> 
	begin
	  let fg = valid_ptrexp sslf ptrexpprime in
	  let fd = valid_cscal sslf cscal in
	    and_valid fg fd 
	end

    |  LiMinusPI ( ptrexpprime , cscal, _) -> 
	begin
	  let fg = valid_ptrexp sslf ptrexpprime in
	  let fd = valid_cscal sslf cscal in
	    and_valid fg fd 
	end






	 
	
	
