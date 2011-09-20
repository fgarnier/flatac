open Var_validity_types
open Cil_types
open Ssl
open Ssl_types
open SSL_lex
open Intermediate_language

exception Neither_intvar_nor_ptvar
exception Unregistered_var
exception Relation_between_vars_out_of_ssl_context

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

(** Checks whether the ssl formula sslf provides a sufficiently detailed
abstraction of the stack/heap to decide whether lvar1 and lvar2 have
the same base address*)

let same_base_meminfo sslf lvar1 lvar2 =
  let lv1_allocated = Ssl.is_allocated  lvar1 sslf in
  let lv2_allocated =  Ssl.is_allocated lvar2 sslf in
    begin
      if (Ssl.cmp_lex_lvar lvar1 lvar2 ) then TruevarValid 
      else if ( lv1_allocated && lv2_allocated ) 
      then FalsevarValid
      else if ( ( lv1_allocated && not lv2_allocated ) 
		|| ( lv2_allocated && not lv1_allocated )) then DKvarValid
      else DKvarValid (* Neither lvar1 nor lvar2 belongs to the space 
		      formula.*)
    end
    

(** This function decides wheter two pointers have the base, or not, or
answers DKvarValid if this question can't be statically decided.*)
let same_base (sslf : ssl_formula)( ptrexp1 : c_ptrexp )( ptrexp2 : c_ptrexp ) =
  let pvar_1 = Validity.base_var_ptrexp  ptrexp1 in
  let pvar_2 = Validity.base_var_ptrexp  ptrexp2 in
  let lvar_1 = Validity.base_ptrexp sslf ptrexp1 in
  let lvar_2 = Validity.base_ptrexp sslf ptrexp2 in
    begin
      if (Ssl_decision.is_rel_in  pvar_1 lvar_1 sslf && 
	    Ssl_decision.is_rel_in pvar_2 lvar_2 sslf )
      then
	same_base_meminfo sslf lvar_1 lvar_2
	
      else raise Relation_between_vars_out_of_ssl_context
    end
     
(** Determines wheter an arithmetic pointer expression evaluates
to TruevarValid, FalsevarValid or DKvarValid.*)

let rec valid_sym_cscal ( loc_map : validity_loc_map ) (sslf : ssl_formula )
 ( scal : c_scal) =
  match scal with
      LiVar(_ , LiIntVar(vname)) -> 
	let entry = validity_of_byname loc_map vname in
	  entry.validity
	    
    | LiConst(_) -> TruevarValid
    | LiSymConst(_) -> TruevarValid
    
    | LiProd ( cscalg, cscald ) ->
	begin
	  let fg = valid_sym_cscal loc_map sslf cscalg in
	  let fd = valid_sym_cscal loc_map sslf cscald in
	    and_sym_validity fg fd
	end
    
    | LiSum (cscalg , cscald ) -> 
	begin
	  let fg = valid_sym_cscal loc_map sslf cscalg in
	  let fd = valid_sym_cscal loc_map sslf cscald in
	    and_sym_validity fg fd
	end	  
    
    | LiMinus (cscalg , cscald ) -> 
	begin
	  let fg = valid_sym_cscal loc_map sslf cscalg in
	  let fd = valid_sym_cscal loc_map sslf cscald in
	    and_sym_validity fg fd
	end
    
    |  LiUnMin (cscalg) -> valid_sym_cscal loc_map sslf cscalg
    
    |  LiMod(cscalg, cscald ) ->
	 begin
	  let fg = valid_sym_cscal loc_map sslf cscalg in
	  let fd = valid_sym_cscal loc_map sslf cscald in
	    and_sym_validity fg  fd 
	 end 
	 
    | LiMinusPP ( ptrexpg , ptrexpd, _ ) ->
	begin
	      let fg = valid_sym_ptrexp loc_map sslf ptrexpg in
	      let fd = valid_sym_ptrexp loc_map sslf ptrexpd in
		and_sym_validity fg fd 
	end
	  
and valid_sym_ptrexp  ( loc_map : validity_loc_map ) (sslf : ssl_formula ) ( ptrexp :  c_ptrexp ) =
  match ptrexp with 
      LiPVar ( _ , LiIntPtr(vname), _ ) ->  
		let entry = validity_of_byname loc_map vname in
		  entry.validity
		    
    | LiPlusPI ( ptrexpprime , cscal , _) -> 
	begin
	  let fg = valid_sym_ptrexp loc_map sslf ptrexpprime in
	  let fd = valid_sym_cscal loc_map sslf cscal in
	    and_sym_validity fg fd 
	end
	  
    | LiIndexPI ( ptrexpprime , cscal , _) -> 
	begin
	  let fg = valid_sym_ptrexp loc_map sslf ptrexpprime in
	  let fd = valid_sym_cscal loc_map sslf cscal in
	    and_sym_validity fg fd 
	end

    |  LiMinusPI ( ptrexpprime , cscal, _) -> 
	begin
	  let fg = valid_sym_ptrexp loc_map  sslf ptrexpprime in
	  let fd = valid_sym_cscal loc_map sslf cscal in
	    and_sym_validity fg fd 
	end






	 
	
	
