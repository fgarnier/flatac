open Var_validity_types
open Cil_types
open Ssl
open Ssl_types
open SSL_lex
open Intermediate_language
(*open Ssl_valid_abs_dom_types*)

(* Ast_infos is here for testing purposes*)
open Ast_goodies

exception Neither_intvar_nor_ptvar
exception Unregistered_var of string
exception Relation_between_vars_out_of_ssl_context

exception Unhandled_lval_subterm

(* Returns the infos from the top most variable descriptor
of a lvalue, concerning its locality information, 
if any, raise an exception in all
other cases *)




let rec loc_of_lvar_lhost lv =
  match lv with
      Var(v) -> 
	begin
	  if v.vglob then GlobalVar
	  else if v.vformal then ParameterVar
	  else LocalVar
	end
and loc_info_of_lval ((lv,_):Cil_types.lval) = 
  match lv with
      Var(v) -> loc_of_lvar_lhost lv 
    | Mem(e) ->
      begin
	match e.enode with
	    Lval(lv,off) -> loc_info_of_lval (lv,off)
	  | AddrOf(lv,off) -> loc_info_of_lval (lv,off)
	  | _ -> raise Unhandled_lval_subterm
      end



let pprint_var_cath ( v : var_cathegory) =
    match v with
	LocalVar -> "local"
      | ParameterVar -> "formal"
      | GlobalVar -> "global"


let pprint_var_valid_entry (v : var_valid_entry)  =
  match v.validity with
    | TruevarValid -> "Valid, "^pprint_var_cath v.location
    | FalsevarValid -> "Not valid, "^pprint_var_cath v.location
    | DKvarValid -> "Unknown validity, "^pprint_var_cath v.location  


let pprint_validity_loc_map ( m : validity_loc_map) =
  
  let print_folder (key : string ) (v :  var_valid_entry ) (prefix : string) =
    Format.sprintf "%s %s %s \n" prefix key (pprint_var_valid_entry v)
  in
  match m with 
       Validlocmap( mapping) -> 
	 Validvarmap.fold print_folder mapping "" 


let cardinal_of_locmap ( m : validity_loc_map) =
  let count_folder _ _ (cnt : int) =
   cnt+1
  in
  match m with 
       Validlocmap( mapping) -> 
	 Validvarmap.fold count_folder mapping 0 

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
	      
	      Not_found -> 
		let error_msg =
		  Format.sprintf "Variable %s does'nt apprear in: Loc map contains : %s \n" varname (pprint_validity_loc_map loc_map) in
		raise (Unregistered_var(error_msg))
		 
	    | e -> raise e  
	end
	  
(**  return a new validity mapping *)

(** If a binding of v.vname already exists in the map, 
then it is replaced by the new validity information. *)

let set_validity_in (loc_map : validity_loc_map ) ( v : Cil_types.varinfo ) 
(off : Cil_types.offset option)  (valid : var_valid) =
  match loc_map with 
      Validlocmap( var_name_map ) ->
	let loc_of_var =  compute_var_cathegory v in
	let new_valid_info = { validity = valid ;
			       location = loc_of_var ;
			     } in
	begin
	  match off with
	      None ->
		let res =
		  Validvarmap.add v.vname new_valid_info var_name_map  in
		Validlocmap(res)
		
	    |  Some(offset_var) ->
	      begin
		let pvar_of_struct_field =  
		  Ast_goodies.get_pvar_from_exp_node 
		    (Lval(Var(v),offset_var)) in
		let name_of_pvar_of_struct_field = get_name_of_ptvar 
		  pvar_of_struct_field in 
		let res =
		  Validvarmap.add 
		    name_of_pvar_of_struct_field new_valid_info var_name_map  
		in
		Validlocmap(res)
	      end
	end

(* This function allows to add to the mapping some infomation concerning
a variable which name is known. The cathegory must be provided during the
call.*)

let set_validity_in_by_name (loc_map : validity_loc_map) (vname : string )
    (valid :  var_valid ) ( cath : var_cathegory ) =
  match loc_map with 
      Validlocmap( var_name_map ) ->
	let new_valid_info = { validity = valid ;
			       location = cath ;
			     } in
	let res =  Validvarmap.add vname new_valid_info var_name_map 
	in
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
		Not_found ->
		  let arg_infos = pprint_attributes v.vattr in
		  let excs = Unregistered_var ((v.vname^"'s attribures :"^arg_infos  ))
		  in 
		    raise excs
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
	begin
	  try
	    let entry = validity_of_byname loc_map vname in
	      entry.validity
	  with
	      Not_found -> 
		let excs = Unregistered_var ((vname^" In valid_cscal")) in
		  raise excs
	end
	    
    | LiConst(_) -> TruevarValid
    | LiSymConst(_) -> TruevarValid
    | LiElemOfCTab(_,_) -> DKvarValid
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

    | LiScalOfAddr ( ptrexp , _ ) -> 
      valid_sym_ptrexp loc_map sslf ptrexp
	  
    | LiScalOfLiBool (ccondition) ->
      valid_sym_boolexp loc_map sslf ccondition

and valid_sym_boolexp (loc_map : validity_loc_map ) (sslf : ssl_formula ) ( bool_expr :  c_bool ) =
  match bool_expr with
    | LiBEq( expg, expd) | LiBNeq( expg,expd) | LiBLt(expg,expd)
    | LiBGt( expg,expd) | LiBLeq(expg,expd) | LiBGeq(expg,expd) 
      ->
      begin
	let valid_eg = valid_sym_cscal loc_map sslf expg in
	let valid_ed = valid_sym_cscal loc_map sslf expd in
	and_sym_validity valid_eg valid_ed 
      end
	
    | LiBPtrEq (expg , expd)  | LiBPtrNeq (expg , expd) 
    | LiBPtrGt (expg , expd)  | LiBPtrLt (expg , expd)
    | LiBPtrGeq (expg , expd) | LiBPtrLeq ( expg , expd)	
      ->
      begin
	let valid_eg = valid_sym_ptrexp loc_map sslf expg in
	let valid_ed = valid_sym_ptrexp loc_map sslf expd in
	and_sym_validity valid_eg valid_ed
      end
	
    | LiBTrue -> TruevarValid
    | LiBFalse -> TruevarValid
      
    | LiBAnd( expg, expd ) 
    | LiBOr ( expg, expd )  ->
      begin
	let valid_eg = valid_sym_boolexp loc_map sslf expg in
	let valid_ed = valid_sym_boolexp loc_map sslf expd in
	and_sym_validity valid_eg valid_ed 
      end


and valid_sym_ptrexp  ( loc_map : validity_loc_map ) (sslf : ssl_formula ) 
    ( ptrexp :  c_ptrexp ) =
  
  match ptrexp with 
      LiPVar ( _ , LiIntPtr(vname), _ ) ->  
	let entry = validity_of_byname loc_map vname in
	entry.validity
		    
    | LiDerefCVar(vname, _ ) -> 
      let entry = validity_of_byname loc_map vname 
      in  entry.validity
      
    | LiDerefCTab(_) -> TruevarValid
		    
      
    | LiStarOfPtr(_) -> DKvarValid 
      
    | LiDerefCPtr(cptr,_) -> valid_sym_ptrexp loc_map sslf cptr

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

    | LiAddrOfScal ( scalexp , _ ) ->
       valid_sym_cscal loc_map sslf scalexp

    | LiBaseAddrOfArray(_,_) -> TruevarValid






	 
	
	
