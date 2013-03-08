
open Cil_types
open Ssl_types
open SSL_lex
open Ssl
open Ssl_valid_abs_dom_types (* This type defines 2-uples of ssl_formual and
			    a validity_loc_map*)
open Ssl_valid_abs_dom (*Contains the copy_validity_absdomain function*)
open Nts_types
open Intermediate_language_types
open Intermediate_language
open Cnt_interpret

open Validity_types
open Validity
open Var_validity_types
open Var_validity

open Ast_goodies
open Flatac_ndet_nts_support_types
open Flatac_ndet_nts_support


open Compilation_utils_types






let valid_name_of_var (vname : string ) =
  "validity__"^vname^"_"

let offset_name_of_var (vname : string ) =
  "offset__"^vname^"_"

let make_ntsvars_of_ptrvar (vname : string ) =
  let val_name = valid_name_of_var vname in
  let offset_name = offset_name_of_var vname in
  (NtsGenVar(NtsVar(val_name,NtsIntType),NtsUnPrimed))::(NtsGenVar(NtsVar(offset_name,NtsIntType),NtsUnPrimed)::[])



let valid_sym_cscal_sslv sslv (exp : c_scal ) =
  valid_sym_cscal sslv.validinfos sslv.ssl_part exp

let valid_sym_ptrexp_sslv sslv (ptrexp : c_ptrexp ) =
  valid_sym_ptrexp sslv.validinfos sslv.ssl_part ptrexp

(*
let is_ntisvar_det v =
  match v with
      NtsINdetVar(_) -> false
    | _ -> true
*)





let compile_ntsivar_of_int_cil_lval  (l : Cil_types.lval ) =
  let il_lval = Intermediate_language.get_li_intvar_from_exp_node (Lval(l)) in
  match il_lval with
      LiVar(_,LiIntVar(vname))-> DetAVal(CntGenVar(NtsGenVar(NtsVar(vname,NtsIntType),NtsUnPrimed)))
    | LiFVar(_,LiFloatVar(vname)) -> DetAVal(CntGenVar(NtsGenVar(NtsVar(vname,NtsRealType),NtsUnPrimed)))
    | LiIntStarOfPtr(LiPVar(_,LiIntPtr(param_c_pvar),_),_) -> 
	NDetAVal(CntGenVar(NtsGenVar(NtsVar("pvar_access"^param_c_pvar,NtsIntType),NtsUnPrimed)))
 
    | LiElemOfCTab(_,_) -> 
      NDetAVal(CntGenVar(NtsGenVar(NtsVar("tab_access",NtsIntType),NtsUnPrimed)))
    | _ -> 
	Format.fprintf Ast_goodies.debug_out "Failed to fetch ivar in ";
	Cil.d_lval Ast_goodies.debug_out l;
	Format.fprintf Ast_goodies.debug_out "\n%!";
	let msg = Intermediate_language.scal_to_string il_lval in 

	let msg = "[compile_ntsivar_ov_in_cil_lval] I don't parse this expression : "^msg in
	  raise (Debug_info(msg))
      
  


let compile_cil_exp_2_cnt sslv ( e : Cil_types.exp ) =
  let type_of_e = Cil.typeOf e 
  in 
  let i =
    match type_of_e with
	TPtr(_,_) | TArray(_,_,_,_) ->
	  begin
	    let ptr_exp = cil_expr_2_ptr e in
	      IlPtr(ptr_exp)
	  end
    
      | _ ->
	  begin
	    
	    match e.enode with
	      | Const(CStr(_)) -> 
		  begin
		    let ptr_exp = cil_expr_2_ptr e in
		      IlPtr(ptr_exp) 
		  end
	      
	      | _ ->
		  begin
		    let alias_tname = 
		      Composite_types.is_integer_type type_of_e in
		      match alias_tname with 
			  Some(_) ->
			    begin
			      let scal_exp = cil_expr_2_scalar e in
				IlScal(scal_exp)
			    end
			| None ->
			  begin
			    let alias_name = Composite_types.is_float_type
			      type_of_e 
			    in
			    match alias_name with 
				Some(_) ->
				  begin
				    let scal_exp = cil_expr_2_scalar e in
				    IlScal(scal_exp)
				  end
			      |  None -> 
				raise (Debug_info ("compile_cil_exp_2_cnt : I have an argume which type is neither a scalar value (integer or floating point) nor a pointer/array"))
			  end
		  end
	  end
  in
    match i with
	IlPtr(ep) -> interpret_c_ptrexp_to_cnt sslv.ssl_part ep
      | IlScal(ep) -> interpret_c_scal_to_cnt sslv.ssl_part ep
	  

let compile_sym_validity_to_cnt v =
  match v with 
      DKvarValid -> CntGenCst(CntGenICst((Big_int.big_int_of_int (-1))),NtsIntType)
    |  FalsevarValid -> CntGenCst(CntGenICst((Big_int.big_int_of_int 0)),NtsIntType)
    |  TruevarValid -> CntGenCst(CntGenICst((Big_int.big_int_of_int 1)),NtsIntType)


(* Takes as input a cil value whose type is either integer of
float/double and returns a Nts variable or expression.*)

let compile_cil_scalar_argument_value sslv (e : Cil_types.exp ) =
  let type_of_e = Cil.typeOf e in
  let alias_tname = 
    Composite_types.is_integer_type type_of_e in
  match alias_tname with 
      Some(_) ->
	begin
	  let cscal_eval = cil_expr_2_scalar e in
	  let nts_scal_exp = interpret_c_scal_to_cnt sslv.ssl_part 
	    cscal_eval in
	  let valid = Var_validity.valid_sym_cscal sslv.validinfos 
	    sslv.ssl_part cscal_eval in
	  IlScalArg(
	    {
	      expr = nts_scal_exp ;
	      validity_of_exp = DetAVal((compile_sym_validity_to_cnt valid)) ;  
	    })
	end
    |  None ->
      begin
	let alias_tname = 
	  Composite_types.is_float_type type_of_e in
	match alias_tname with 
	    Some(_) ->
	      begin
		let cscal_eval = cil_expr_2_scalar e in
		let nts_scal_exp = interpret_c_scal_to_cnt sslv.ssl_part 
		  cscal_eval in
		let valid = Var_validity.valid_sym_cscal sslv.validinfos 
		  sslv.ssl_part cscal_eval in
		IlScalArg(
		  {
		    expr = nts_scal_exp ;
		    validity_of_exp = 
		      DetAVal((compile_sym_validity_to_cnt valid)) ;  
		  })
	      end
	  | None -> 
	    raise (Debug_info ("[compile_cil_scalar_argument_value ]I got an argument whose type is neither integer nor a float."))
      end
	


let compile_cil_fun_argexpr_2_cnt sslv (e : Cil_types.exp ) =
  let type_of_e = Cil.typeOf e 
  in 
  match type_of_e with
      TPtr(_,_) | TArray(_,_,_,_) -> 
	begin
	  let ptr_exp_t = cil_expr_2_ptr e in
	  let nts_ptr_exp = interpret_c_ptrexp_to_cnt sslv.ssl_part 
	    ptr_exp_t in
	  let ptr_base = Validity.base_ptrexp sslv.ssl_part ptr_exp_t in
	  let validkind = Var_validity.valid_sym_ptrexp sslv.validinfos
	    sslv.ssl_part ptr_exp_t  
	  in
	  IlPtrArg({
	    base_of_exp = ptr_base  ;
	    offset_of_exp = nts_ptr_exp ;
	    validity_of_ptr_exp = DetAVal(compile_sym_validity_to_cnt validkind);
	  })
	end
    | _ ->
      begin
        compile_cil_scalar_argument_value sslv e
	(*let alias_tname = 
	  Composite_types.is_integer_type type_of_e in
	match alias_tname with 
	    Some(_) ->
	      begin
		let cscal_eval = cil_expr_2_scalar e in
		let nts_scal_exp = interpret_c_scal_to_cnt sslv.ssl_part 
		  cscal_eval in
		let valid = Var_validity.valid_sym_cscal sslv.validinfos 
		  sslv.ssl_part cscal_eval in
		IlScalArg(
		  {
		    expr = nts_scal_exp ;
		    validity_of_exp = compile_sym_validity_to_cnt valid ;  
		  })
	      end
	  | None ->

	    begin
	     let alias_tname = 
	       Composite_types.is_float_type type_of_e in 
	     match alias_tname 
	       Some(_) ->
		 
	    end
	    Format.printf "Failed to compile this argument : \n";
	    Cil.d_exp Ast_goodies.debug_out e; Format.fprintf Ast_goodies.debug_out "\n%!";
	    raise (Debug_info ("compile_cil_fun_argexpr_2_cnt : I have an argume which type is neither an integer value nor a pointer/array"))
	*)
      end




let compile_param_list_2_cnt_list sslv ( lparam : Cil_types.exp list) =
  let arg_compile_folder out_list argument =
    let compile_arg =  compile_cil_fun_argexpr_2_cnt sslv argument
    in 
    compile_arg::out_list
  in
  let inv_order_list = List.fold_left arg_compile_folder [] lparam in
  List.rev inv_order_list
  


let rec compile_cil_array_2_cnt sslv (name : string) (vtype_arg : Cil_types.typ) =
  
  let rec translate_reftab_recursor vtype =
    match vtype with
      | TArray(t,None,_,_)->
	  begin
	    match t with
		TArray(_,_,_,_) ->
		  begin
		    let inner_tab = translate_reftab_recursor t in
		    RefMulDimArray(inner_tab)
		  end
	      | _ ->
		begin
		  let inner_type = Cnt_interpret.ciltype_2_ntstype t in
		  RefBasicTypeArray(inner_type)
		end
	  end   
      | _ -> assert false	
  in
  let rec translate_recursor vtype =
      match vtype with
	  TArray(t,Some(exp),_,_)->
	    begin
	      let size = compile_cil_exp_2_cnt sslv exp in
	      let size = Flatac_ndet_nts_support.arithm_value_of_ndsupport_or_fails size 
	      in
	      match t with
		  TArray(_,_,_,_) -> 
		    begin
		      let inner_tab = translate_recursor t in
		      FixedSizeNtsArray(FixedSizeMulDimNtsArray(size,inner_tab))
		    end
		| _ ->
		  begin
		    let inner_type = ciltype_2_ntstype t in
		    FixedSizeNtsArray(FixedSizeBasicTypeNtsArray(size,inner_type))
		  end
	    end

	| TArray(t,None,_,_)->
	  begin
	    let inner_type = translate_reftab_recursor vtype
	    in
	    RefNtsArray(inner_type)
	  end   
	      
	| _-> raise Not_Array_type
  in
  let array_type = translate_recursor vtype_arg in
  let base_type = Ast_goodies.base_type_of_muldim_array_type vtype_arg in
  let cmpilbase_type = Cnt_interpret.ciltype_2_ntstype base_type in
  NtsArrayVar(name,array_type,cmpilbase_type)




let  compile_sizeof_array_type sslv ( t : Cil_types.typ ) =

  let rec array_size_recursor (siz_upper_dim : nts_genrel_arithm_exp option)
      (trec : Cil_types.typ ) = 
    
    match siz_upper_dim, trec with 
	(Some(prev_size), TArray(tin, Some(exp) ,_ ,_ )) ->
	  let size_curr_dim = compile_cil_exp_2_cnt sslv exp in
	  let size_curr_dim = 
	    Flatac_ndet_nts_support.arithm_value_of_ndsupport_or_fails 
	      size_curr_dim 
	  in
	  let size_curr_dim_prev_size = 
	    Some(CntGenArithmBOp(CntGenProd,prev_size,size_curr_dim,NtsIntType)) 
	  in
	  array_size_recursor size_curr_dim_prev_size tin 
	 
      | (None,TArray (tin, Some(exp),_,_ )) ->
	let size_curr_dim = compile_cil_exp_2_cnt sslv exp in
	let size_curr_dim = 
	  Flatac_ndet_nts_support.arithm_value_of_ndsupport_or_fails 
	  size_curr_dim
	in array_size_recursor (Some(size_curr_dim)) tin 
	
     
      | (None,_) -> 
	Cnt_interpret.interpret_ciltypes_size trec

      | (Some(size),_) ->
	let size_t = Cnt_interpret.interpret_ciltypes_size trec in
	CntGenArithmBOp(CntGenProd,size, size_t,NtsIntType)
  in
   array_size_recursor None t
  
