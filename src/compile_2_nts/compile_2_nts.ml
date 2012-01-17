
open Cil_types
open Ssl_types
open SSL_lex
open Ssl
open Ssl_valid_abs_dom_types (* This type defines 2-uples of ssl_formual and
			    a validity_loc_map*)
open Ssl_valid_abs_dom (*Contains the copy_validity_absdomain function*)
open Nts_types
open Intermediate_language
open Cnt_interpret

open Validity_types
open Validity
open Var_validity_types
open Var_validity

open Ast_goodies



let valid_sym_cscal_sslv sslv (exp : c_scal ) =
  valid_sym_cscal sslv.validinfos sslv.ssl_part exp

let valid_sym_ptrexp_sslv sslv (ptrexp : c_ptrexp ) =
  valid_sym_ptrexp sslv.validinfos sslv.ssl_part ptrexp


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
			    raise (Debug_info ("compile_cil_exp_2_cnt : I have an argume which type is neither an integer value nor a pointer/array"))
		  end
	  end
  in
    match i with
	IlPtr(ep) -> interpret_c_ptrexp_to_cnt sslv.ssl_part ep
      | IlScal(ep) -> interpret_c_scal_to_cnt sslv.ssl_part ep
	  

let compile_sym_validity_to_cnt v =
  match v with 
      DKvarValid -> CntCst(-1)
    |  FalsevarValid -> CntCst(0)
    |  TruevarValid -> CntCst(1)

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
	    validity_of_ptr_exp = compile_sym_validity_to_cnt validkind ;
	  })
	end
    | _ ->
      begin
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
		    validity_of_exp = compile_sym_validity_to_cnt valid ;  
		  })
	      end
	  | None ->
	    raise (Debug_info ("compile_cil_fun_argexpr_2_cnt : I have an argume which type is neither an integer value nor a pointer/array"))
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
  in
  let rec translate_recursor vtype =
      match vtype with
	  TArray(t,Some(exp),_,_)->
	    begin
	      let size = compile_cil_exp_2_cnt sslv exp in
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
  NtsArrayVar(name,array_type)
