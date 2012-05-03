(** This file  contains aims encodes the semantic of some C-functions
over the SSL formula. Those one are currently hard coded, which is
not satisfactory in a long term view.

Question & remarks : Address to florent dot garnier AT imag dot fr.
*)
open Format
open Cil_types
open Ssl_types
open SSL_lex
open Ssl
open Ssl_decision
open Global_mem
open List
open Self
open Int64
open Ssl_normalization
open Ssl_pprinters
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
open Compile_2_nts
open Guard_of_mem_acces

exception No_pvar_in_free_expression
exception Wrong_parameter_type_in_free
exception Debug_information of string
exception Debug_info of string 
exception Assert_fail_exception 
exception LvalureNotaVariable


(* This function is called by decl_and_affect_cst_char_star *)
let set_valid_of_lhs_for_cstcharaffect (lv,off) sslv =
  match lv with
      Var(v) ->
	(Ssl_valid_abs_dom.set_var_validity_in_absdomain 
	   sslv v (Some(off)) TruevarValid)
	  
    | _ -> raise (Debug_info("[In decl_and_affect_cst_char_star ] I expect a var of type char* as lhs"))
      
let rec is_expr_cst_char (cst_char_star : Cil_types.exp) =
 (* Format.fprintf Ast_goodies.debug_out "[is_expr_cst_char :] ";
  Cil.d_exp  Ast_goodies.debug_out cst_char_star;
  let etype = Cil.typeOf cst_char_star in
   Cil.d_type Ast_goodies.debug_out etype ;
 *)
  match cst_char_star.enode with 
      Const(CStr(s)) -> true
    | CastE(_,e) -> is_expr_cst_char e 
    | _ -> false


let rec get_cst_char_root_exp (cst_char_star : Cil_types.exp) =
  match cst_char_star.enode with 
      Const(CStr(s)) -> cst_char_star
    |  CastE(_,e) -> get_cst_char_root_exp e
    | _ -> raise (Debug_info("This term contains no leaf that has
const char type."))
    


let decl_and_affect_cst_char_star 
    ((lv, off ) : Cil_types.lval)
    (cst_char_star : Cil_types.exp) 
    (mid : global_mem_manager)
    (sslv : ssl_validity_absdom) =
  
  (*If the const char has been caseted, we try to get the basic
  definition of the constant char.*)
  let cst_char_root = get_cst_char_root_exp cst_char_star in
  match cst_char_root.enode with 
      Const(CStr(s)) -> 
	begin
	  let sslv = copy_validity_absdomain sslv in
	  let sizeof_string = String.length s in
	  let sizeof_string = My_bigint.of_int sizeof_string in 
	  let mem_size = 
	    CntProd(CntCst(sizeof_string),CntCst(My_bigint.one)) in 
	        (*unit_big_int ecodes one, i.e. the size of a char.*)
	  let lvar = mid#lvar_from_constant_char (Some(mem_size)) in
	  let pvar = Ast_goodies.get_pvar_from_exp_node  (Lval(lv,off)) in
	  (* One set pvar to Valid in sslv*)
	  let sslv = set_valid_of_lhs_for_cstcharaffect (lv,off) sslv in
	  (* One adds the relation between pvar and lvar in sslv*)
	  let affect = Pointsto(pvar,lvar) in
	  Ssl.change_affect_var affect sslv.ssl_part;
	  Ssl.add_alloc_cell lvar sslv.ssl_part;
	  (*Ssl_valid_abs_dom.add_atomic_affect_to_validity_abstdomain affect
	    sslv;*)
	  let update_var_list = Guard_of_mem_acces.make_size_locvar lvar
	    mid  mem_size in
	  ((sslv,update_var_list)::[])
	 
	end
    | _ -> raise (Debug_info("In decl_and_affect_cst_char_star, the expression has not a const char type "))
  
  

(**
This function progates the validity to an expression to the lvalue
when the latter is an integer variable
*)

let affect_int_val_upon_sslv ((lv , off) : Cil_types.lval) (expr : Cil_types.exp) 
    (sslv : ssl_validity_absdom ) =

  
  
	Self.debug ~level:0 " Entering affect_int_val_upon_sslv ";
	let scal_of_exp = cil_expr_2_scalar expr in
	let type_of_lval = Cil.typeOfLval (lv , off ) in
	let validity_of_rval = valid_sym_cscal sslv.validinfos sslv.ssl_part 
	  scal_of_exp in
	let ret_absdomain = copy_validity_absdomain sslv in
	let (cnt_affect_list , ret_absdomain ) =(
	  match ( Composite_types.is_integer_type type_of_lval) with 
	      Some(_) -> 
		begin
		  let nts_lvar = Compile_2_nts.compile_ntsivar_of_int_cil_lval 
		    (lv , off) in
		  let vname = Nts.nts_pprint_nts_var nts_lvar in 
		  let locality_of_lval = Var_validity.loc_info_of_lval (lv,off)
		  in
		  
		  let ret_absdomain =
		    set_var_validity_by_name ret_absdomain vname 
		      validity_of_rval locality_of_lval in
		  let c_scal_exp = cil_expr_2_scalar expr in 
		  let cnt_expr = interpret_c_scal_to_cnt sslv.ssl_part
		    c_scal_exp in
		  
		  let cnt_affect = CntAffect(nts_lvar,cnt_expr) in
		  (cnt_affect::[],ret_absdomain)
		end
	    | None -> ([], ret_absdomain)
	    | _ ->
	      ([], ret_absdomain)
	)
	in
	
  (***********Transition for valid memory access in expr parameter *******************)
	let access_cond_of_expr = cnt_guard_of_mem_access sslv expr in
	let access_cond_of_lval = cnt_guard_of_mem_access_enode sslv (Lval(lv , off))
	in
	let global_succs_guard = CntBAnd(access_cond_of_lval,access_cond_of_expr) in
	let success_guard = CntGuard(global_succs_guard) in
	let success_transition = (ret_absdomain , success_guard::(cnt_affect_list)) in
    (******** Transition for invalid memory access in expr parameter ****************)

	let failure_absdom = create_validity_abstdomain () in
	set_heap_to_top failure_absdom.ssl_part;
	let failure_guard = CntGuard(CntNot(global_succs_guard)) in
	let fail_trans = (failure_absdom,failure_guard ::[] )  in
	fail_trans::(success_transition::[])
	  
  

(**
Impact and guards of a pointer affectation upon the SSL domain.
*)


let affect_ptr_upon_sslv ( (lv,off) : Cil_types.lval)  (expr : Cil_types.exp) mid (sslv : ssl_validity_absdom ) =
  Self.debug ~level:0 "Im am in affect_ptr_upon_sslv \n";
  let sslv =  copy_validity_absdomain sslv in
  (*let v = get_pvarinfo_of_left_value (lv,off) in*)
  (*let varname =  get_pvarname_of_left_value lv in*)
    (** One need to check that lv is a Var which type is TPtr(_,_)*)
  try
    
    Format.printf "[affect_ptr_upon_sslv: expression :  ?=%s \n]"  (pprint_cil_exp expr);
    if (is_expr_cst_char expr ) 
    then
      (* In the case if expr is a cst char, one need to represent the
      string in the memory model, and we branch here*)
       decl_and_affect_cst_char_star (lv,off) expr mid sslv
    else if (is_lval_of_mem_access (lv,off)) 
    then
      begin
	(*In this case, a memory access is performed on the lhs by a memory
	access. The modification of the content of the references address
	does not impact the abstract value domain *)
	let cnd_of_rhs_access = Guard_of_mem_acces.cnt_guard_of_mem_access
	  sslv expr in
	let guard_of_rhs_mem_access = CntGuard(cnd_of_rhs_access) in
	let negate_of_cnd_of_rhs_access = Nts.negate_cntbool_shallow cnd_of_rhs_access in
	let guard_of_broken_mem_access = CntGuard(negate_of_cnd_of_rhs_access) in
	let broken_mem = Ssl_valid_abs_dom.create_errorstate_validity_abstdomain () in
	let transit_list = (sslv,guard_of_rhs_mem_access::[])::[] in
	let transit_list = (broken_mem,guard_of_broken_mem_access::[])::transit_list
	in
	transit_list
      end
    else
      (*Here is the general case*)
      let pvar_left = Ast_goodies.get_pvar_from_exp_node  (Lval(lv,off)) in
      let pvar_right = get_pvar_from_exp expr in
      let sslv = copy_validity_absdomain sslv in
      let lvar_right = get_ptr_affectation sslv.ssl_part pvar_right 
      in
      begin
	match lvar_right with
	    LVar("") ->  Ssl.and_atomic_ptnil (Pointsnil(pvar_right)) sslv.ssl_part
	  | LVar(_) -> Ssl.change_affect_var (Pointsto(pvar_left,lvar_right)) sslv.ssl_part
      end;
      let param_cscal = Intermediate_language.cil_expr_2_ptr expr in
      let offset_of_pexpr = interpret_c_ptrexp_to_cnt sslv.ssl_part param_cscal in
      let offset_var_of_pvar = offset_ntsivar_of_pvar pvar_left in
      let affect_off = CntAffect(offset_var_of_pvar,offset_of_pexpr) in
      let affect_validity_of_pvar = valid_sym_ptrexp sslv.validinfos 
	sslv.ssl_part param_cscal in
      let valid_rhs_var = make_validity_varpvar pvar_right in
      let valid_lhs_var =  make_validity_varpvar pvar_left in
      let cnd_of_lhs_access = Guard_of_mem_acces.cnt_guard_of_mem_access_enode 
	sslv (Lval(lv,off)) in
      let cnd_of_rhs_access = Guard_of_mem_acces.cnt_guard_of_mem_access 
	sslv expr in
      let guard_of_good_mem_access = CntGuard(CntBAnd(cnd_of_rhs_access,cnd_of_lhs_access)) in
      let copy_valaff_tolhs = CntAffect( valid_lhs_var, CntVar(valid_rhs_var)) in
      let pvar_loc_info = Var_validity.loc_info_of_lval (lv, off) in
      let sslv_new = set_pvar_validity_in_absdomain 
	sslv pvar_left affect_validity_of_pvar pvar_loc_info
      in
      let sslv_mem_broken = create_errorstate_validity_abstdomain () in
      let cnd_of_mem_broken = 
	Nts.negate_cntbool_shallow (CntBAnd(cnd_of_rhs_access,cnd_of_lhs_access)) in
      let guard_of_mem_broken =
	CntGuard( cnd_of_mem_broken)
      in
      let trans_mem_broken = (sslv_mem_broken, guard_of_mem_broken::[]) in
      let ret_trans = ((sslv_new,copy_valaff_tolhs::(affect_off::(guard_of_good_mem_access::[])))::[]) in
      trans_mem_broken::ret_trans
      
  with
      
    | Loc_is_nil -> 
      begin 
	let pvar_left =  Ast_goodies.get_pvar_from_exp_node  (Lval(lv,off)) in
	let loc_info =   Var_validity.loc_info_of_lval (lv, off) in
	and_atomic_ptnil (Pointsnil(pvar_left)) sslv.ssl_part;
	let new_sslv = set_pvar_validity_in_absdomain sslv pvar_left FalsevarValid loc_info in
	(new_sslv , [])::[]
      end
	
(*| Contains_no_pvar ->
      let exprpvarless = pprint_cil_exp expr in
      Format.printf "No pvar found in %s \n" exprpvarless;
      raise Contains_no_pvar
    *)	  


	  
(** This function modifies the sslf formula that abstracts the current
    heap and stack when a call to malloc is performed.*)
let malloc_upon_ssl  ( lhs : Cil_types.lval option ) ( mid : global_mem_manager )  (sslf : ssl_formula ) =
  match lhs with Some ((lv,off)) ->
    let lvar = mid#lvar_from_malloc () in
    let pvar = get_pvar_from_exp_node (Lval(lv,off)) in
    let affect = (Pointsto (pvar,lvar)) in
      Ssl.add_quant_var lvar sslf;
      Ssl.change_affect_var affect sslf;
      Ssl.add_alloc_cell lvar sslf
	(*TODO : Need to set the left value pointer var validity 
	  as TruevarValid*)
    | None ->
	let lvar = mid#lvar_from_malloc () in
	  Ssl.add_quant_var lvar sslf;
	  Ssl.add_alloc_cell lvar sslf
	    
(** Effect of a free(x),  where x is a pointer variable, on an ssl
formula.*)
let free_upon_sslv (pvar : ptvar)(sslv : ssl_validity_absdom ) =
  	Self.debug ~level:0 "Entering free_upon_sslv";
  let sslv = copy_validity_absdomain sslv in
  try
    let lvar = get_ptr_affectation sslv.ssl_part pvar in
    if not (space_contains_locvar lvar  sslv.ssl_part.space )
    then 
      begin 
	set_heap_to_top sslv.ssl_part;
	(sslv,[]) :: [] (* The only transition enabled leads to 
			Top_heap*)
      end
    else
      begin
	try_remove_segment lvar sslv.ssl_part; (* If the previous operation
					       succeeds, we have to consider
					       two possible transitions : Case
					       of the offset of the address 
					       is zero and the error prone 
						  non-zero case.*)
	let offset_of_pvar = Cnt_interpret.offset_cnt_of_pvar pvar in
	let eq_zero_guard = CntBool(CntEq,offset_of_pvar,CntCst(My_bigint.zero)) in
	let non_zero_guard =   CntBool(CntNeq,offset_of_pvar,CntCst(My_bigint.zero)) in
	let fucked_up_case =  create_validity_abstdomain () in
	Ssl.set_heap_to_top fucked_up_case.ssl_part;
	let trans_list= (fucked_up_case, (CntGuard(non_zero_guard))::[])::[] 
	in
	let trans_list = (sslv,(CntGuard(eq_zero_guard))::[])::trans_list in
	trans_list
      end
  with
      Not_found -> 
	begin 
	  set_heap_to_top sslv.ssl_part; (* Here we get that pvar
					does not belong to the 
					the affectation table*)

	  (sslv,[])::[] (*sslv is supposed to be a copy. Double check that.*)
	end
 (* TODO : Shall we set the pointer's validity as FalsevarValid ? *)
	
  
(** This function computes the heap shape after a successful call to malloc,
i.e. when Valid(I) and [I]_{\phi} > 0*)
let r_malloc_succ ( lhs : Cil_types.lval option ) sslv (mid: global_mem_manager ) (scal_param : c_scal) =
  	Self.debug ~level:0 "Entering r_malloc_succ";
  let sslv = copy_validity_absdomain sslv in
  match lhs with
      Some((lv,off)) ->
	begin
	let new_abstract = copy_validity_absdomain sslv in
	malloc_upon_ssl lhs mid new_abstract.ssl_part;
	let l = mid#get_last_lvar () in
	let interpret_param = interpret_c_scal_to_cnt sslv.ssl_part 
	  scal_param in
	let lhs_pvar = get_pvar_from_exp_node (Lval(lv,off)) in
	let valid_var_cnt_aff =   make_validity_varpvar lhs_pvar in
	let valid_var_aff = CntAffect( valid_var_cnt_aff , CntCst(My_bigint.one)) in
	let interpret_gt_zero = CntBool(CntGt,interpret_param,CntCst(My_bigint.zero)) in
	let list_locvar_cnt_affect = make_size_locvar l mid interpret_param in 
	let cnt_ptvar_offset =  make_offset_locpvar lhs_pvar in
	let zero_pvar_offset =  CntAffect( cnt_ptvar_offset, CntCst(My_bigint.zero)) in
	let transit_list =  (CntGuard(interpret_gt_zero)) :: list_locvar_cnt_affect  in
	let transit_list = zero_pvar_offset :: transit_list in
	let transit_list = valid_var_aff :: transit_list in
	let ret_list = (( new_abstract , transit_list) :: []) in
	ret_list
	end
    | None ->
      begin
	let new_abstract = copy_validity_absdomain sslv in
	malloc_upon_ssl None mid new_abstract.ssl_part;
	let l = mid#get_last_lvar () in
	let interpret_param = interpret_c_scal_to_cnt sslv.ssl_part 
	  scal_param in
	let interpret_gt_zero = CntBool(CntGt,interpret_param,CntCst(My_bigint.zero)) in
	let list_locvar_cnt_affect = make_size_locvar l mid interpret_param in
	let transit_list =  (CntGuard(interpret_gt_zero)) :: list_locvar_cnt_affect  in
	let ret_list = (( new_abstract , transit_list) :: []) in
	ret_list
      end

(** Same function as above, but includes an extra guard that express which constraints the counter evaluation shall satisfy so that Valid(I) is true. *)
let r_malloc_succ_withvalidcntguard ( lhs : Cil_types.lval option) sslv (mid: global_mem_manager ) (scal_param : c_scal) =
	Self.debug ~level:0 "Entering r_malloc_succ_withvalidcntguard ";
  let sslv = copy_validity_absdomain sslv in
  match lhs with 
      Some((lv,off)) ->
	begin
	  let new_abstract = copy_validity_absdomain sslv in
	  malloc_upon_ssl lhs mid new_abstract.ssl_part;
	   
	    let l = mid#get_last_lvar () in
	    let valid_paral_malloc = valid_cscal sslv.ssl_part scal_param in
	  let validity_guard_cnt = valid_expr_2_cnt_bool valid_paral_malloc in
	  let interpret_param = interpret_c_scal_to_cnt sslv.ssl_part 
	    scal_param in
	  let interpret_gt_zero = CntBool(CntGt,interpret_param,CntCst(My_bigint.zero)) in
	  let good_malloc_guard = CntBAnd(validity_guard_cnt,interpret_gt_zero)
	  in 
	  let pvar_of_lhs = get_pvar_from_exp_node (Lval(lv,off)) in
	  let valid_lhs_var = make_validity_varpvar pvar_of_lhs in
	  let list_locvar_cnt_affect = make_size_locvar l mid interpret_param in
	  let cnt_ptvar_offset =  make_offset_locpvar pvar_of_lhs in
	  let zero_pvar_offset =  CntAffect( cnt_ptvar_offset, CntCst(My_bigint.zero)) in
	  let valid_aff_lhs =  CntAffect( valid_lhs_var, CntCst(My_bigint.one)) in
	  let transit_list =  (CntGuard(good_malloc_guard)) :: list_locvar_cnt_affect  in
	  let transit_list = zero_pvar_offset :: transit_list in
	  let transit_list = valid_aff_lhs :: transit_list in
	  let ret_list = ( new_abstract , transit_list) :: [] in
	  ret_list
	end
    | None ->
      begin
	let new_abstract = copy_validity_absdomain sslv in
	malloc_upon_ssl None mid new_abstract.ssl_part;
	let l = mid#get_last_lvar () in
	let valid_paral_malloc = valid_cscal sslv.ssl_part scal_param in
	let validity_guard_cnt = valid_expr_2_cnt_bool valid_paral_malloc in
	let interpret_param = interpret_c_scal_to_cnt sslv.ssl_part 
	  scal_param in
	let interpret_gt_zero = CntBool(CntGt,interpret_param,CntCst(My_bigint.zero)) in
	let good_malloc_guard = CntBAnd(validity_guard_cnt,interpret_gt_zero)
	in 
	let list_locvar_cnt_affect = make_size_locvar l mid interpret_param in
	
	let transit_list =  (CntGuard(good_malloc_guard)) :: list_locvar_cnt_affect  in
	let ret_list = ( new_abstract , transit_list) :: [] in
	ret_list
      end
  

(** this function computes the labels and the new heap shape when a
call of malloc is performed using a negative or zero parameter.*)	
let r_malloc_neg_or_zero_arg ( lhs : Cil_types.lval option ) sslv  (mid: global_mem_manager ) (scal_param : c_scal) =

  Self.debug ~level:0 "Entering r_malloc_neg_or_zero_arg ";
  let sslv = copy_validity_absdomain sslv in
  match lhs with
      Some((lv,off)) ->
	begin
	  let new_abstract = copy_validity_absdomain sslv in
	  let pvar = get_pvar_from_exp_node (Lval(lv,off)) in
	  let interpret_param = interpret_c_scal_to_cnt new_abstract.ssl_part 
	    scal_param in
	  let aff_to_nil = Pointsnil(pvar) in
	  and_atomic_ptnil aff_to_nil new_abstract.ssl_part;
	  let guard_leq_zero =  CntGuard(CntBool(CntLeq,interpret_param,CntCst(My_bigint.zero))) 
	  in
	  let pvar_of_lhs = get_pvar_from_exp_node (Lval(lv,off)) in  
	  let valid_lhs_var = make_validity_varpvar pvar_of_lhs in
	  let invalid_aff_lhs =  CntAffect( valid_lhs_var, CntCst(My_bigint.zero)) in
	  let ret_list = ((new_abstract, (guard_leq_zero ::( invalid_aff_lhs ::[])))::[] ) in
	  ret_list
	end
    | None ->
      	begin
	  let new_abstract = copy_validity_absdomain sslv in
	  let interpret_param = interpret_c_scal_to_cnt new_abstract.ssl_part 
	    scal_param in
	  let guard_leq_zero =  CntGuard(CntBool(CntLeq,interpret_param,CntCst(My_bigint.zero))) 
	  in 
	  let ret_list = ((new_abstract, (guard_leq_zero :: []))::[] ) in
	  ret_list
	end

(** Same as above, but includes a NTS guard that express the validity condition
w.r.t. counters assigne values.*)

let r_malloc_neg_or_zero_arg_withvalidityguard (lhs : Cil_types.lval option ) sslv  (mid: global_mem_manager ) (scal_param : c_scal) =
  Self.debug ~level:0 " r_malloc_neg_or_zero_arg_withvalidityguard ";
  let sslv = copy_validity_absdomain sslv in
  match lhs with 
      Some((lv,off)) ->
	begin
	  let new_abstract = copy_validity_absdomain sslv in
	  let pvar = get_pvar_from_exp_node (Lval(lv,off)) in
	  let valid_paral_malloc = valid_cscal new_abstract.ssl_part scal_param in
	  let validity_guard_cnt = valid_expr_2_cnt_bool valid_paral_malloc in
	  let interpret_param = interpret_c_scal_to_cnt new_abstract.ssl_part 
	    scal_param in
	  let aff_to_nil = Pointsnil(pvar) in
	  and_atomic_ptnil aff_to_nil new_abstract.ssl_part;
	  let interpret_leq_zero = CntBool(CntLeq,interpret_param,CntCst(My_bigint.zero)) 
	  in 
	  let guard = CntGuard(CntBAnd(validity_guard_cnt,interpret_leq_zero)) in 
	   let pvar_of_lhs = get_pvar_from_exp_node (Lval(lv,off)) in  
	  let valid_lhs_var = make_validity_varpvar pvar_of_lhs in
	  let invalid_aff_lhs =  CntAffect( valid_lhs_var, CntCst(My_bigint.zero)) in 
	  let ret_list = ((new_abstract, invalid_aff_lhs::(guard :: []))::[] ) in
	  ret_list
	end
    | None ->
      begin
	let new_abstract = copy_validity_absdomain sslv in
	let valid_paral_malloc = valid_cscal new_abstract.ssl_part scal_param in
	let validity_guard_cnt = valid_expr_2_cnt_bool valid_paral_malloc in
	let interpret_param = interpret_c_scal_to_cnt new_abstract.ssl_part 
	  scal_param in
	let interpret_leq_zero = CntBool(CntLeq,interpret_param,CntCst(My_bigint.zero)) 
	in 
	let guard = CntGuard(CntBAnd(validity_guard_cnt,interpret_leq_zero)) in 
	let ret_list = ((new_abstract, (guard :: []))::[] ) in
	ret_list	
      end

let r_malloc_failed_with_unvalidcntgard lhs sslv  (mid: global_mem_manager ) (scal_param : c_scal) =

  match lhs with 
      None ->
	begin
	  Self.debug ~level:0 " r_malloc_failed_with_unvalidcntgard ";
	  let sslv = copy_validity_absdomain sslv in
	  let abst_domain = create_validity_abstdomain () 
	  in
	    set_heap_to_top abst_domain.ssl_part ;
	    let valid_paral_malloc = valid_cscal sslv.ssl_part scal_param in
	    let validity_guard_cnt = valid_expr_2_cnt_bool valid_paral_malloc in
	    let invalidity_guard = CntGuard(CntNot ( validity_guard_cnt )) in
	    let ret_list = (( abst_domain , (invalidity_guard :: [])) ::[]) in
	      ret_list
	end
    | Some((lv,off)) ->
	begin
	   Self.debug ~level:0 " r_malloc_failed_with_unvalidcntgard ";
	  let sslv = copy_validity_absdomain sslv in
	  let abst_domain = create_validity_abstdomain () 
	  in
	    set_heap_to_top abst_domain.ssl_part ;
	    let pvar_of_lhs = get_pvar_from_exp_node (Lval(lv,off)) in  
	    let valid_lhs_var = make_validity_varpvar pvar_of_lhs in
	    let invalid_aff_lhs =  CntAffect( valid_lhs_var, CntCst(My_bigint.zero)) in 
	    let valid_paral_malloc = valid_cscal sslv.ssl_part scal_param in
	    let validity_guard_cnt = valid_expr_2_cnt_bool valid_paral_malloc in
	    let invalidity_guard = CntGuard(CntNot ( validity_guard_cnt )) in
	    let ret_list = (( abst_domain , invalid_aff_lhs::(invalidity_guard :: [])) ::[]) in
	      ret_list
	end




(** The evaluation of the abstract prior the application
of malloc is needed to compute the guards on the
transition. This value is passed as the sslf_pre paramater,
and the value sslf_post is used to express the successful
application of the malloc call. i.e. when the condition
expressed by the guards are met.*)

let malloc_ssl_nts_transition ( lhs : lval  option ) sslv  lparam mid  = 
  Self.debug ~level:0 " malloc_ssl_nts_transition ";
  let sslv = copy_validity_absdomain sslv in
  (** Case of a malloc success *)
  let locmap = sslv.validinfos in
     (* Validlocmap (locmap ) -> *) 
  let l = List.hd lparam in (* malloc takes one and only one input parameter.*)
  Format.printf "Malloc parametter is : %s" (Ast_goodies.pprint_cil_exp l);
  let scal_param = cil_expr_2_scalar l in
  Self.debug ~level:0 " [malloc_ssl_nts_transition] Pre valid_sym_cscal ";  
  let valid_sym_guard = valid_sym_cscal locmap sslv.ssl_part scal_param in
  Self.debug ~level:0 " [malloc_ssl_nts_transition] Post valid_sym_cscal ";
  match valid_sym_guard with 
      TruevarValid ->
	      (*In this case, two transitions are allowed, corresponding
		to the cases where the interpretation of the paramater is
		positive or negative. The positiveness can't be checked
	      before runtime, hence we have to generate both transition
		so that flata can do the job.*)
	begin
	  let ret_list = r_malloc_succ lhs sslv mid scal_param in
	  let ret_list = (r_malloc_neg_or_zero_arg lhs sslv mid scal_param )@ret_list 
	  in
	  ret_list
	end
		
    | FalsevarValid ->
      let abs_domain = create_validity_abstdomain () in
      set_heap_to_top abs_domain.ssl_part ;
      (abs_domain,[])::[] (** in this case, we transit right to an error
				state*)
	  (* In this case, the empty list is returned*)
    | DKvarValid ->
      let ret_list =  r_malloc_succ_withvalidcntguard  lhs sslv  mid scal_param in
      let ret_list = (r_malloc_neg_or_zero_arg_withvalidityguard lhs sslv mid scal_param)@ret_list in
      let ret_list = (r_malloc_failed_with_unvalidcntgard lhs sslv mid scal_param)@ret_list in
      ret_list
	      
	
(** mid must be an instance of the class global mem manager*)
let next_on_ssl_instr  (mid : global_mem_manager ) ( sslv : ssl_validity_absdom) ( instruction : Cil_types.instr) =
   	Self.debug ~level:0 "\n Dans next_on_ssl_instr \n" ;
  let sslv = copy_validity_absdomain sslv in
    match instruction with 
	  (*****************************************************************)
	
       
	  (*   We consider here the call of function that have an impact
	  on the heap and the stack, namely :
	       _malloc & calloc
	       _free
	  *)


	  (*****************************************************************)
    
      | Set ( (lv,off),expr, loc) ->       (* Here we handle value 
	(*Set(lv,offset), expr , loc) *)        affectations and pointer 
						 affectations*)
	begin
	  Self.debug ~level:0 "Trying to handle an affectation \n"; 
	  let t = (Cil.typeOfLval (lv , off)) in
	  match t  with 
	      (*Var(v) ->
		begin*)
	    (*  (Self.debug ~level:0 "The left value is a variablex \n");
		match v.vtype with *)
	      TPtr(_,_) -> affect_ptr_upon_sslv (lv , off) expr mid sslv 
		    (* affect_int_val_upon_sslv set the valitidity of
		    v to the validity of expr*)
	    | TInt(_,_) -> affect_int_val_upon_sslv (lv , off) expr sslv
			
		   
	    | _ ->
		let alias_tname = Composite_types.is_integer_type t in
		begin
		  match alias_tname with
		    | Some(_) ->
		      affect_int_val_upon_sslv (lv , off) expr sslv
			      
		    | None ->
		      begin
				  
			(Self.debug ~level:0 " Warning ! Unhandled type of variable affectation, skiping it \n");
			(sslv,[])::[]
				(*let msg= 
				  Format.sprintf "[next_on_ssl_instr] Var : %s = %s : %s \n"  (v.vname) (pprint_cil_exp exp1)( pprint_ciltypes v.vtype) in
				  raise (Debug_info(msg)) *)
				(* Format.printf "%s" msg;
				   (sslv,[])::[] *)
		      end
		end
			
			
			
	end
   
     
      |  Call( Some(lvo) , exp1, lparam , _ )->
	begin
	  	Self.debug ~level:0 " I have a call with some affectation to a variable \n" ;
	      match lvo , exp1.enode with
		  ((Var(v),_) , Lval((Var(f),_)) ) ->
		    begin
		       	Self.debug ~level:0 "\n Dans Call de %s=%s \n" v.vname f.vname ;
		      match v.vtype with
			  (*Returned value has an integer type*)
			  
			  TPtr(_,_) -> (*|  TNamed(_,_) ->*)
			    begin
			      
			      
			      (*The returned value is a variable that has another
				type than an integer type. Tpointer, float for instance*)
			      match f.vname with
				  "malloc" | "calloc" -> (malloc_ssl_nts_transition (Some(lvo)) sslv lparam mid)
				    
				|  _ -> 

				  let funname = f.vname in
				  
				  let arg_nts_list =
				     compile_param_list_2_cnt_list sslv lparam in
				   (* List.map ( fun s-> interpret_c_scal_to_cnt sslv.ssl_part s ) *)
				  let nts_lvals = Nts.make_ntsvars_of_ptrvar v.vname in
				  let cnt_trans_label = 
				    CntFunCall(funname,Some(nts_lvals),arg_nts_list) in
				 (* let mem_access_trans_label =
				    CntGuard((mem_guards_of_funcall_arg_list sslv lparam)) in*)
				  
				  let msg= 
				    Format.sprintf "[next_on_ssl_instr] Pointer type Var : %s = %s : %s \n[next_on_ssl_instr] argument list %s \n "  (v.vname) (pprint_cil_exp exp1)( pprint_ciltypes v.vtype) (Nts.cnt_pprint_translabel cnt_trans_label ) in
				Format.printf "%s" msg;

				  let mem_access_cond = 
				    (mem_guards_of_funcall_arg_list sslv lparam) in
				  let mem_access_trans_label =
				    CntGuard(mem_access_cond) in
				  let mem_access_failure_trans_label =
				    CntGuard(CntNot(mem_access_cond)) in
				  let failure_absdom = create_validity_abstdomain () in
				  set_heap_to_top failure_absdom.ssl_part;
				  let failtransit = (failure_absdom,mem_access_failure_trans_label::[]) in
				  

				  
				failtransit::((sslv,mem_access_trans_label::(cnt_trans_label::[]))::[]) 



			(*	  let msg = Format.sprintf 
				    "[next_on_ssl_instr] Unhandled operation : Pointer Var : %s = %s(...) \n" v.vname f.vname in
				  Format.printf "%s" msg;
				  ((sslv,[])::[]) *)
			    (*raise (Debug_info(msg))*)
				    
			    (** Plug other functions name
				that behaves like malloc in this 
				space*)
			    end
			      
			(*| TNamed(tinfo,_) ->
			(*In this case, the returned type is a composite
			  one, either a structure or an union.*) *)
			      
			| _ ->
			  let alias_tname = Composite_types.is_integer_type v.vtype in
			  begin
			    match alias_tname with
			      | Some(_) ->
				  let funname = f.vname in
				  
				  let arg_nts_list =
				     compile_param_list_2_cnt_list sslv lparam in
				   (* List.map ( fun s-> interpret_c_scal_to_cnt sslv.ssl_part s ) *)
				  let nts_lvals = Nts.make_ntsvars_of_intvars v.vname in
				  let cnt_trans_label = 
				    CntFunCall(funname,Some(nts_lvals),arg_nts_list) in
				 (* let mem_access_trans_label =
				    CntGuard((mem_guards_of_funcall_arg_list sslv lparam)) in
				  let mem_access_failure_trans_label =
				    CntNot(mem_access_trans_label) in *)


				  let mem_access_cond = 
				    (mem_guards_of_funcall_arg_list sslv lparam) in
				  let mem_access_trans_label =
				    CntGuard(mem_access_cond) in
				  let mem_access_failure_trans_label =
				    CntGuard(CntNot(mem_access_cond)) in
				  let failure_absdom = create_validity_abstdomain () in
				  set_heap_to_top failure_absdom.ssl_part;
				  let failtransit = (failure_absdom,mem_access_failure_trans_label::[]) in

			  
				  
				  let msg= 
				    Format.sprintf "[next_on_ssl_instr] Integer type Var : %s = %s : %s \n[next_on_ssl_instr] argument list %s \n "  (v.vname) (pprint_cil_exp exp1)( pprint_ciltypes v.vtype) (Nts.cnt_pprint_translabel cnt_trans_label ) in
				Format.printf "%s" msg;
				 failtransit::((sslv,mem_access_trans_label::(cnt_trans_label::[]))::[])

			      | None ->
				begin
				  let msg= 
				    Format.sprintf "[next_on_ssl_instr] Var : %s = %s : %s \n"  (v.vname) (pprint_cil_exp exp1)( pprint_ciltypes v.vtype) in
				  raise (Debug_info(msg))
			  (* Format.printf "%s" msg;
			     (sslv,[])::[] *)
				end
			  end
		    end

		| ((Mem(e),_), Lval(Var(f),_)) ->
		  begin
		    match e.enode with 
			Lval(Var(v),_) ->
			  begin
			    match f.vname with
				"malloc" | "calloc" -> (malloc_ssl_nts_transition (Some(lvo)) sslv lparam mid)
			  
			      |  _ -> 
				raise ( Debug_info ("Lost in call of malloc/calloc of ((Mem(e),_),Lval(Var(f),_)) case of  next_on_ssl_instr "))
			  end
		      | _ -> raise ( Debug_info ("Lost in ((Mem(e),_),Lval(Var(f),_)) case of  next_on_ssl_instr "))
		  end
		 
		| _ ->  
		  begin
		    let msg = Format.sprintf "[!!! next_on_ssl_instr !!!] The lhs is not a variable : ? = %s, leaving absdomain untouched \n" (pprint_cil_exp exp1 ) in
		    Format.printf "%s %!" msg;
		    (sslv,[])::[]
		  end
	  (** Here the returned value is not a variable,
	      the returned value shall be logged in this case.*)
	end
	  

      |  Call( None , exp1, lparam , _ )->
		Self.debug ~level:0 "I've got a call with no affectation of the returned value \n" ;
	Format.printf "[next_on_ssl_instr:] expr1 = %s \n" (pprint_cil_exp exp1);
	begin
	  match  exp1.enode  with
	      Lval((Var(f),_))->
		begin
		 	Self.debug ~level:0  "Called function is %s \n" f.vname ; 
		  match f.vname with
		      "free" -> 
			begin
			try 
			  let pv = get_first_ptvar_from_lparam lparam in
			  match pv with
			      PVar (vname) ->
				begin
				 	Self.debug ~level:0  "Pvar name is : %s \n" vname ;
				  free_upon_sslv pv sslv
				end			
			with
			    No_pvar_in_free_expression -> 
			      set_heap_to_top sslv.ssl_part;
			      (sslv,[])::[]

			  | Loc_is_nil ->  Self.debug ~level:0 "free on a nil pointer \n"; set_heap_to_top sslv.ssl_part;
			    (sslv,[])::[]
			end
		    | "malloc" | "calloc" -> (malloc_ssl_nts_transition  None sslv lparam mid)
		    
		   
		    | _ -> 
		      let funname = f.vname in
		      
		      let arg_nts_list =
			compile_param_list_2_cnt_list sslv lparam in
				  
		      

		      (********************* Code à factoriser proprement ! *************************)
		      let ret_nts_var_opt =
			(
			  match f.vtype with
			    TFun(TVoid(_),_,_,_) -> None
			    | _ -> let vname = 
				     Ast_goodies.name_of_non_assigned_ret_val () in
				   let nts_lvals = Nts.make_ntsvars_of_intvars vname in
				   Some(nts_lvals)
			);
		      in
		      let mem_access_cond = 
			(mem_guards_of_funcall_arg_list sslv lparam) in
		      let cnt_trans_label = 
			CntFunCall(funname,ret_nts_var_opt,arg_nts_list) in
		      let mem_access_trans_label =
			CntGuard(mem_access_cond) in
		      let valid_transit = 
			(sslv,cnt_trans_label::(mem_access_trans_label::[])) in
		      
		      let mem_access_failure_trans_label =
			CntGuard(CntNot(mem_access_cond)) in
		      let failure_absdom = create_validity_abstdomain () in
		      set_heap_to_top failure_absdom.ssl_part;
		      let failtransit = (failure_absdom,mem_access_failure_trans_label::[]) in				 
		     
		      
		      (*********************Fin de bloc à factoriser *****************************)

		      let msg= 
			Format.sprintf "[next_on_ssl_instr] Call to function: %s : %s \n[next_on_ssl_instr] argument list %s \n "  funname ( pprint_ciltypes f.vtype) (Nts.cnt_pprint_translabel cnt_trans_label ) in
		      Format.printf "%s" msg;
		       failtransit::(valid_transit::[])
		   
		       
		(** All other function name that are dropped leads 
			       here*)
		end
	    | _ -> 
	      (sslv,[])::[]
	(** Here the formula is let untouched*)
	end

      | Asm(_,_,_,_,_,_) -> 
	raise (Debug_info ("I'm lost : ASM block \n"))

      | Skip(position) ->
	Format.printf " !!! Skipping instruction !!! to %s \n" (pprint_lexing_infos position);
	(sslv,[])::[]
	(*
	let msg = Format.sprintf "I'm lost : Skip instruction : To position : %s \n"
	  (Ast_goodies.pprint_lexing_infos position) in
	 raise (Debug_info (msg))
	*)

      | Code_annot (_,_) -> 
	(*raise (Debug_info ("I'm lost : Code_annot block \n"))*)
	(sslv,[])::[]
(** This is the default case, that's to say when
    the parsed operation doesn't match the semantics.
    At this point, we shall add some relevant information
    dealing with the abstracted part of the ast.
*)
	  


let next_on_ssl_nts (mid : global_mem_manager ) (sslv  ) (skind : Cil_types.stmtkind )   =
  match skind with 
      Instr ( instruction ) -> 
	(*let message = ("\n Formula : "^(Ssl_pprinters.pprint_ssl_formula sslv.ssl_part)^"\n") in
	Format.printf "%s \n" message;*)
	 next_on_ssl_instr  mid sslv instruction

    | Return (Some(expr),_) ->
      begin
	let ret_value_type = Cil.typeOf expr in
	match ret_value_type with 
	    TPtr(_,_) ->
	      begin
		let ptr_offset =  cil_expr_2_ptr  expr in
		let cnt_offset = interpret_c_ptrexp_to_cnt sslv.ssl_part ptr_offset in
		let cnt_affect_offset = CntAffect(NtsIVar(("offset__ret_val__")),cnt_offset) in
	     
		let valid_sym_expr = valid_sym_ptrexp_sslv sslv ptr_offset in
		let valid_of_ret = 
		  (match valid_sym_expr with
		      DKvarValid -> CntHavoc(NtsIVar("validity__ret_val__")::[])
		    | TruevarValid -> CntAffect(NtsIVar("validity__ret_val__"),CntCst(My_bigint.one))
		    | FalsevarValid -> CntAffect(NtsIVar("validity__ret_val__"),CntCst(My_bigint.zero))
		  )
		in  
		(sslv , (valid_of_ret::(cnt_affect_offset::[]))) :: []
	      end
	  | _->
	    begin
	      let cnt_exp =  compile_cil_exp_2_cnt sslv expr in
	      let cnt_affect = CntAffect(NtsIVar(("ret_val_")),cnt_exp) in
	    
	      (sslv , (cnt_affect::[])) :: []
	    end
      end

    | Return (None,_) ->
       (sslv , []) :: []
    | _ -> 
      Format.printf " Warning : Not a basic instruction : %s \n" (pprint_skind_basic_infos skind)  ;
      (sslv, [])::[]









    
    
  
  
