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


exception No_pvar_in_free_expression
exception Wrong_parameter_type_in_free
exception Debug_information of string

exception Debug_info of string 


exception Assert_fail_exception 

(*
type il_expr = IlScal of c_scal
	       | IlPtr of c_ptrexp
*)

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
	  
		      

let compile_param_list_2_cnt_list sslv ( lparam : Cil_types.exp list) =
  let cilexp_2_il_iterator e =
    let type_of_e = Cil.typeOf e in
    match type_of_e with
	TPtr(_,_) | TArray(_,_,_,_) -> 
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
	      raise (Debug_info ("compile_param_list_2_cnt_list : I have an argume which type is neither an integer value nor a pointer/array"))
	end
  in
  let ilexp_2_cnt_iterator i =
    match i with
	IlPtr(e) -> interpret_c_ptrexp_to_cnt sslv.ssl_part e
      | IlScal(e) -> interpret_c_scal_to_cnt sslv.ssl_part e
  in
  let il_list = List.map ( cilexp_2_il_iterator ) lparam in
  let ret_list = List.map (ilexp_2_cnt_iterator ) il_list
  in ret_list
 (*  contains the ret_list *)


let make_offset_locpvar (v : ptvar ) =
  match  v  with 
      PVar ( s ) -> let soff =
	"offset__"^s^"_" in
	NtsIVar(soff)

let make_validity_varpvar ( v : ptvar) =
  match  v  with 
      PVar ( s ) -> let vdty =
	"validity__"^s^"_" in
	NtsIVar(vdty)



(** If used to descibe a malloc creation of a mem segment,
this function must be called AFTER the generation of the
ssl formula, to get the good gmid identificator.*)
let make_size_locvar ( l : locvar ) (mid : global_mem_manager ) ( block_size : cnt_arithm_exp) =
  match l with
      LVar( vname ) -> 
	let id_seg = mid#get_last_mid () in
	let lbase_name = vname^"_base" in
	let lsize_name = vname^"_size" in
	let cnt_lbase = NtsIVar(lbase_name) in
	let cnt_lsize = NtsIVar(lsize_name) in
	let affect_list = (CntAffect(cnt_lbase,CntCst(id_seg))::[] ) in
	let affect_list = (CntAffect(cnt_lsize,block_size))::affect_list in
	affect_list



(**
This function progates the validity to an expression to the lvalue
when the latter is an integer variable
*)

let affect_int_val_upon_sslv (v : Cil_types.varinfo) (expr : Cil_types.exp) 
    (sslv : ssl_validity_absdom ) =
  Self.debug ~level:0 " Entering affect_int_val_upon_sslv ";
  let scal_of_exp = cil_expr_2_scalar expr in
  let validity_of_rval = valid_sym_cscal sslv.validinfos sslv.ssl_part 
    scal_of_exp in
  let ret_absdomain = copy_validity_absdomain sslv in
  let ret_absdomain =
    set_var_validity_in_absdomain ret_absdomain v None validity_of_rval in
  let c_scal_exp = cil_expr_2_scalar expr in 
  let cnt_expr = interpret_c_scal_to_cnt sslv.ssl_part c_scal_exp in
  let cnt_affect = CntAffect(NtsIVar(v.vname),cnt_expr) in
    (ret_absdomain , (cnt_affect::[])) :: []
    



(** This function aims at getting the first variable name
of the list of parameters. Might be useful if some parameter
expressions are prefixed by a cast or any other ugly stuff so
pecuiliar to the C-language.
*)

(*If lv is a variable whose type is TPtr(_,_), then return it,
else raise an exception
*)

let get_pvarinfo_of_left_value lv =
  match lv with
      Var(v) -> 
	begin
	  match v.vtype with
	      TPtr(_,_) -> v
	    | _ -> raise (Debug_info("I have a variable, but it's not a pointer var, as I expected \n"))
	end
    | _ -> raise (Debug_info ("This left value is not a variable \n"))

(* If the left value lv is a Ptr variable, then return its name*)
let get_pvarname_of_left_value lv =
  match lv with
      Var(v) -> 
	begin
	  match v.vtype with
	      TPtr(_,_) -> v.vname
	    | _ -> raise (Debug_info("I have a variable, but it's not a pointer var, as I expected \n"))
	end
    | _ -> raise (Debug_info ("This left value is not a variable \n"))


let affect_ptr_upon_sslv ( (lv,off) : Cil_types.lval)  (expr : Cil_types.exp) (sslv : ssl_validity_absdom ) =
  Self.debug ~level:0 "Im am in affect_ptr_upon_sslv \n";
  let sslv =  copy_validity_absdomain sslv in
  let v = get_pvarinfo_of_left_value lv in
  let varname =  get_pvarname_of_left_value lv in
    (** One need to check that lv is a Var which type is TPtr(_,_)*)
  try
    
    Format.printf "[affect_ptr_upon_sslv: expression : %s=%s \n]" varname (pprint_cil_exp expr);
    let pvar_left = Ast_goodies.get_pvar_from_exp_node  (Lval(lv,off)) in
    let pvar_right = get_pvar_from_exp expr in
    Format.printf "I have a pvar \n %!";
    let sslv = copy_validity_absdomain sslv in
    let lvar_right = get_ptr_affectation sslv.ssl_part pvar_right 
    in
    begin
      match lvar_right with
	  LVar("") ->  Ssl.and_atomic_ptnil (Pointsnil(pvar_right)) sslv.ssl_part
	| LVar(_) -> Ssl.change_affect_var (Pointsto(pvar_left,lvar_right)) sslv.ssl_part
    end;
    Format.printf "On the way to compute cil_expre_2_per of expr %s \n %!" (pprint_cil_exp expr);
      let param_cscal = Intermediate_language.cil_expr_2_ptr expr in
      Format.printf "Operation done \n about to compute interpret_c_ptr_exp_to_cnt \n %!";
      let offset_of_pexpr = interpret_c_ptrexp_to_cnt sslv.ssl_part param_cscal in
      let offset_var_of_pvar =  offset_ntsivar_of_pvar pvar_left in
      let affect_off = CntAffect(offset_var_of_pvar,offset_of_pexpr) in
      let affect_validity_of_pvar = valid_sym_ptrexp sslv.validinfos sslv.ssl_part param_cscal in
      let sslv_new = set_var_validity_in_absdomain 
	sslv v (Some(off)) affect_validity_of_pvar 
      in
      ((sslv_new,affect_off::[])::[]) 
  with
      
    | Loc_is_nil -> 
      begin 
	and_atomic_ptnil (Pointsnil((PVar(v.vname)))) sslv.ssl_part;
	let new_sslv = set_var_validity_in_absdomain sslv v (Some(off)) FalsevarValid in
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
	let eq_zero_guard = CntBool(CntEq,offset_of_pvar,CntCst(0)) in
	let non_zero_guard =   CntBool(CntNeq,offset_of_pvar,CntCst(0)) in
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
	let valid_var_aff = CntAffect( valid_var_cnt_aff , CntCst(1)) in
	let interpret_gt_zero = CntBool(CntGt,interpret_param,CntCst(0)) in
	let list_locvar_cnt_affect = make_size_locvar l mid interpret_param in 
	let cnt_ptvar_offset =  make_offset_locpvar lhs_pvar in
	let zero_pvar_offset =  CntAffect( cnt_ptvar_offset, CntCst(0)) in
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
	let interpret_gt_zero = CntBool(CntGt,interpret_param,CntCst(0)) in
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
	  let interpret_gt_zero = CntBool(CntGt,interpret_param,CntCst(0)) in
	  let good_malloc_guard = CntBAnd(validity_guard_cnt,interpret_gt_zero)
	  in 
	  let pvar_of_lhs = get_pvar_from_exp_node (Lval(lv,off)) in
	  let valid_lhs_var = make_validity_varpvar pvar_of_lhs in
	  let list_locvar_cnt_affect = make_size_locvar l mid interpret_param in
	  let cnt_ptvar_offset =  make_offset_locpvar pvar_of_lhs in
	  let zero_pvar_offset =  CntAffect( cnt_ptvar_offset, CntCst(0)) in
	  let valid_aff_lhs =  CntAffect( valid_lhs_var, CntCst(1)) in
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
	let interpret_gt_zero = CntBool(CntGt,interpret_param,CntCst(0)) in
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
	  let guard_leq_zero =  CntGuard(CntBool(CntLeq,interpret_param,CntCst(0))) 
	  in 
	  let ret_list = ((new_abstract, (guard_leq_zero :: []))::[] ) in
	  ret_list
	end
    | None ->
      	begin
	  let new_abstract = copy_validity_absdomain sslv in
	  let interpret_param = interpret_c_scal_to_cnt new_abstract.ssl_part 
	    scal_param in
	  let guard_leq_zero =  CntGuard(CntBool(CntLeq,interpret_param,CntCst(0))) 
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
	  let interpret_leq_zero = CntBool(CntLeq,interpret_param,CntCst(0)) 
	  in 
	  let guard = CntGuard(CntBAnd(validity_guard_cnt,interpret_leq_zero)) in 
	  let ret_list = ((new_abstract, (guard :: []))::[] ) in
	  ret_list
	end
    | None ->
      begin
	let new_abstract = copy_validity_absdomain sslv in
	let valid_paral_malloc = valid_cscal new_abstract.ssl_part scal_param in
	let validity_guard_cnt = valid_expr_2_cnt_bool valid_paral_malloc in
	let interpret_param = interpret_c_scal_to_cnt new_abstract.ssl_part 
	  scal_param in
	let interpret_leq_zero = CntBool(CntLeq,interpret_param,CntCst(0)) 
	in 
	let guard = CntGuard(CntBAnd(validity_guard_cnt,interpret_leq_zero)) in 
	let ret_list = ((new_abstract, (guard :: []))::[] ) in
	ret_list	
      end

let r_malloc_failed_with_unvalidcntgard _ sslv  (mid: global_mem_manager ) (scal_param : c_scal) =
   Self.debug ~level:0 " r_malloc_failed_with_unvalidcntgard ";
  let sslv = copy_validity_absdomain sslv in
  let abst_domain = create_validity_abstdomain () in
  set_heap_to_top abst_domain.ssl_part ;
  let valid_paral_malloc = valid_cscal sslv.ssl_part scal_param in
  let validity_guard_cnt = valid_expr_2_cnt_bool valid_paral_malloc in
  let invalidity_guard = CntGuard(CntNot ( validity_guard_cnt )) in
  let ret_list = (( abst_domain , (invalidity_guard :: [])) ::[]) in
  ret_list


  
  
  
  
  
	  

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
	  match lv with 
	      Var(v) ->
		begin
		  (Self.debug ~level:0 "The left value is a variablex \n");
		  match v.vtype with 
		      TPtr(_,_) -> affect_ptr_upon_sslv (lv , off) expr sslv 
		    (* affect_int_val_upon_sslv set the valitidity of
		    v to the validity of expr*)
		    | TInt(_,_) -> affect_int_val_upon_sslv v expr sslv
			
		   
		    | _ ->
		      let alias_tname = Composite_types.is_integer_type v.vtype in
		      begin
			match alias_tname with
			  | Some(_) ->
			    affect_int_val_upon_sslv v expr sslv
			      
			  | None ->
			    begin
				  
			      (Self.debug ~level:0 "Unhandled type of variable affectation, skiping it \n");
			      (sslv,[])::[]
				(*let msg= 
				  Format.sprintf "[next_on_ssl_instr] Var : %s = %s : %s \n"  (v.vname) (pprint_cil_exp exp1)( pprint_ciltypes v.vtype) in
				  raise (Debug_info(msg)) *)
				(* Format.printf "%s" msg;
				   (sslv,[])::[] *)
			    end
		      end
			
			
			
		end
	    | Mem(e) ->
	      begin
		(*let v_dest = get_pvar_from_exp e in*)
		match e.enode with
		    Lval(Var(v),off) ->
		      begin
			match v.vtype with
			    TPtr(_,_)
			    -> ((sslv,[])::[])
			      (*affect_ptr_upon_sslv  (Var(v),off) expr sslv*)
			 (* | TInt(_,_)
			    -> affect_int_val_upon_sslv v expr sslv
			 *) 
			  | _ ->
			    
			let msg =
			  Format.sprintf "Mem(e) with e.node : Lval(Var(v),off), not yet supported, where v = %s and offset = %s \n, %s <- %s. Need to check alignement and bloc size." v.vname (Ast_goodies.pprint_offset off ) v.vname (pprint_cil_exp expr) in
			Format.printf "%s" msg;
			 ((sslv,[])::[])
			(*raise (Debug_info (msg))*)
		      end
			| _ -> 
			  let msg = Format.sprintf "[next_on_ssl_affect :]Paramater e in Mem(e) is not a Lval(Var(),_), e is : %s. Need to check block size and segmentation here \n" (pprint_cil_exp e) in
			  Format.printf "%s" msg;
			  (*raise (Debug_info (msg))*)
			  ((sslv,[])::[])
	      end
	    | _ ->  Self.debug ~level:0 "The left member of this affectation is not a variable, skiping it \n"; 
	      (sslv,[])::[]
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
				  let nts_lval = NtsIVar(v.vname) in
				  let cnt_trans_label = 
				    CntFunCall(funname,Some(nts_lval),arg_nts_list) in
				  let msg= 
				    Format.sprintf "[next_on_ssl_instr] Pointer type Var : %s = %s : %s \n[next_on_ssl_instr] argument list %s \n "  (v.vname) (pprint_cil_exp exp1)( pprint_ciltypes v.vtype) (Nts.cnt_pprint_translabel cnt_trans_label ) in
				Format.printf "%s" msg;
				 ((sslv,(cnt_trans_label::[]))::[]) 



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
				  let nts_lval = NtsIVar(v.vname) in
				  let cnt_trans_label = 
				    CntFunCall(funname,Some(nts_lval),arg_nts_list) in
				  let msg= 
				    Format.sprintf "[next_on_ssl_instr] Integer type Var : %s = %s : %s \n[next_on_ssl_instr] argument list %s \n "  (v.vname) (pprint_cil_exp exp1)( pprint_ciltypes v.vtype) (Nts.cnt_pprint_translabel cnt_trans_label ) in
				Format.printf "%s" msg;
				 ((sslv,(cnt_trans_label::[]))::[])

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
		    
		    (*| "__assert_fail" ->
			raise Assert_fail_exception*) (* Used to notify that
						    the current control state
						    do correspond to an 
						    assertion failure. Heap
						    and stack aren't 
						       necessarily flawed.*)
		    | _ -> 
		      let funname = f.vname in
		      
		      let arg_nts_list =
			compile_param_list_2_cnt_list sslv lparam in
				   (* List.map ( fun s-> interpret_c_scal_to_cnt sslv.ssl_part s ) *)
		      
		      let cnt_trans_label = 
			CntFunCall(funname,None,arg_nts_list) in
		      let msg= 
			Format.sprintf "[next_on_ssl_instr] Call to function: %s : %s \n[next_on_ssl_instr] argument list %s \n "  funname ( pprint_ciltypes f.vtype) (Nts.cnt_pprint_translabel cnt_trans_label ) in
		      Format.printf "%s" msg;
		      ((sslv,(cnt_trans_label::[]))::[]) 
		       
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
      
      let cnt_exp =  compile_cil_exp_2_cnt sslv expr in
      let cnt_affect = CntAffect(NtsIVar(("ret_val_")),cnt_exp) in
      (sslv , (cnt_affect::[])) :: []

    | Return (None,_) ->
       (sslv , []) :: []
    | _ -> 
      Format.printf " Warning : Not a basic instruction : %s \n" (pprint_skind_basic_infos skind)  ;
      (sslv, [])::[]









    
    
  
  
