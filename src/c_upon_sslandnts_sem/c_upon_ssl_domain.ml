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


exception No_pvar_in_free_expression
exception Wrong_parameter_type_in_free
exception Debug_information of string
exception Contains_no_pvar
exception Loc_is_nil
exception Loc_is_a_constant of int64




let make_offset_locpvar (v : ptvar ) =
  match  v  with 
      PVar ( s ) -> let soff =
	s^"_off" in
	NtsIVar(soff)



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



(** This function aims at getting the first variable name
of the list of parameters. Might be useful if some parameter
expressions are prefixed by a cast or any other ugly stuff so
pecuiliar to the C-language.
*)

(** This function checks whether the argument is a pointer 
variable or a casted pointer variable. It returns a Ssl_type.PVar("vname")
if so, and raise an exception is not.*)
let rec get_pvar_from_exp (expr : Cil_types.exp ) =
  match expr.enode with
      Lval ( Var( p ) , _ ) ->
	begin
	  match p.vtype with 
	      TPtr(_,_) -> (PVar(p.vname))
	    | _ -> raise Contains_no_pvar
	end
    | CastE (TPtr (_,_), e ) ->
	get_pvar_from_exp e

    | Const ( CInt64 (i ,_,_)) -> 
      if (Int64.compare i (Int64.zero)) == 0 then 
	raise Loc_is_nil
      else raise (Loc_is_a_constant(i))

    | _ ->  raise Contains_no_pvar
	  
   
let rec get_first_ptvar_from_lparam ( lparam : Cil_types.exp list ) =
  Self.debug ~level:0 " I am in get_first_ptvar_from lparam\n"; 
 match lparam with 
     [] -> raise No_pvar_in_free_expression
   | h::l' ->
	 begin
	   match h.enode with
	       (Lval(Var(varinfo),_)) ->
		 begin
		   match varinfo.vtype with
		       TPtr(_,_) -> (PVar(varinfo.vname))
		     | _ -> get_first_ptvar_from_lparam l'
		 end
		    | CastE(_,expr) -> 
		      begin 
			Self.debug ~level:0 "J'ai vu un cast dans la liste
des parametres \n" ;
			try
			  get_pvar_from_exp expr
			with
			    Contains_no_pvar -> get_first_ptvar_from_lparam l'
			      
		      end
		(* begin
		 match param.enode with
		     Lval(Var(varinf),_) -> (PVar(varinf.vname))
		   | _ -> get_first_ptvar_from_lparam l'
		 end *)
       
		    | _  -> get_first_ptvar_from_lparam l' 
	 end
	   


let affect_ptr_upon_sslv (v : Cil_types.varinfo)  (expr : Cil_types.exp) (sslv : ssl_validity_absdom ) =
  Self.debug ~level:0 "Im am in affect_ptr_upon_ssl \n";
  try
    let pvar_left = (PVar(v.vname)) in
    let pvar_right = get_pvar_from_exp expr in
    let lvar_right = get_ptr_affectation sslv.ssl_part pvar_right  in
    Ssl.change_affect_var (Pointsto(pvar_left,lvar_right)) sslv.ssl_part;
    ((sslv,[])::[]) (* TODO : Update the validity of the lval pointer 
		    variable.*)
  with
      Not_found -> Self.debug ~level:0 "Undefined right member in affectation, affect_ptr_upon_ssl crash"; raise Not_found
    | Loc_is_nil -> and_atomic_ptnil (Pointsnil((PVar(v.vname)))) sslf
  


(** This function modifies the sslf formula that abstracts the current
heap and stack when a call to malloc is performed.*)
let malloc_upon_sslv  ( v : Cil_types.varinfo option ) ( mid : global_mem_manager )  (sslv : ssl_validity_absdom ) =
  match v with Some (vinfo) ->
    let lvar = mid#lvar_from_malloc () in
    let pvar = (PVar(vinfo.vname)) in
    let affect = (Pointsto (pvar,lvar)) in
    Ssl.add_quant_var lvar sslv.ssl_part;
    Ssl.change_affect_var affect sslv.ssl_part;
    Ssl.add_alloc_cell lvar sslv.ssl_part
      (*TODO : Need to set the left value pointer var validity 
	as TruevarValid*)
    | None ->
       let lvar = mid#lvar_from_malloc () in
       Ssl.add_quant_var lvar sslf;
       Ssl.add_alloc_cell lvar sslf
	 
(** Effect of a free(x),  where x is a pointer variable, on an ssl
formula.*)
let free_upon_sslv (pvar : ptvar)(sslv : ssl_validity_absdom ) =
  try
    let lvar = get_ptr_affectation sslv.ssl_part pvar in
    if not (space_contains_locvar lvar  sslv.ssl_part.space )
    then set_heap_to_top sslv.ssl_part 
    else
      try_remove_segment lvar sslv.ssl_part
  with
      Not_found -> 
	begin 
	  set_heap_to_top sslv.ssl_part; (* Here we get that pvar
					does not belong to the 
					the affectation table*)
	  (sslv,[])::[]
	end
 (* TODO : Shall we set the pointer's validity as FalsevarValid ? *)
	
  
(** This function computes the heap shape after a successful call to malloc,
i.e. when Valid(I) and [I]_{\phi} > 0*)
let r_malloc_succ (v : Cil_types.varinfo ) sslv (mid: global_mem_manager ) (scal_param : c_scal) =
  let new_abstract = copy_validity_absdomain sslv in
  malloc_upon_ssl (Some(v)) mid new_abstract.ssl_part;
  let l = mid#get_last_lvar () in
  let interpret_param = interpret_c_scal_to_cnt sslv.ssl_part 
    scal_param in
  let interpret_gt_zero = CntBool(CntGt,interpret_param,CntCst(0)) in
  let list_locvar_cnt_affect = make_size_locvar l mid interpret_param in
  let cnt_ptvar_offset =  make_offset_locpvar (PVar(v.vname)) in
  let zero_pvar_offset =  CntAffect( cnt_ptvar_offset, CntCst(0)) in
  let transit_list =  (CntGuard(interpret_gt_zero)) :: list_locvar_cnt_affect  in
  let transit_list = zero_pvar_offset :: transit_list in
  let ret_list = (( new_abstract , transit_list) :: []) in
  ret_list



(** Same function as above, but includes an extra guar that express which constraints the counter evaluation shall satisfy so that Valid(I) is true. *)
let r_malloc_succ_withvalidcntguard (v : Cil_types.varinfo ) sslv (mid: global_mem_manager ) (scal_param : c_scal) =

  let new_abstract = copy_validity_absdomain sslv in
  malloc_upon_ssl (Some(v)) mid new_abstract.ssl_part;
  let l = mid#get_last_lvar () in
  let valid_paral_malloc = valid_cscal sslv.ssl_part scal_param in
  let validity_guard_cnt = valid_expr_2_cnt_bool valid_paral_malloc in
  let interpret_param = interpret_c_scal_to_cnt sslv.ssl_part 
    scal_param in
  let interpret_gt_zero = CntBool(CntGt,interpret_param,CntCst(0)) in
  let good_malloc_guard = CntBAnd(validity_guard_cnt,interpret_gt_zero)
  in 
  let list_locvar_cnt_affect = make_size_locvar l mid interpret_param in
  let cnt_ptvar_offset =  make_offset_locpvar (PVar(v.vname)) in
  let zero_pvar_offset =  CntAffect( cnt_ptvar_offset, CntCst(0)) in
  let transit_list =  (CntGuard(good_malloc_guard)) :: list_locvar_cnt_affect  in
  let transit_list = zero_pvar_offset :: transit_list in
  let ret_list = ( new_abstract , transit_list) :: [] in
  ret_list

  
(** this function computes the labels and the new heap shape when a
call of malloc is performed using a negative or zero parameter.*)
	
let r_malloc_neg_or_zero_arg (v : Cil_types.varinfo ) sslv  (mid: global_mem_manager ) (scal_param : c_scal) =
  let new_abstract = copy_validity_absdomain sslv in
  let pvar = (PVar( v.vname )) in
  let interpret_param = interpret_c_scal_to_cnt sslv.ssl_part 
    scal_param in
  let aff_to_nil = Pointsnil(pvar) in
  and_atomic_ptnil aff_to_nil new_abstract.ssl_part;
  let guard_leq_zero =  CntGuard(CntBool(CntLeq,interpret_param,CntCst(0))) 
  in 
  let ret_list = ((new_abstract, (guard_leq_zero :: []))::[] ) in
  ret_list

(** Same as above, but includes a NTS guard that express the validity condition
w.r.t. counters assigne values.*)
let r_malloc_neg_or_zero_arg_withvalidityguard (v : Cil_types.varinfo ) sslv  (mid: global_mem_manager ) (scal_param : c_scal) =
  let new_abstract = copy_validity_absdomain sslv in
  let pvar = PVar( v.vname ) in
  let valid_paral_malloc = valid_cscal sslv.ssl_part scal_param in
  let validity_guard_cnt = valid_expr_2_cnt_bool valid_paral_malloc in
  let interpret_param = interpret_c_scal_to_cnt sslv.ssl_part 
    scal_param in
  let aff_to_nil = Pointsnil(pvar) in
  and_atomic_ptnil aff_to_nil new_abstract.ssl_part;
  let interpret_leq_zero = CntBool(CntLeq,interpret_param,CntCst(0)) 
  in 
  let guard = CntGuard(CntBAnd(validity_guard_cnt,interpret_leq_zero)) in 
  let ret_list = ((new_abstract, (guard :: []))::[] ) in
  ret_list


let r_malloc_failed_with_unvalidcntgard (v : Cil_types.varinfo ) sslv  (mid: global_mem_manager ) (scal_param : c_scal) =
  let abst_domain = create_validity_abstdomain in
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

let malloc_ssl_nts_transition ( v : Cil_types.varinfo ) sslv  lparam mid  = 
  (** Case of a malloc success *)
  let locmap = sslv.validinfos in
     (* Validlocmap (locmap ) -> *) 
  let l = List.hd lparam in (* malloc takes one and only one input parameter.*)
  let scal_param = cil_expr_2_scalar l in
  let valid_sym_guard = valid_sym_cscal locmap sslv.ssl_part scal_param in  
  match valid_sym_guard with 
      TruevarValid ->
	      (*In this case, two transitions are allowed, corresponding
		to the cases where the interpretation of the paramater is
		positive or negative. The positiveness can't be checked
	      before runtime, hence we have to generate both transition
		so that flata can do the job.*)
	begin
	  let ret_list = r_malloc_succ v sslv mid scal_param in
	  let ret_list = (r_malloc_neg_or_zero_arg v sslv mid scal_param )@ret_list 
	  in
	  ret_list
	end
		
    | FalsevarValid ->
      let abs_domain = create_validity_abstdomain in
      set_heap_to_top abs_domain.ssl_part ;
      (abs_domain,[])::[] (** in this case, we transit right to an error
				state*)
	  (* In this case, the empty list is returned*)
    | DKvarValid ->
      let ret_list =  r_malloc_succ_withvalidcntguard  v sslv  mid scal_param in
      let ret_list = (r_malloc_neg_or_zero_arg_withvalidityguard v sslv mid scal_param)@ret_list in
      let ret_list = (r_malloc_failed_with_unvalidcntgard v sslv mid scal_param)@ret_list in
      ret_list
	      
	    

  





(** mid must be an instance of the class global mem manager*)
let next_on_ssl_instr  (mid : global_mem_manager ) ( sslv : ssl_validity_absdom) ( instruction : Cil_types.instr) =
   	Self.debug ~level:0 "\n Dans next_on_ssl_instr \n" ;
    match instruction with 
	  (*****************************************************************)
	
       
	  (*   We consider here the call of function that have an impact
	  on the heap and the stack, namely :
	       _malloc & calloc
	       _free
	  *)


	  (*****************************************************************)
    
      | Set ( (lv,_),expr, loc) ->       (* Here we handle value 
	(*Set(lv,offset), expr , loc) *)        affectations and pointer 
						 affectations*)
	begin
	  Self.debug ~level:0 "Trying to handle an affectation \n"; 
	  match lv with 
	      Var(v) ->
		begin
		  (Self.debug ~level:0 "The left value is a variablex \n");
		  match v.vtype with 
		      TPtr(_,_) -> affect_ptr_upon_ssl v expr sslv 
		    | _ -> (Self.debug ~level:0 "Unhandled type of variable affectation, skiping it \n")
		end
	    | _ ->  Self.debug ~level:0 "The left member of this affectation is not a variable, skiping it \n"; ()	
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
			   
			  TPtr(_,_)->
			    begin
			      match f.vname with
				  "malloc" | "calloc" -> (malloc_upon_ssl (Some(v)) mid sslf)
				|  _ -> () (** Plug other functions name
					   that behaves like malloc in this 
					   space*)
			    end
			 (*The returned value is a variable that has another
			 type than an integer type. Tpointer, float for instance*)

			| _ -> () (** Here the formula is let untouched*)
		    end
		| _ -> () (** Here the returned value is not a variable,
			  the returned value shall be logged in this case.*)
	  end


      |  Call( None , exp1, lparam , _ )->
		Self.debug ~level:0 "I've got a call with no affectation of the returned value \n" ;
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
				  free_upon_ssl pv sslf 
				end			
			with
			    No_pvar_in_free_expression -> 
			      set_heap_to_top sslf
			  | Loc_is_nil ->  Self.debug ~level:0 "free on a nil pointer \n"; set_heap_to_top sslf
			end
		    | "malloc" | "calloc" -> (malloc_upon_ssl  None mid sslf)
		    | _ -> () (** All other function name that are dropped leads 
			       here*)
		end
	    | _ -> () (** Here the formula is let untouched*)
	end

      | _ -> () (** This is the default case, that's to say when
		    the parsed operation doesn't match the semantics.
		    At this point, we shall add some relevant information
		    dealing with the abstracted part of the ast.
		*)








let next_on_ssl_nts (mid : global_mem_manager ) (sslv  ) (skind : Cil_types.stmtkind ) _  =
  match skind with 
      Instr ( instruction ) ->  next_on_ssl_instr  mid sslv.ssl_part instruction;
	let message = ("\n Formula : "^(Ssl_pprinters.pprint_ssl_formula sslv.ssl_part)^"\n") in
	Format.printf "%s \n" message;
	normalize_ssl sslv.ssl_part
    | _ -> ()  
(*
let  generate_list_of_transitions sslf  instr =
  match instr with 

    
      | Set ( (lv,_),expr, loc) ->       (* Here we handle value 
	(*Set(lv,offset), expr , loc) *)        affectations and pointer 
						 affectations*)
	begin
	  Self.debug ~level:0 "Trying to handle an affectation \n"; 
	  match lv with 
	      Var(v) ->
		begin
		  (Self.debug ~level:0 "The left value is a variablex \n");
		  match v.vtype with 
		      TPtr(_,_) -> affect_ptr_upon_ssl v expr sslf 
		    | _ -> (Self.debug ~level:0 "Unhandled type of variable affectation, skiping it \n")
		end
	    | _ ->  Self.debug ~level:0 "The left member of this affectation is not a variable, skiping it \n"; ()	
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
			   
			  TPtr(_,_)->
			    begin
			      match f.vname with
				  "malloc" | "calloc" -> (malloc_upon_ssl (Some(v)) mid sslf)
				|  _ -> () (** Plug other functions name
					   that behaves like malloc in this 
					   space*)
			    end
			 (*The returned value is a variable that has another
			 type than an integer type. Tpointer, float for instance*)

			| _ -> () (** Here the formula is let untouched*)
		    end
		| _ -> () (** Here the returned value is not a variable,
			  the returned value shall be logged in this case.*)
	  end


      |  Call( None , exp1, lparam , _ )->
		Self.debug ~level:0 "I've got a call with no affectation of the returned value \n" ;
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
				  free_upon_ssl pv sslf 
				end			
			with
			    No_pvar_in_free_expression -> 
			      set_heap_to_top sslf
			  | Loc_is_nil ->  Self.debug ~level:0 "free on a nil pointer \n"; set_heap_to_top sslf
			end
		    | "malloc" | "calloc" -> (malloc_upon_ssl  None mid sslf)
		    | _ -> () (** All other function name that are dropped leads 
			       here*)
		end
	    | _ -> () (** Here the formula is let untouched*)
	end

      | _ -> () (** This is the default case, that's to say when
		    the parsed operation doesn't match the semantics.
		    At this point, we shall add some relevant information
		    dealing with the abstracted part of the ast.
		*)
 

*)
(*

 We shall do the following thing :
 Adding a generate_list_of_bad_numerical_preconditions 

let next_on_ssl (mid : global_mem_manager ) (sslf : ssl_formula ) (skind : Cil_types.stmtkind ) _  =
  match skind with 
      Instr ( instruction ) ->  next_on_ssl_instr  mid sslf instruction;
	normalize_ssl sslf;
  let trans_list =
  generate_list_of_transitions ( sslf : evaluates the abstract domain value
 after a successful exectution of instr) instr 
 in trans_list

 (** generate the list of cnt_trans_label list * ssl_formula such that 
 contains the transition from the current state to the next abstact state when
 some preconditions are met and from the current state to bot when others  
 conditions are met.

 We expect that the second set of conditions corresponds to the negations 
 of the first ones.
 *) 
    | _ -> ()

*)







    
    
  
  
