(**
This file specializes the generic front end, in order to extract a
counter automata based model which nodes are labelled with values
of the SSL logic. 
*)
(*open Intermediate_language*)

exception IndexOfCompositeTypesNotSet
exception Not_a_mem_violation_guard
exception Not_a_switch_construct

open Cil_types
open Sem_and_logic_front_end 
open Ssl_types
open SSL_lex
open Ssl_entailement
open Ssl_decision
open Ssl_pprinters 
open C_upon_ssl_domain (* Contains the semantic tranformation
		       of the C instruction on the SSL formulae*)
open Nts_types
open Nts
open Global_mem
 (* This type defines 2-uples of ssl_formual and
			    a validity_loc_map*)
open  Ssl_valid_abs_dom_types
open  Ssl_valid_abs_dom (*Contains the copy_validity_absdomain function*)

open Composite_type_types
open Composite_types (* Type definition for analysing C language composite
		     types, such as structures and enumerations*)
open Composite_type_visitors
open Visitor (* Frama-c visitors*)
open Intermediate_language
open Cnt_interpret
open Var_validity_types
open Var_registration
open Self
open Ast_goodies
open Compile_2_nts
(*open Guard_of_mem_access*)

(* This function is used to compute the guard of the default
case of a switch-case-brake control flow construction.

Basically, if the default case is indeed present, then
the guard corresponds to the negation of all the other
cases guards.
*)


let compute_default_case_guard sslv
    (expr_switch_param : Cil_types.exp ) (stmt_succs : Cil_types.stmt list ) =
 
  (*This iterator bellow is useful when a statment has 
    several case(expr) label expressions

  switch(t){
   case 2:
   case 5:
   case 7:
     printf(...).
  
   One shall compute 
     t!=2 /\ t!=5 /\ t!=7
  *)
  
  (* On compiles the switch parameter expression once for all*)
  let guard_lhs = compile_cil_exp_2_cnt sslv expr_switch_param 
  in
  let rhs_case_condition_folder (local_guard : cnt_bool ) 
      (rhs : Cil_types.label)
      =
    match rhs with 
	Case(rhs_exp,_) ->
	  let rhs_compiled = 
	    compile_cil_exp_2_cnt sslv rhs_exp
	  in
	  let local_guard = 
	    CntBAnd(local_guard,CntBool(CntNeq,guard_lhs,rhs_compiled))
	  in local_guard
	  
      | _ -> local_guard
  in
  let guard_folder ( guard : cnt_bool) (stmt : Cil_types.stmt) =
    if (stmt_has_default_label stmt) 
    then
      begin
	guard (* Don't change the guard if the label is default*)
      end
    else
      begin
	  (*The local guards do corresponds to conjonction
	  of the rhs != lhs, for all lhs that appear as a 
	    parametter of a case(lhs) :*)

	let local_guard =
	  List.fold_left rhs_case_condition_folder guard 
	    stmt.labels
	in
	local_guard   
      end
  in
  let default_case_guard = 
    List.fold_left guard_folder CntBTrue stmt_succs 
  in 
  default_case_guard (* This value should be the expression
		     of the default guard.*)
      
    

(* Returns  Some(stmt) that corresponds to the default case
if any, otherwise None is returned *)
let get_default_stmt (stmt_succs : Cil_types.stmt list ) =
  let get_folder ( def_statement : Cil_types.stmt option) 
      (curr_stmt : Cil_types.stmt ) =
    match def_statement with
	None ->
	  begin
	    if stmt_has_default_label curr_stmt 
	    then Some(curr_stmt)
	    else None
	  end
      | Some(_) ->
	def_statement
  in
  List.fold_left get_folder None stmt_succs

    

	



let pprint_trans_list_foldleft (s : string ) ( trans : cnt_trans_label ) =
  match (s,trans) with 
    | ("",CntGuard(guard))-> 
      let s_guard = simplify_cnt_boolexp guard in
      (*let s_guard = guard in*)
      begin
	match s_guard with 
	    CntBTrue -> ""
	  | _ -> cnt_pprint_boolexp s_guard
      end
    | ("",_) ->
      (cnt_pprint_translabel trans )
    | (_,CntGuard(guard)) -> 
      let s_guard = simplify_cnt_boolexp guard in
      (*let s_guard=guard in *)
      begin
	match s_guard with 
	    CntBTrue -> s
	  | _ -> s^ " and "^(cnt_pprint_boolexp s_guard) 
      end
	
    | (_,_) -> s^" and "^(cnt_pprint_translabel trans )
  

let prefix_trans_label_list (prefix : Nts_types.cnt_trans_label list ) 
    ((abs, labels):(ssl_validity_absdom * Nts_types.cnt_trans_label list )) 
     =
  (abs,prefix@labels)
  

class ssl_flatac_front_end = object(self) 
  inherit [ssl_validity_absdom , Nts_types.cnt_trans_label list ]  sem_and_logic_front_end
   

  val mid =  (new global_mem_manager )

  val mutable index_of_pointers_of_composite_types =
    Composite_types.create_index_of_composite_types ()
  val mutable index_of_composite_types_set = false
    
  method set_index_of_composite_types ( i : index_of_composite_types ) =
    if not index_of_composite_types_set then
      begin
	index_of_pointers_of_composite_types <- 
	  (Composite_types.copy_index_of_composite_types i );
	index_of_composite_types_set <- true
      end
    else ()
      
  method get_empty_transition_label () =
    []

  method get_entry_point_abstraction () =
     Ssl_valid_abs_dom.create_validity_abstdomain ()
 
  method get_entry_point_from_fundec (finfo : Cil_types.file) ( funinfo : Cil_types.fundec ) =
    if ( not index_of_composite_types_set  ) then
      raise IndexOfCompositeTypesNotSet
    else
      let absdom = Ssl_valid_abs_dom.create_validity_abstdomain () in
	Format.printf "[get°entry_point_from_fundec ]  New absdom : %s \n" (Ssl_pprinters.pprint_ssl_formula_tex absdom.ssl_part);
      absdom.composite_types_infos <- index_of_pointers_of_composite_types ; 
      let absdom = Var_registration.register_slocals mid funinfo absdom in
	Format.printf "[get°entry_point_from_fundec ]  Absdom after registering slocals : %s \n" (Ssl_pprinters.pprint_ssl_formula_tex absdom.ssl_part);
	let absdom = register_sformals mid funinfo absdom in
	  Format.printf "[get°entry_point_from_fundec ]  Absdom after registering sformals : %s \n" (Ssl_pprinters.pprint_ssl_formula_tex absdom.ssl_part);
	  let absdom = register_globals mid finfo absdom in
	    absdom


	
  method copy_transit_label _ =
    []

  method get_initialize_label () =
    mid#get_nts_transition_for_init_tab_size ()
  
  method copy_absdom_label (absval : ssl_validity_absdom ) =
     copy_validity_absdomain absval 
      

  method is_error_state (sslv : ssl_validity_absdom ) =
    match sslv.ssl_part.space with
	Top_heap -> true
      | _ -> 
	  begin 
	    if (Ssl_decision.sat_ssl sslv.ssl_part)
	    then  ( (Ssl_decision.garbage_ssl sslv.ssl_part) )
	    else false
	  end



(* this method tells whether one consider some Cil statements as
being error states.*)
  method is_control_state_erroneous (skind : Cil_types.stmtkind ) =
    match skind with
      |  Instr(Call( None , exp1, _ , _ ))->
           begin
             match  exp1.enode with
                 Lval((Var(f),_))  ->
                   begin
                     match f.vname with
                         "__assert_fail" ->
                           true
		       | _ -> false
		   end
	       | _ -> false
	   end

      |  Goto(stmt_ref,_) ->
	begin
	  let stmt = !stmt_ref in 
	  begin
	    let erroneous_label l =
	      match l with
		  Label("Error",_,true) ->
		    true    
		| _-> false
	    in
	    List.exists erroneous_label stmt.labels
	  end
	end
      | _ -> false


  method pretty (sslv : ssl_validity_absdom ) =
    Ssl_pprinters.pprint_ssl_formula_tex sslv.ssl_part

      
  method static_unsat_label (label : cnt_trans_label list) =
    Nts.static_check_if_translist_unsat label (* false*)

  method make_absdom_errorval (sslv : ssl_validity_absdom) =
    Ssl.set_heap_to_top sslv.ssl_part


 (* This method returns a value which type is :
 ( Cil_types.stmt, (ssl_validity_absdomain, cnt_trans_label_list )) list.
 This value encodes the transition relation form the switch statement.
 
 Invalid memory access are checked once to deal with the expt_test field
 of the Switch term, i.e. one transition to bot is generated and guarded
 thanks to the bad_mem_access_test Nts guard.

 Labels used in the case constructors are constant integers or 
 constant char, according to the ANSI-C specification, hence
 no guarded transition to an error state is generated for the
 "Case(expr)."

 The default "case" is guarded by the conjuction of the
 negation of all other cases guards -- Because there is no way
 to express execution order in the NTL.
 *)




(* Builds the (stmt * (`a,`b)) for all  default and general cases *)

  method private get_switch_case_succs sslv 
    (expr_switch_param : Cil_types.exp ) (stmt_succs : Cil_types.stmt list ) 
    =
    Format.fprintf Ast_goodies.debug_out "[In get_swithc_case_succs \n] %!";
  let guard_lhs = compile_cil_exp_2_cnt sslv expr_switch_param 
  in

  let rhs_case_condition_folder (local_guard : cnt_bool ) 
      (rhs : Cil_types.label)
      =
    match rhs with 
	Case(rhs_exp,_) ->
	  let rhs_compiled = 
	    compile_cil_exp_2_cnt sslv rhs_exp
	  in
	  let local_guard = 
	    CntBOr(local_guard,CntBool(CntEq,guard_lhs,rhs_compiled))
	  in local_guard
	  
      | _ -> local_guard
  in
  let compute_guard  (stmt : Cil_types.stmt) =
    Format.fprintf Ast_goodies.debug_out " [Entering compute guard \n] %!";
    if (stmt_has_default_label stmt) 
    then
    (* In this case one need to compute the guard for the
    default case. This case is redundant with the else
    case of the build_list_folder function defined below,
    this branch should never be executed in this algorithm.
       Raising an exception would be more suited b.t.w.
    *)
      begin
	Format.fprintf Ast_goodies.debug_out "[I am computing a default Guard] \n";
	compute_default_case_guard sslv expr_switch_param stmt_succs
      end
    else
      begin
	  (*The local guards do corresponds to disjunction
	  of the rhs = lhs, for all lhs that appear as a 
	  parametter of a case(lhs) :*)

	let local_guard =
	  List.fold_left rhs_case_condition_folder CntBFalse 
	    stmt.labels
	in
	local_guard   
      end
  in
  let build_list_folder ret_list (case_stmt : Cil_types.stmt ) =
    if not (has_default_label case_stmt.succs) then
      begin
	let local_guard_test =  compute_guard case_stmt in
	let local_guard = CntGuard(local_guard_test)::[] in
	(* Debug infos*)
	Format.fprintf Ast_goodies.debug_out "[Swich Guard ]: Case guard  is %s \n " (self#pretty_label local_guard);
	let local_absmem = copy_validity_absdomain sslv in
	let nexts_of_case = self#next local_absmem local_guard
	  case_stmt.skind in
	(case_stmt , nexts_of_case )::ret_list
      end
    else
      begin
	let local_guard_test = compute_default_case_guard 
	  sslv expr_switch_param stmt_succs in
	let local_guard = CntGuard(local_guard_test)::[] in
	(* Debug infos*)
	Format.fprintf Ast_goodies.debug_out "[Swich Guard ]: Default guard  is %s \n " (self#pretty_label local_guard);
	let local_absmem = copy_validity_absdomain sslv in
	let nexts_of_default = self#next local_absmem local_guard
	  case_stmt.skind in
	(case_stmt , nexts_of_default )::ret_list
      end
  in List.fold_left build_list_folder [] stmt_succs




  method next_on_switch_statement (sslv : ssl_validity_absdom )
    ( switch_stmt : Cil_types.stmt ) =
    Format.printf  "I'm entering next_on_switch_statment \n";
    match switch_stmt.skind with
	Switch(expr_test, block_sw , stmt_succs, _) ->
	  begin
	    let first_statement_succs = List.hd switch_stmt.succs in
	    let broken_mem_abs = 
	      self#copy_absdom_label sslv in
	    self#make_absdom_errorval broken_mem_abs;
	    let mem_access_cnd = Guard_of_mem_acces.cnt_guard_of_mem_access 
	      sslv expr_test in
	    let bad_mem_access_cnd = Nts.negate_cntbool_shallow 
	      mem_access_cnd in
	    let bad_mem_access_guard = CntGuard( bad_mem_access_cnd )::[] in
	    let mem_violated_case = (first_statement_succs,((broken_mem_abs,bad_mem_access_guard)::[]))  in
	   
	    (* Here we shall compute the transition for the general
	    case
	    *)
	    let switch_succs_cases = 
	      self#get_switch_case_succs sslv expr_test stmt_succs in
	    (mem_violated_case::switch_succs_cases)
	    (*switch_succs_cases*)
	  end

      | _-> raise  Not_a_switch_construct



  method next_on_if_statement (sslv : ssl_validity_absdom ) 
    (cdition : Cil_types.exp) =
    begin
      let abs_mem_broken1 = 
	self#copy_absdom_label sslv in
	self#make_absdom_errorval abs_mem_broken1;
      
      let abs_val_true = self#copy_absdom_label sslv in
      let abs_val_false = self#copy_absdom_label sslv in
      let cbool_cdition = cil_expr_2_bool cdition in
      (*let cbool_cdition = Nts.format_cntcond_for_cfg_condition cbool_cdition in
      let neg_cbool_cdition =  negate_bool_bot cbool_cdition in*)
      let mem_access_cnd = Guard_of_mem_acces.cnt_guard_of_mem_access 
	sslv cdition in
      let bad_mem_access_cnd =  Nts.negate_cntbool_shallow 
	mem_access_cnd in
	
      let nts_cdition = c_bool_to_cnt_bool 
	abs_val_true.ssl_part cbool_cdition 
      in
      (*let nts_cdition = Nts.format_cntcond_for_cfg_condition 
       nts_cdition 
      in*)

      let neg_of_nts_cdition = Nts.negate_cntbool_shallow nts_cdition
      in
      let valid_mem_nts_no = CntBAnd(neg_of_nts_cdition,mem_access_cnd) in
      let valid_mem_nts_yes = CntBAnd(nts_cdition,mem_access_cnd) in
      let nts_trans_yes = (abs_val_true ,(CntGuard(mem_access_cnd))::
	(CntGuardIf(nts_cdition)::[]))
      in	
      let nts_trans_mem_broken = (abs_mem_broken1,(CntGuard(bad_mem_access_cnd))::[])
      in
      let nts_trans_no = 
	(abs_val_false ,(CntGuard(mem_access_cnd))::(CntGuardElse(neg_of_nts_cdition)::[]))
      in
      let ret_true_false = (nts_trans_yes,nts_trans_no,nts_trans_mem_broken) in
      ret_true_false
    end
    
    

  method next (sslv : ssl_validity_absdom ) prefix_trans_label
    (skind : Cil_types.stmtkind) =
   (** we now need to copy the current sslf_formula of sslv. 
       validinfo is not a persistant structure, as it is based
       upon a standard library Map.
   *)
    let sslv_local =Ssl_valid_abs_dom.copy_validity_absdomain sslv in
    if List.length prefix_trans_label == 0  then
	C_upon_ssl_domain.next_on_ssl_nts mid sslv_local skind (* translist;*) 
    else
      let trans_with_suffix_labels = 
	C_upon_ssl_domain.next_on_ssl_nts mid sslv_local skind 
      in
      List.map (prefix_trans_label_list prefix_trans_label) 
	trans_with_suffix_labels

  (* Pretty prints each elements of tlist and concatenates it on the
  returned string.*)
  method pretty_label tlist = 
    let  str = List.fold_left pprint_trans_list_foldleft "" tlist in
      str
    
  method entails sslvg sslvd =
     let etp = {
      left = Ssl.copy sslvd.ssl_part ;
      right = Ssl.copy sslvg.ssl_part;
    }
    in
     (*(Ssl_entailement.accept_new_abstraction etp )*)
     not (accept_new_abstraction etp)

 
 
  method havocise_label l =
     let l = Nts.rewrite_ndet_assignation l
       in		
    Nts.havocise l


  method need_split_transition l =
    Nts.need_split_transition l

  method split_guard_call_transition translabel =
    let translabel = 
      Nts.rewrite_ndet_assignation translabel in
    Nts.split_guard_call_transition translabel


     
  method accepts_adder sslvg sslvd = 
     let etp = {
      left = Ssl.copy sslvg.ssl_part ;
      right = Ssl.copy sslvd.ssl_part;
    }
    in
    (*not (Ssl_entailement.does_entail etp )*)
    Ssl_entailement.entails_abstraction_adder etp




  method accepts sslvg sslvd =

    (** One checks that the current abstraction entails the next state
    abstraction, where the current state abstraction is sslfd and
    the next state abstraction is sslfg (Inverted order) *)
   
    let etp = {
      left = Ssl.copy sslvd.ssl_part ;
      right = Ssl.copy sslvg.ssl_part;
    }
    in
    (*not (Ssl_entailement.does_entail etp )*)
    Ssl_entailement.accept_new_abstraction etp



  method positive_guard_from_error_guard 
    (tlabel : cnt_trans_label list) =
    match tlabel with
	(CntGuard(g)::[]) ->
	  let ng = Nts.negate_cntbool_shallow g in
	  (CntGuard(ng)::[])
      | _ -> raise Not_a_mem_violation_guard



  method  number_of_valid_vars sslv = 
      Var_validity.cardinal_of_locmap sslv.validinfos

  method pprint_list_of_valid_var sslv =
    let index_id = ref 0
    in
    let pprint_valid_cnt_folder  vname _ prestr =
      let val_name = Nts.valid_name_of_var vname in 
      if !index_id != 0 then
	begin
	  index_id:=!index_id+1; 
	  prestr^(","^val_name)
	end
      else
	begin
	  index_id:=!index_id+1; 
	  prestr^(val_name)
	end
	  
    in
    let vmap =(
      match sslv.validinfos with
	  Validlocmap(lamap)-> lamap)
    in
    Validvarmap.fold pprint_valid_cnt_folder vmap ""


  method pprint_list_of_valid_locals_var sslv fundec=

    let is_slocal_valididty_var vname =
      List.exists (fun v-> 
	if (String.compare v.vname vname) == 0 
	then true 
	else false
      ) fundec.slocals 
    in
    let index_id = ref 0
    in
    let pprint_valid_cnt_folder  vname _ prestr =
      let val_name = Nts.valid_name_of_var vname in 
      if !index_id != 0 then
	begin
	  if is_slocal_valididty_var vname then
	    begin
	      index_id:=!index_id+1;
	      
	      prestr^(","^val_name)
	    end
	  else
	    prestr
	  end
      else
	begin
	  if is_slocal_valididty_var vname then
	    begin
	      index_id:=!index_id+1;
	      prestr^(val_name)
	    end
	  else
	    prestr
	end
    in
    let vmap =(
      match sslv.validinfos with
	  Validlocmap(lamap)-> lamap)
    in
    Validvarmap.fold pprint_valid_cnt_folder vmap ""



  method equals_labels lg ld =
    Nts.compare_tranlabel_list lg ld

  method pprint_list_of_malloc_vars () =
    mid#pprint_vars ()


end


