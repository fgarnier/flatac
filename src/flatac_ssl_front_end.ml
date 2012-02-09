(**
This file specializes the generic front end, in order to extract a
counter automata based model which nodes are labelled with values
of the SSL logic. 
*)
(*open Intermediate_language*)

exception IndexOfCompositeTypesNotSet

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



let pprint_trans_list_foldleft (s : string ) ( trans : cnt_trans_label ) =
  match (s,trans) with 
    | ("",CntGuard(guard))-> 
      (*let s_guard = simplify_cnt_boolexp guard in*)
      let s_guard = guard in
      begin
	match s_guard with 
	    CntBTrue -> ""
	  | _ -> cnt_pprint_boolexp s_guard
      end
    | ("",_) ->
      (cnt_pprint_translabel trans )
    | (_,CntGuard(guard)) -> 
      (*let s_guard = simplify_cnt_boolexp guard in*)
      let s_guard=guard in
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
	    match stmt.labels with
		Label("Error",_,true)::_ ->
		  true
		    
		    
		    
	      | _-> false
	  end
	end

      | _ -> false


  method pretty (sslv : ssl_validity_absdom ) =
    Ssl_pprinters.pprint_ssl_formula_tex sslv.ssl_part

      
  method static_unsat_label (label : cnt_trans_label list) =
    (*Nts.static_check_if_translist_unsat label*) false

  method make_absdom_errorval (sslv : ssl_validity_absdom) =
    Ssl.set_heap_to_top sslv.ssl_part

  method next_on_if_statement (sslv : ssl_validity_absdom ) 
    (cdition : Cil_types.exp) =
    begin
      let abs_mem_broken1 = 
	Ssl_valid_abs_dom.copy_validity_absdomain sslv in
	Ssl.set_heap_to_top abs_mem_broken1.ssl_part;
      
      let abs_val_true = Ssl_valid_abs_dom.copy_validity_absdomain 
	sslv in
      let abs_val_false = Ssl_valid_abs_dom.copy_validity_absdomain 
	sslv in
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
      let nts_cdition = Nts.format_cntcond_for_cfg_condition 
       nts_cdition 
      in
      let neg_of_nts_cdition = Nts.negate_cntbool_shallow nts_cdition
      in
      let valid_mem_nts_no = CntBAnd(neg_of_nts_cdition,mem_access_cnd) in
      let valid_mem_nts_yes = CntBAnd(nts_cdition,mem_access_cnd) in
      let nts_trans_yes = (abs_val_true ,(CntGuard(valid_mem_nts_yes))::[])
      in	
      let nts_trans_mem_broken = (abs_mem_broken1,(CntGuard(bad_mem_access_cnd))::[])
      in
      let nts_trans_no = 
	(abs_val_false ,(CntGuard(valid_mem_nts_no))::[])
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
    Nts.havocise l


  method need_split_transition l =
    Nts.need_split_transition l

  method split_guard_call_transition translabel =
    let translabel = 
      Nts.rewrite_ndet_assignation translabel in
    Nts.split_guard_call_transition translabel

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


  method  number_of_valid_vars sslv  = 
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


