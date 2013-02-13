(*
Written by Florent Garnier, at Verimag Labs  2012 
Contact florent dot garnier at gmail dot com for  further informations.

This files is released under the terms of the LGPL v2.1 Licence.

 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor,
 Boston, MA  02110-1301  USA

*)



open Ntsint
open Nts_functor
open Nts_generic
open Nts_types
open Trace_types

(* Dotofintnts.dot_of_subcfg_of_nts nt_system trace_l *)


module Make =
  functor (Param : Nts_functor.NTS_PARAM ) ->
struct 



  module NFParam=Nts_functor.Make(Param)
  type anotations = NFParam.anotations
  type control = NFParam.control
    
  type nts_automaton = NFParam.nts_automaton
  type nts_system = NFParam.nts_system
    
  let control_out_of_string = Param.key_val_of_string
  

  type subsystem_call_count = (nts_automaton , int ref) Hashtbl.t
    

  open NFParam

  (**
     function nts_out_of_subrealtion :

      Pretty prints the collection of the transitions rules 
      that describe the transition relation. No variables definition,
      and no information concerning the various states are provides --
      no info concerning wheter which one are initial, final or 
      error state.

     main use : Called by nts_out_of_subrelation.
  *)
  
  let nts_out_of_subrelation subrel =
    NFParam.pprint_subgraph_transitions subrel
 

 (**  *)
  let get_tran s_relation_max_min (ca : nts_automaton) ( max : control ) 
      ( min : control ) =
    let subgraph = NFParam.subgraph_between ca max min 
    in nts_out_of_subrelation subgraph
  


  let get_contextual_transitions_of_subgraph context subgraph =
    let contextual_folder ctlist corg label cdest =
      (context,(corg,label,cdest))::ctlist
    in
    let inv_list = NFParam.fold_transitions_container 
       subgraph.sub_transitions contextual_folder  []
    in
    List.rev inv_list


  let debug_pprint_syscontrol sysc =
    match sysc with 
      Trace_types.Sys_control(c,s) ->
	Format.sprintf "Cautomaton :%s, state :%s" c s
	  
  let control_of_syscontrol sysc =
    match sysc with 
      Trace_types.Sys_control(_,s) ->
	let s_val = control_out_of_string s 
	in
	NFParam.Nts_State(s_val)
	  
	      
  let ca_name_of_syscontrol sysc =
    match sysc with 
      Trace_types.Sys_control(caname,_) -> caname
	
  let ca_of_syscontrol nt sysc =
    match sysc with 
      Trace_types.Sys_control(sysname,_) ->
	NFParam.get_cautomaton_by_name nt sysname
	  


  let rec is_transition_a_call l =
    match l with
      CntGenCall(sysname,opt_ret,params)::_ -> true
    | _::tl -> is_transition_a_call tl	
    | [] -> false
    


 
  let is_a_return ca_def (_,_,cdest) =
   NFParam.is_final_state ca_def cdest  
  
 
    
  let is_transition_relation_a_call corg label cdest =
    is_transition_a_call label

  (**
     
     nts_lib is collection of nts automaton that compose the nts
     library.
     
     nt is another library. It is reasonable to think that both
     libraries might be merged into a single one in a next version.
  *)
     
  let get_contextual_transition_list_from_pair_folder
      nts_lib nt  (pre_context_tlist,pre_sysc) curr_sysc =
    
    let _build_list_on_nts_param nts_param =
      match pre_sysc with
	None -> (pre_context_tlist,Some(curr_sysc)) (*No previous state visted*)
      | Some(Sys_control(ca_name,_) as prev_sysc ) 
	->
	begin
	  if (String.compare ca_name (ca_name_of_syscontrol curr_sysc))
	    != 0 
	  then  
	    begin
	     (* Format.printf "[Debug] context name change detected : Old context %s, new context %s \n %!" (debug_pprint_syscontrol prev_sysc) (debug_pprint_syscontrol curr_sysc);
	     *)
	      let max_c = control_of_syscontrol prev_sysc in
	      let ca = ca_of_syscontrol nt prev_sysc 
	      in
	      if not (is_a_return ca ((),(),max_c))
	      then 
		begin
		  try
		    let (dest,l) = get_one_transition ca max_c  in
		    (*in
		    Format.printf "[Debug] Funcall is Label is %s \n" (nts_pprint_gen_trans_label_list l);
		    *)
		    (pre_context_tlist@((ca,(max_c,l,dest))::[]),Some(curr_sysc))
		  with
		    Not_found -> 
		      
		      let control_name = NFParam.pprint_control max_c in 
		      Format.printf "No successor known for automata/state %s %s\n%!" ca.nts_automata_name control_name;
		      assert false 
		end
	      else
		(pre_context_tlist,Some(curr_sysc)) (* Current state and previous
						       one don't belong to the same 
						       subsystem. It is call return to a subsystem.*)		
	    end
	  else
	    begin
	      let max_c = control_of_syscontrol prev_sysc in 
	      let min_c = control_of_syscontrol curr_sysc in
	      let ca = ca_of_syscontrol nts_param curr_sysc in
	      if not (NFParam.is_successor_of ca max_c min_c) then
		begin
		  let subgraph = NFParam.subgraph_between_cond_on_edges 
		    ( fun corg l cdest -> not (is_transition_relation_a_call corg l cdest)) 
		   ca max_c min_c
		  in
		  let interval_content = 
		    get_contextual_transitions_of_subgraph ca subgraph
		  in
		  (pre_context_tlist@interval_content,Some(curr_sysc))
		end
	      else
		begin
		  let transition_labels = NFParam.get_transition_from
		    ca max_c min_c in
		  let interval_content = 
		    (
		      match transition_labels
		      with 
			Some(ll) ->
			  let label = List.hd ll in
			  ( ca , (max_c,label, min_c))::[]
			    
		      | None -> []
		    ) 
		  in
		  (pre_context_tlist@interval_content,Some(curr_sysc))
		end
	    end

	end 
    in
    try
      _build_list_on_nts_param nt
    with
      No_such_counter_automata_in_nts_system(_,_) ->
	_build_list_on_nts_param nts_lib
    | other_ex -> 
      raise other_ex    

	

  let contextual_transition_list_of_trace nts_lib nt tr =
    let (res,_)=
      List.fold_left 
	(get_contextual_transition_list_from_pair_folder nts_lib nt) 
	([],None) tr
    in
    res
  


(**
 A context is defined as a nts_cautomaton and a context_id.
*)


  let initial_context_of_ctl_list tr =
    let (ca,_) = List.hd tr in
    (ca,0)


  let nts_subsystem_of_ca_cid ca_name cid =
    Format.sprintf "%s_%d" ca_name cid


(** 
    Updates the name of the called subsystem so that it matches the
    subsystem generated from the trace itself.
*)

 
  let contextual_call_of_subsystem l (usid_counter : int ref) =
    (*Format.printf "contextual call of susbsystem id : %d \n%!"
      !usid_counter;*)
   
    let rec l_iterator accu ll =
    match ll with
      CntGenCall(sysname,opt_ret,params)::tl ->
	begin
	  (*Format.printf "[DEBUG] call function %s \n" sysname;*)
	  usid_counter := !usid_counter + 1;
	  let contextual_sysname = nts_subsystem_of_ca_cid sysname 
	    !usid_counter  
	  in
	  accu@(CntGenCall(contextual_sysname,opt_ret,params)::tl)
	end
    
    | h::tl -> l_iterator (accu@(h::[])) tl 
    | [] -> accu
    in
    l_iterator [] l

      
(** 
    In this function, the parameter nts_out nt_sytems_build_container 
    describes the generated transition system that will ultimately
    be exported for flatac.

    nts_lib is the set of functions that does not correspond to compiled
    C code and 
*)



  let rec get_called_subsystem_name l =
    match l with
      CntGenCall(sysname,_,_)::_ ->
	begin
	  sysname
	end	
	  
    | _::tl -> get_called_subsystem_name tl
    | []->  assert false
      
      
  let get_ca_by_name nts_lib nt name =
    try
      Hashtbl.find nt.nts_automata name
    with
      Not_found ->
	Hashtbl.find nts_lib.nts_automata name


  let definition_of_called_ca nts_lib nt l =
    let ca_name = get_called_subsystem_name l in
    get_ca_by_name nts_lib nt ca_name
	  
	  
  let new_context_table_entry catable uid ca_def=
    Hashtbl.add catable uid (ca_def,[])



  (** 
      l is the label of the current transition and tl is the tail of the
      trace list. Basically the head of tail is the next contextual operation.
  *)

  let rec is_transition_a_call_actually_called  l tl =
    if is_transition_a_call l 
    then
      begin
	match tl with
	  (ca,(corg,lnext,dest))::_ ->
	    begin
	      let called_subsystem = get_called_subsystem_name l in
	      let context_name_of_next_op = ca.NFParam.nts_automata_name in
	      called_subsystem = context_name_of_next_op
	    end
	| [] -> true
      end
    else
      false


  let is_context_switch_ahead curr_context_ca next_op_list =
    match next_op_list with
      (ca,_)::_ ->  (String.compare curr_context_ca.NFParam.nts_automata_name 
	  ca.NFParam.nts_automata_name) != 0
    | [] -> assert false

  let empty_tail tl =
    match tl with
      [] -> true
    | _ -> false

  let add_transtion_in_contextual_trans_sys context_table ca_param uid trans =
    if Hashtbl.mem context_table uid 
    then
      begin
	let (ca,tlist) = Hashtbl.find context_table uid in
	Hashtbl.replace context_table uid (ca,tlist@(trans::[]))
      end
    else
      begin
	Hashtbl.add context_table uid (ca_param,(trans::[]))
      end


(**)



  let context_table_pprinter tbl =
    let trans_folder pre (corg,l,cdest) =
    Format.sprintf "%s %s->%s{ %s } \n"
      pre
      (NFParam.pprint_control corg)
      (NFParam.pprint_control cdest) 
      (Nts_generic.nts_pprint_gen_trans_label_list l)
    in
    let _folder context_id (ca,tlist) prefix =
      let rules_printout = 
	List.fold_left trans_folder "" tlist 
      in
      Format.sprintf "%s %s_%d : %s \n" prefix ca.nts_automata_name context_id 
	rules_printout
    in
    Hashtbl.fold _folder tbl "" 
	
  let nts_of_transitions_rules_container tbl =
    let sys_table = Hashtbl.create 97 in
    let mapper (corg,l,cdest) =
     
      (corg,cdest,l)

    in
    let trans_table_iterator context_id (ca_def,tlist) =
      let tlist_map = List.map mapper tlist in
      let context_sysname = nts_subsystem_of_ca_cid ca_def.nts_automata_name 
	context_id in
      let subrel = transitions_container_of_trans_list tlist_map in
      let context_cautomaton = NFParam.cautomaton_of_transitions_container
	 context_sysname  ca_def subrel in
      Hashtbl.add sys_table context_sysname context_cautomaton
    in
    Hashtbl.iter trans_table_iterator tbl;
    sys_table
  
    


  let build_nts_table_from_contextual_trace nts_lib nt tr =
    
    (*Format.printf "[Debug] trace length : %d \n" (List.length tr) ;*)
    
    let context_uid = ref 0 in (* Add one to this variable each time
			       a call is performed.*)
    let context_table = Hashtbl.create 97 in (* (int  , ( ca, (control, trans list, control))) Hashtbl.t *)
    let contextual_transition_list = 
      contextual_transition_list_of_trace nts_lib nt tr in
    let current_context = initial_context_of_ctl_list 
      contextual_transition_list in 
    let context_stack = Stack.create () in
    Stack.push current_context context_stack;


    (*
    let pprint_ctl_ele e =
      match e with
	(ca,(corg,l,dest)) ->
	  Format.sprintf "context_name : %s, corg : %s -> cdest :%s, {label %s}" 
	    ca.NFParam.nts_automata_name 
	    (NFParam.pprint_control corg)
	    (NFParam.pprint_control dest)
	    (Nts_generic.nts_pprint_gen_trans_label_list l)
    in*)
    
    let rec build_ctl_iterator ctl = 
      (*Format.printf "[Debug] Iterating on ctl list, List length %d \n" (List.length ctl);*)
      let ( current_context_ca,current_cid) = 
	Stack.top context_stack 
      in
      (** 
	  Two transitions migth lead to a final state, that's why
	  I need to look ahead in the tail to be sure that the next
	  transition is not within the same context, before removing
	  the current context description from the stack.
      *)

      (*Format.printf "Current cid is %d \n " current_cid;*)
      
      match ctl with 	    
	(ca,((corg,l,dest) ))::tl ->
	  begin
	 
	   

	    if ( (is_transition_a_call_actually_called  l tl) && (not (empty_tail tl)) )
	    then 
	      begin 
		if (is_context_switch_ahead ca tl )
		then
		  begin
		    let called_subsystem_definition =  
		      definition_of_called_ca nts_lib nt l 
		    in
		    let l = contextual_call_of_subsystem l context_uid 
		    in
		    add_transtion_in_contextual_trans_sys 
		      context_table ca current_cid (corg,l,dest);
	       
		    let new_context = 
		      (called_subsystem_definition,!context_uid) in
		    Stack.push new_context context_stack;
	      (*
		Format.printf "[Debug]: Got a push \n"
	      *)
		  (* Create a new context, and push it on the top of
		 the stack *)
		  end
	      end
	    else
	      begin
		(*Format.printf "Transition is not a call \n";*)
		assert ((NFParam.is_state_in_cautomaton corg ca));	
		if (is_a_return ca ((),(),corg))
		then
		  assert false
		  (*begin
		    let _ = Stack.pop context_stack 
		    in 
		    ()
		  end*)
		else
		  (*begin
		    if not ( is_a_return ca ((),(),corg)) 
		  *)
		  begin 
		    add_transtion_in_contextual_trans_sys 
		      context_table ca current_cid (corg,l,dest) ;
		    
		   (* if ( is_a_return ca ((),(),dest)) 
		    then
		      begin
			if ( empty_tail tl) then
			  begin
			    let  _ = Stack.pop context_stack in 
			    ()
			  end
 			else*)
		    if ( empty_tail tl) then
		      ()
			
		    else if ( is_context_switch_ahead ca tl ) 	
		    then
		      begin
			(*
			Format.printf "[Debug] Got a pop \n";
			*)
			let _ = Stack.pop context_stack 
			in ()
		      end
		    
		    else 
		      ()
		 
		  end
	      end;
	    build_ctl_iterator tl (*recursion upon tail list.*)
	  end
	    
      | [] -> ()
    in
    build_ctl_iterator contextual_transition_list;

    (*let debug_out = 
      context_table_pprinter context_table
    in
    Format.printf "[Debug : CONTEXT TABLE] %s \n " debug_out;
    *)
    let nts_sys_table = 
      nts_of_transitions_rules_container context_table
    in
    nts_sys_table




  let nts_out_trace nts_lib nt tr =
    let trans_table = build_nts_table_from_contextual_trace nts_lib nt tr
    in
    let pre_ret =
    {
      NFParam.nts_system_name = "debug_trace_system";
      NFParam.nts_global_vars = nt.NFParam.nts_global_vars;
      NFParam.nts_automata = trans_table ;
      NFParam.nts_gvars_init = nt.NFParam.nts_gvars_init ;
      NFParam.nts_system_threads = None
    } in pre_ret
    (*in NFParam.nt_system_var_cleaner pre_ret*)
    

end;;







