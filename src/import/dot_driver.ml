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


open Nts_functor
open Nts_types
open Hashtbl
open Nts_generic
open Trace_types


module Make = 
  functor ( Param  : Nts_functor.NTS_PARAM ) ->
    struct
      module NFParam=Nts_functor.Make(Param)
      type anotations = NFParam.anotations
      type control = NFParam.control
	  
      type nts_automaton = NFParam.nts_automaton
      type nts_system = NFParam.nts_system
	
      let control_out_of_string = Param.key_val_of_string
      open NFParam


      let pprint_control = NFParam.pprint_control
	
      let rec get_opt_transition ntlist =
	match ntlist with
	    (CntGenCall ( _, _, _) as h)::l -> Some(h)
	  | _::l -> get_opt_transition l
	  | [] -> None
	    
	       
	       
	    

      let dot_of_init_nodes (ca : nts_automaton) =
	let init_state_printer  prefix control =
	  Format.sprintf "%s %s_%s [style=filled,color=blue];\n" 
	    prefix ca.NFParam.nts_automata_name  (pprint_control control) 
	in
	NFParam.fold_states_containers 
	  ca.init_states init_state_printer  ""
	  



      let dot_of_final_nodes (ca : nts_automaton) =
	let final_state_printer prefix control =
	  Format.sprintf "%s%s_%s[style=filled,color=green];\n" 
	    prefix ca.NFParam.nts_automata_name (pprint_control control) 
	in
	NFParam.fold_states_containers 
	  ca.NFParam.final_states final_state_printer  ""
	  

      (** Prints error states in red.*)  
      let dot_of_error_nodes_reach_upb (ca : nts_automaton ) invtable =
	let error_printer  prefix control =
	  if NFParam.is_state_in_inv_relation invtable control 
	  then
	    begin
	      Format.sprintf "%s %s_%s [style=filled,color=red];\n" 
		prefix ca.NFParam.nts_automata_name  (pprint_control control)
	    end
	  else
	    prefix
	in
	NFParam.fold_states_containers 
	  ca.NFParam.error_states error_printer ""
	  
	  
      let dot_of_error_nodes (ca : nts_automaton) =
	let in_folder  prefix control =
	  Format.sprintf "%s%s_%s[label=\"error\",color=red];\n" 
	    prefix ca.NFParam.nts_automata_name (pprint_control control) 
	in
	NFParam.fold_states_containers 
	  ca.NFParam.final_states in_folder  ""
	  
	  
	  
(** subrel is a subset of the transition of the counter automaton ca.
 gv color is an optional string.
*)
      let dot_of_subgraph ?(color="greenyellow") prefix ca  subrel =
	let transition_printer prefix control_org l control_dest =   
	  Format.sprintf "%s%s_%s->%s_%s[color=%s];\n"
	    prefix 
	    ca.NFParam.nts_automata_name 
	    (pprint_control control_org) 
	    ca.nts_automata_name (pprint_control control_dest)
	     color
	in
	NFParam.fold_transitions_container 
	  subrel transition_printer prefix
	

    (** Transition printer folder*)
      let transition_printer ca prefix control_org l control_dest =
	let opt_call = get_opt_transition l in
	  match opt_call with
	    None ->
		Format.sprintf "%s%s_%s->%s_%s;\n" prefix 
		  ca.NFParam.nts_automata_name 
		  (pprint_control control_org) 
		  ca.nts_automata_name (pprint_control control_dest)
	    | Some(CntGenCall ( sysname, _, _)) -> 
	      begin
		Format.sprintf "%s%s_%s->%s_%s[color=red,label=\"call to %s \"];\n" prefix 
		  ca.NFParam.nts_automata_name 
		  (pprint_control control_org) 
		  ca.nts_automata_name (pprint_control control_dest)
		  sysname
	      end
	    | Some(_) -> assert false
	      


      (** Interprocedural calls are represented using a red transition 
      label.*)
      let dot_of_transitions  (ca : nts_automaton ) prefix =
	
	let transition_printer = transition_printer ca in
	NFParam.fold_transitions_container 
	  ca.transitions transition_printer prefix
	  

      let dot_of_cautomaton ?(standalone_graph = false) (ca : nts_automaton )=
	
	let ret_str = ( if standalone_graph then
	    Format.sprintf " digraph %s {" ca.nts_automata_name 
	  else 
	    Format.sprintf " subgraph cluster_%s { \n color=blue; label=\"%s\";" ca.nts_automata_name ca.nts_automata_name
	)
	in
	
	let invtable = NFParam.compute_pred_relation ca 
	in
	
	let res = Format.sprintf "%s %s %s %s %s }" ret_str
	  (dot_of_init_nodes ca) (dot_of_error_nodes_reach_upb ca invtable)
	  (dot_of_final_nodes ca) (dot_of_transitions ca "")
	in 
	res
	
	

      let dot_of_nts (nt : nts_system ) =
	let automata_folder name caut pre_str =
	  Format.sprintf "%s\n%s" pre_str (dot_of_cautomaton caut)
	in
	let automata_dump = 
	  Hashtbl.fold automata_folder nt.nts_automata "" 
	in
	let ret_string = 
	  Format.sprintf "digraph %s { %s }" nt.nts_system_name automata_dump
	in
	ret_string
	

      let pprint_sys_control s =
	match s with
	    Trace_types.Sys_control(sname,cname) ->
	      sname^"_"^cname 
		 

      let pprint_trace_tansitions tr =
	let pprint_trace_transitions_folder (prefix_printer,previous_state) 
	    curr_control =
	  match previous_state with 
	      None -> 
		("",Some(curr_control))
	    | Some(prev) -> 
	      begin
		let out_string = Format.sprintf "%s %s -> %s [color=gold]\n" prefix_printer 
		(pprint_sys_control prev) (pprint_sys_control curr_control) in
		(out_string,Some(curr_control))
	      end
	in
	let  (ret_string, _ ) = 
	  List.fold_left pprint_trace_transitions_folder ("",None) tr 
	in
	ret_string
	  
	
      let dot_of_trace_upon_nts (nt : nts_system ) (tr : Trace_types.trace ) =
	let automata_folder name caut pre_str =
	  Format.sprintf "%s\n%s" pre_str (dot_of_cautomaton caut)
	in
	let automata_dump = 
	  Hashtbl.fold automata_folder nt.nts_automata "" 
	in
	let ret_string = 
	  Format.sprintf "digraph %s { %s" nt.nts_system_name automata_dump
	in
	let trace_transitions =  pprint_trace_tansitions tr in
	Format.sprintf "%s %s } " ret_string trace_transitions


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


      let highlight_graph_between  (ca : nts_automaton) (max : control) 
	  ( min : control) =
	let subgraph = subgraph_between ca max min 
	in
	dot_of_subgraph "" ca subgraph.sub_transitions 


      (** Computes and then concatenates the sequence of arrows that
      belong to the different control points of a trace, whenever
      two or more belong to the same nts automaton. If  *)

      let pprint_subgraph_between_ctr_pair_folder 
	  nt (pre_str,pre_sysc) curr_sysc =
	
	match pre_sysc with
	  None -> (pre_str,Some(curr_sysc)) (*No previous state visted*)
	| Some(Sys_control(ca_name,_) as prev_sysc ) 
	  ->
	  begin
	    if (String.compare ca_name (ca_name_of_syscontrol curr_sysc))
	      <> 0 
	    then  (pre_str,Some(curr_sysc)) (* Current state and previous
					       one don't belong to the same 
					       subsystem*)
	    else
	      begin
		try
		  let max_c = control_of_syscontrol prev_sysc in 
		  let min_c = control_of_syscontrol curr_sysc in
		  let ca = ca_of_syscontrol nt curr_sysc in
		  if not (NFParam.is_successor_of ca max_c min_c) then
		  begin
		    let gprint_out = highlight_graph_between ca max_c min_c
		    in
		    let suffix = Format.sprintf "%s%s\n" pre_str gprint_out in
		    (suffix,Some(curr_sysc))
		  end
		  else
		    begin
		      let transition_labels = NFParam.get_transition_from
			ca max_c min_c in
		      let print_out = 
			(
			  match transition_labels
			  with 
			    Some(ll) ->
			      let label = List.hd ll in
			      transition_printer ca pre_str max_c label min_c
			
			  | None -> ""
			) 
		      in
		      (print_out,Some(curr_sysc))
		    end
		    
		with
		  No_such_counter_automata_in_nts_system(_,_) ->
		    (pre_str,None)
		  | other_ex -> raise other_ex    
	      end
	  end


      let dot_of_subcfg_of_nts (nt : nts_system ) ( tr : Trace_types.trace ) =
	
	let automata_folder name caut pre_str =
	  Format.sprintf "%s\n%s" pre_str (dot_of_cautomaton caut)
	in
	let automata_dump = 
	  Hashtbl.fold automata_folder nt.nts_automata "" 
	in
	let ret_string = 
	  Format.sprintf "digraph %s { %s" nt.nts_system_name automata_dump
	in
	let (printout_hgraph,_) = 
	  List.fold_left (pprint_subgraph_between_ctr_pair_folder nt ) 
	    ("",None) tr
	in
	Format.sprintf "%s%s}" ret_string printout_hgraph

	

      (*
	In this section, we define the set of functions that
	allows to draw the control flow graph from a numerical
	transition system. 
      *)
	  

      let rename_control_id_by_block_type  bblock cstate =
	(*
	Format.printf " control label : %s  ; bblock.block_label_name : %s \n ================= " (pprint_control cstate)   bblock.block_head_label;
	*)
	prepostfix_id_of_control cstate (bblock.block_head_label^"_") ""

      type nts_block_type = Block_transition
			    | Error_block
			    | Init_block
			    | Final_block


      let decorate_block_by_type ntblock_type =
	match ntblock_type with
	  Block_transition -> ""
	| Error_block -> "[color=red]"
	| Init_block -> "[color=blue]"
	| Final_block -> "[color=green]"

      let dot_of_basic_block nts_cfg ?(block_type = Block_transition) bblock =
	let block_opt_by_type = decorate_block_by_type block_type 
	in
	let trans_block_print_folder prefix (corg,_,cdest) =
	  let corg = rename_control_id_by_block_type bblock corg in 
	  (* prepostfix_id_of_control corg (bblock.block_head_label^"_") "" in *)
	  let cdest = rename_control_id_by_block_type  bblock cdest in
	  (*prepostfix_id_of_control cdest (bblock.block_head_label^"_") "" 
	  in *)  

	  Format.sprintf "%s %s->%s %s;\n" 
	    prefix 
	    (pprint_control corg)  (pprint_control cdest) block_opt_by_type
	in
	let inner_block_transitions  = 
	  List.fold_left trans_block_print_folder "" bblock.block in
	let block_out = Format.sprintf "subgraph cluster_%s { \n %s; label=\"%s\"; %s }" bblock.block_head_label block_opt_by_type bblock.block_head_label inner_block_transitions in
	block_out


	  
      let get_list_of_opt_list ol =
	match ol with 
	  None -> []
	| Some(l) -> l

      let link_basic_blocks_of_nts_cfg nts_cfg =
	let link_block_table_folder  _ bblock pre =

	  let block_link_folder curr_block curr_block_last_cstate 
	      pre (bref,_) =
	    let next_block = !bref 
	    in
	    let curr_block_last_cstate = rename_control_id_by_block_type 
	      bblock curr_block_last_cstate in
	    let next_block_hstate = rename_control_id_by_block_type 
	      next_block next_block.block_head_state   in
	    Format.sprintf "%s  %s -> %s [color=\"red\"]; \n" pre
	      (pprint_control curr_block_last_cstate)  
	      (pprint_control next_block_hstate)   
	  in    
	  let curr_block_last_cstate = 
	    NFParam.get_last_control_state_of_bblock bblock 
	  in
	  List.fold_left (block_link_folder bblock 
			    curr_block_last_cstate) pre 
	    ( get_list_of_opt_list bblock.block_succs) 
	in
	let print_out = 
	  Hashtbl.fold link_block_table_folder  nts_cfg.nts_cfg_init_block "" 
	in
	let print_out =
	  Hashtbl.fold link_block_table_folder  nts_cfg.nts_blocks_transitions print_out
	in
	print_out
      
	  

      let dot_of_nts_cfg ?(standalone = true) nts_cfg = 
	
	let dot_table_folder ?(block_type = Block_transition) vlabel bblock pre =
	  Format.sprintf "%s \n %s" pre (dot_of_basic_block nts_cfg ~block_type:block_type bblock)
	in
	let print_out = Hashtbl.fold (dot_table_folder ~block_type:Init_block) nts_cfg.nts_cfg_init_block "" in
	let print_out =  Hashtbl.fold (dot_table_folder ~block_type:Final_block) nts_cfg.nts_cfg_final_block print_out in
	let print_out =  Hashtbl.fold (dot_table_folder ~block_type:Error_block) nts_cfg.nts_cfg_error_block print_out in
	let print_out = Hashtbl.fold dot_table_folder nts_cfg.nts_blocks_transitions print_out in
	let print_inter_block_links = link_basic_blocks_of_nts_cfg nts_cfg in
	let header = 
	  ( 
	    if standalone then "digraph"
	    else "subgraph"
	  ) 
	in
	Format.sprintf "%s %s { %s\n %s }\n" header nts_cfg.nts_cfg_name print_out print_inter_block_links



      let dot_of_all_subsystem_of_nts nts = 

	let all_dot_folder _ cautomaton pre =
	  let nts_cfg = NFParam.blocks_compression_of_nts_automaton 
	    cautomaton in
	  let dot_out = dot_of_nts_cfg ~standalone:false nts_cfg in
	  Format.sprintf "%s %s \n" pre dot_out 
	in
	let dot_out = Hashtbl.fold all_dot_folder nts.NFParam.nts_automata
	  "" in
	Format.sprintf "digraph blocks_of_%s { %s }" 
	  nts.NFParam.nts_system_name dot_out
	  
	  
	
end;;
