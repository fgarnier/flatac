
(*
This files contains the implementation of a functor that allows
to perform a translation from the 
Numerical Transition System library language to the Interproc SPL
language.

This language eases the translation of a transition systems to
the SPL language, where instructions are regrouped into basic
blocks and where the programs is represented as a listing.

This intermediate language aims at focusing on the translation
from the tansitions sytems to the control flow structure which
is proper to SPL.
A second phase consists in translating the variables, arithmetical
expressions and pressburger expressions from the NTL languague to
BDD Apron expressions.


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




open Nts_types
open Nts_spl_intermediate_language_types


module Make_NtsCfg =
  functor (Param : Nts_functor.NTS_PARAM) ->
   struct
      module NtsSys = Nts_functor.Make(Param)
     
      type control = NtsSys.control
      type anotations = NtsSys.anotations
      type block_labels = string
      
      module Block_lab_order = 
      struct
	type t = string
        let compare = String.compare
      end
	

      type nts_basic_block = {
        mutable head_label : block_labels;
        mutable block : (Nts_types.nts_trans_label list) list;
        mutable block_succs :
          (nts_basic_block ref * Nts_types.nts_trans_label list) list option;
      }
	  
      type nts_function_cfg = {
        mutable nts_cfg_name : string;
        mutable cfg_anot : anotations;
        nts_cfg_blocks : (string, unit) Hashtbl.t;
	
        nts_cfg_final_block : (string, unit) Hashtbl.t;
        nts_cfg_error_block : (string, unit) Hashtbl.t;
        nts_input_vars : Nts_types.nts_genrel_var list;
        nts_output_vars : Nts_types.nts_genrel_var list;
        nts_local_vars : Nts_types.nts_genrel_var list;
        nts_blocks_transitions : (string, nts_basic_block) Hashtbl.t;
      }

      type nts_functional_program = {
	nts_fun_glob_vars : nts_genrel_var list;
	nts_fun_function_definitions : nts_function_cfg list option;
	nts_fun_gvars_init : nts_gen_relation list option; 
	nts_fun_function_main : nts_function_cfg;

	nts_fun_system_threads : 
	  ( string * Big_int.big_int ) list  option;   
	
      }

      (** This type need not to be defined in the interface. *)
(*
      type coalescing_parameter = {
	control_label_map : +block_labels Map.Make(block_lab_order).t;
	
	
      }
*)

(*
      let nts_cfg_of_automaton ( : NtsSys.nts_automaton )=  nts_function_cfg
      let nts_cfg_of_nts_sys (: NtsSys.nts_system ) = nts_function_cfg
*)  
  end





module Make =
  functor ( Param :  Nts_functor.NTS_PARAM ) ->
struct
 
  
  (** Types and functions used to generate a control flow graph
      from the numerical transition system description*)

  module NtsSys = Nts_functor.Make(Param)
  
  type nts_automaton = NtsSys.nts_automaton
  type control = NtsSys.control
  type anotations = NtsSys.anotations


  exception Cant_be_head_of_basic_block of control
  exception Nts_i_have_a_binding

  type nts_basic_block = {
    mutable head_label : string ;
    mutable block : (control * nts_trans_label list) list; 
    (** Current control state,
	nts_trans_label_list corresponds
	to what changes/is called before
	transiting*)
    
    mutable block_succs : ( nts_basic_block ref * nts_trans_label list ) list option;
  (** transitions between blocks. Nexts blocks and the transisions being
      described.
      None is in the case the last control state is an error state.
      It's also a convenience for the buiding process.   
  *) 
  } 
      
      
  type nts_automaton_cfg = {
    mutable nts_cfg_name : string; 
    mutable cfg_anot : anotations;
    (*states : (control , unit ) Hashtbl.t;*)
    nts_cfg_init_block : (string , unit ) Hashtbl.t;
    nts_cfg_final_block : (string , unit ) Hashtbl.t;
    nts_cfg_error_block : (string , unit ) Hashtbl.t;
    nts_input_vars : nts_genrel_var list; (*Variable ordering is important*)
    nts_output_vars : nts_genrel_var list;
    nts_local_vars : nts_genrel_var list;
    nts_blocks_transitions : ( string , nts_basic_block ) Hashtbl.t
  }



  
    
  (** Generic function that returns one binding in a Hashtbl provided
  there exists one --Some ( key , value)-- and returs None if none
  if the Hashtbl is empty. *)

  let pick_elem_in_hashtbl tbl =
    let gen_binding = ref None in
    let get_first_elem_iterator a b =
      gen_binding := Some( ( a , b ) );
      raise Nts_i_have_a_binding
    in
    try 
      Hashtbl.iter get_first_elem_iterator tbl;
      !gen_binding
    with
	Nts_i_have_a_binding -> !gen_binding
	  


  
 (* Anything below is work in progress and need to be redesigned *)



  (* Creates a basic block header from a control state, the latter must
  have one successor at most, else an exception is raised.*)


  let create_basic_block (control_state : control ) ( label : string ) 
    (cautomaton : nts_automaton ) =

    if (NtsSys.out_degree_of_control_state control_state cautomaton) <> 1 then
      raise (Cant_be_head_of_basic_block ( control_state ))
    else
      begin
	

	let block_head = 
	  NtsSys.get_one_transition cautomaton control_state
	in
	
  	{ 
	  head_label = label;
	  block  = block_head :: [] ;
	  block_succs = None;
 	}
	
      end

  (* Create one block per initial states successor. The blocks are not
     filled entirely, moreover the initial states are not marked.
  *)


(*	
  let add_init_states_to_cblocks_queue nts_automaton q 
      (label_id : int Pervasives.ref ) =
    
    let init_state_rel_iterator control _ =
      let label =  Format.sprintf "lab%d" !label_id in
      label_id := !label_id + 1;
      let init_block = create_basic_block control label nts_automaton
      in
      Queue.push init_block q
    in
    Hashtbl.iter init_state_rel_iterator nts_automaton.NtsSys.init_states
    

  type visited_table = Visited of (control, unit ) Hashtbl.t
  type label_index = Label_index of (string , control ) Hashtbl.t
  type control_label_index = Control_label of (control , string ) Hashtbl.t
  type block_control_index = Control_block_index of ( control, nts_basic_block ) Hashtbl.t
  type block_label_index = Label_block_index of (string , nts_basic_block ) Hashtbl.t



  (** Returns the number of control states registered as a predecessor
  of the control state "state" given as a parameter. *)


      
  let in_degree_of_control_state state pre_relation =
    try 
      let matches = Hashtbl.find_all pre_relation state in
      List.length matches
    with
	Not_found -> 0
	
       
*)  
    
    
    
(*
  
  let label_contol_state control =
  
  
  let build_basic_block_for_branching_statement 
  bblock (vtable : visited_table ) 
      (lindex : label_index) (cindex : control_lablel_index )
  (bindex : block_index) (pred_relation : (control , unit) Hashtbl.t )
  cautomaton 
  (label_id : int ref ) = 
*)  
    
(* 
   Creates, label and chain a block per branch. Mark them as visited
   Updates the lindex and bindex with the newly created control 
   blocks.
*)
    
(*
  let sequentialize_branching control_state_org 
  control_state_dest nts_label block_list =
  
  let  
  
  let successor_table = 
  Hashtbl.find control_state cautomaton.transitions in
*)    
    
    
(*
  Takes as input a basic block header and completes the list
  of couples ( controls * nts_transition list) and 
  returns the filled basic block plus the 
  list of the basic blocks headers that have not
  been visited that succeeds the currently
  visited block
  
*)
    
(*
  let fill_basic_block bblock (vtable : visited_table ) 
  (lindex : label_index) (cindex : control_lablel_index )
  (bindec : block_index) (pred_relation : (control , unit) Hashtbl.t )
  cautomaton 
  (label_id : int ref )  = 
  
  let rec add_elem_of_segment current_control =
  if ( out_degree_of_control_state current_control cautomaton = 1 
	  && in_degree_of_control_state current_control pred_relation = 1 )
  then
  begin
  let out_relation = Hashtbl.find 
	    cautomaton.transitions current_control in
	  let (ctr, trans_list ) =  pick_elem_in_hastbl out_relation in
	  bblock.block <- (bblock.block @ (ctr,trans_list)) ;
	  add_elem_of_segment ctr
	end
      else
	(* In this case, the current element given as parameter, is
	the first element of another block. It is either branching
  or have many predecessors*)
  let 
  
  in
*)

(*
  let cfg_of_nts_automaton ca =
    (*One shall memorize wich are the control strates
    that have aleady been traversed.*)
    let visited_states = Hashtbl.create 97 in    
    (* Labelleling statement with are at the begining of
       a basic block.*)
    let label_statement = Hashtbl.create 97 in
    (* Unique id counter for labelling basic blocks*)
    let counter_label = ref 0 in
    (* Basic blocks*)
    let blocks_index = Hashtbl.create 97 in
    (* Blocks being built *)
    let current_blocks = Queue.create () in
    add_init_states_to_cblocks_queue ca current_blocks;
    
    (* Initial states are marked for not being traversed again.*)
    
    let mark_init_as_visited_iterator control _ =
      Hashtbl.add visited_states control ()
    in
    Hashtbl.iter mark_init_as_visiter_iterator ca.initial_states ;
       
*)  
      

end







