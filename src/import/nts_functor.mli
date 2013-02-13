(*

Generic interface for numerical transitions systems.

(C) Verimag 2012

For questions and/or remarks :

 Write to florent dot garnier at imag dot fr


This files contains the implementation of the Numerical Transition Library 
--see http://richmodels.epfl.ch/ntscomp-- main objects, namely :

_ Numerical transitions subsystems, (i.e. parametric counter automaton
  with return values upon return)
_ Hyerarchical transistions subsystems .


Plus a parser, a pretty printer as well as cleanup functions.
A type checker will be added.



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

(**
This files provide a functorial interface for the numerical transition
system definition. 

Nts_functor.Make contains all types and functions definitions.
--Click on module Make to get a comprehensive list when browsing
the html API.
*)

open Nts_types
open Hashtbl
open Lexing 

exception UnboundVarName of string 
exception No_such_counter_automata_in_nts_system of string * string 

module type NTS_PARAM =
  sig
    type t         
      (*Type for key id of control states: e.g : int, string*)
    type anot_type (*Type for anotations*)
    val anot_parser : unit -> anot_type 
      (*Needed In case of anotation
	are using mutable container that
	need to be created, hashtbl for
	instance*)
      
    val pprint_keyid : t -> string (*Types for pprinting anotations*)
    val key_val_of_string : string -> t (* Generating an identifier value
					from a string.*) 
    val compare_keyid : t-> t -> int (* comparision function for keyid*)
    val pprint_anot : anot_type -> string

  
  end 



(** Signature definition of the Make functor. Types are abstracts.*)
module Make :
  functor( Param : NTS_PARAM ) ->
sig 
      
(** Type for anotations*)
  type anotations 

(** Type of a control state*)
  type control =  Nts_State of Param.t

(** Type used to encode transitions between control states *) 
  type transitions_container 
  type states_container  

(** Type used to encode the inverse of the
unlabelled successor relation transition
			      *)
  type inv_relation_container 

(** 

      'a is the type of the folded value.
      A nts transition is defined by a tuple of type 
      ( control * nts_gen_rel list * control ). A function of
      type ('a -> control -> nts_gen_rel list-> control -> 'a ) is
      required by this folder. e.g : type 'a = string for any pretty
      printting.
									        *)
  val fold_states_containers : states_container ->  ( 'a -> control -> 'a ) -> 'a -> 'a
  val fold_transitions_container : transitions_container ->  ('a -> control -> nts_trans_label list-> control -> 'a ) -> 'a -> 'a 
 
  val add_transition_to_container : transitions_container -> control -> nts_trans_label list -> control  -> unit

  val iter_transitions_container : transitions_container ->  ( control -> nts_trans_label list-> control -> unit ) -> unit 

  val iter_state_container : states_container -> ( control -> unit ) -> unit
 
    
  val is_state_in_inv_relation : inv_relation_container -> control -> bool
  val is_state_in_transition_container : control -> transitions_container -> bool

 

    
  (** counter automata with inputs and
      output variables and hierachical 
      calls enabled.
  *)
  type nts_automaton = {
    
    mutable nts_automata_name : string;
    mutable anot : anotations  ;
    
    init_states : states_container;
    final_states : states_container;
    error_states : states_container;
    
    input_vars : nts_genrel_var list; (*Variable ordering is important*)
    output_vars : nts_genrel_var list;
    local_vars : nts_genrel_var list;
    transitions : transitions_container;
   
  }

  (** Hierarchical numerical transition systems type definition *)
  type nts_system = {
        nts_system_name : string;
        nts_global_vars : nts_genrel_var list;
        nts_automata : ( string , nts_automaton ) Hashtbl.t;
	nts_gvars_init : Nts_types.nts_gen_relation list option;
        nts_system_threads : (string * Big_int.big_int) list option;
      }

  
(** Subrelation type definition.*)
  type num_subrel_in_cautomaton = {
    subrel_root : control ;
    sub_vertices : states_container;
    sub_transitions : transitions_container;
  }
  

  val is_state_in_cautomaton : control -> nts_automaton -> bool 
  val prepostfix_id_of_control : control -> string -> string -> control
 	
  val pprint_control : control -> string	
  val anot_parser : unit -> anotations
    
  val states_container_of_states_list : control list -> states_container  
  val transitions_container_of_trans_list : (control * control * Nts_types.nts_trans_label list ) list -> transitions_container
    
  
  (** Creates a control state type value from an identifier value.*)
  val control_of_id_param : Param.t -> control 
  
  (** Number of outgoing transition of a control state*)
  val out_degree_of_control_state :  control ->  nts_automaton -> int

  (**Number of incoming transitions in a control state*)
  val in_degree_of_control_state : control -> inv_relation_container -> int

  val get_varinfo_by_optname : nts_system -> string option -> string -> nts_genrel_var option 

  val get_varinfo_by_optcautomaton : nts_system -> nts_automaton option ->string -> nts_genrel_var option
    

  val is_error_state : nts_automaton -> control -> bool
  val is_initial_state : nts_automaton -> control -> bool
  val is_final_state : nts_automaton -> control -> bool 


    
  val get_transition_from :
    nts_automaton ->
    control -> control -> Nts_types.nts_trans_label list list option
  
  val get_successor_of : nts_automaton -> control -> states_container
  val get_one_state : states_container -> control option


  (*val get_outgoing_transit : nts_automatont -> control -> 
    (control * control * ())*)

  (** returns true iff the third argument is a successor of the second one
  in the automaton provided as first argument.*)
  val is_successor_of : nts_automaton -> control -> control -> bool

  (** Picks an outing transiton from control in the automaton*)
  val get_one_transition : nts_automaton -> control -> (control * Nts_types.nts_trans_label list) 
    
  val pprint_inputvars : nts_automaton  -> string
  val pprint_outputvars : nts_automaton  -> string 
  val pprint_localvars : nts_automaton  -> string
    
  (**
     computes a numerical transition system in which all local variables
     list of each automaton has been cleared of non used varibles
  *)
  val nt_system_var_cleaner : nts_system -> nts_system 


  (**
     Returns a nts system where all subsystems that does not appear
     in a call are removed.
  *)
  val nt_system_uncalled_subsystem_cleaner : nts_system -> nts_system 
  
  (** This function pretty prints a nts subsystem.*)
  val pprint_to_nts : nts_automaton -> string

  (** This function pretty prints a hierarchical transition system.*)
  val pprint_nts : nts_system -> string 
   
    
  val get_cautomaton_by_name : nts_system -> string -> nts_automaton
  val pprint_transitions : string -> nts_automaton -> string
  
  (** Compute the set of one step predecessors of all control states*)
  val compute_pred_relation : nts_automaton -> inv_relation_container

  (** Computes the subgraph between two control states*)
  val subgraph_between : nts_automaton -> control -> control -> num_subrel_in_cautomaton

  (** Computes the subgraph between two control states where each transition
  lable complies with the function provided as first parameter.*)
  val subgraph_between_cond_on_edges :
    (control -> Nts_types.nts_trans_label list -> control -> bool) ->
    nts_automaton -> control -> control -> num_subrel_in_cautomaton

  (** Pretty prints a subgraph using the nts syntax on control states
  and labels*)
  val pprint_subgraph_transitions : num_subrel_in_cautomaton -> string


  (** Creates a new couter automaton, using the definition of a cautomaton
  -2nd argument- and a subrelation -3rd argument, and call it using the
  name provided in the first argument.*)
  val cautomaton_of_subrelation_cautomaton : string -> nts_automaton -> num_subrel_in_cautomaton -> nts_automaton

  (** Creates a subsystem from a nts_automaton and a transition container.*)
  val cautomaton_of_transitions_container : string -> nts_automaton -> transitions_container -> nts_automaton


  (** Types and functions used to generate a control flow graph
      from the numerical transition system description*)
  type nts_type_basic_block = Nts_branching_block
			     | Nts_standard_block    


  (** 
    Type definition for block encoding. Here, a basic block
      is defined using a label, a head control state, a sequence
      of transition and a set of successor, coded as 
      (blocks references * transition labels).
  *)
  type nts_basic_block = {
    block_head_label : string ;
    block_head_state : control;
    block_type : nts_type_basic_block ;
    mutable block : (control * nts_trans_label list * control) list; 
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
    
  (**
     Block encoding of a subsystem.
  *)   
  type nts_automaton_cfg = {
    mutable nts_cfg_name : string; 
    mutable cfg_anot : anotations;
    (*states : (control , unit ) Hashtbl.t;*)
    nts_cfg_init_block : (string , nts_basic_block ) Hashtbl.t;
    nts_cfg_final_block : (string , nts_basic_block ) Hashtbl.t;
    nts_cfg_error_block : (string , nts_basic_block ) Hashtbl.t;
    nts_input_vars : nts_genrel_var list; (*Variable ordering is important*)
    nts_output_vars : nts_genrel_var list;
    nts_local_vars : nts_genrel_var list;
    nts_blocks_transitions : ( string , nts_basic_block ) Hashtbl.t
  }

  val get_last_control_state_of_bblock : nts_basic_block -> control
  
  (** Computes the block encoding of a transition system.*)
  val blocks_compression_of_nts_automaton : nts_automaton -> nts_automaton_cfg 

end

