(**

Generic interface for numerical transitions systems.

(C) Verimag 2012

For questions and/or remarks :

 Write to florent dot garnier at imag dot fr


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
    val compare_keyid : t-> t -> int (* comparision function for keyid*)
    val pprint_anot : anot_type -> string
  end 



(** Signature definition of the Make functor. Types are abstracts.*)
module Make :
  functor( Param : NTS_PARAM ) ->
sig 
      
  type anotations (** Type for anotations*)
  type control   (** Type of a control state*)
  type nts_automaton = (** counter automata with inputs and
			 output variables and hierachical 
			 calls enabled.
		     *) 
	{
        mutable nts_automata_name : string;
        mutable anot : anotations  ;
        (*states : (control , unit ) Hashtbl.t;*)
        init_states : (control , unit ) Hashtbl.t;
        final_states : (control , unit ) Hashtbl.t;
        error_states : (control , unit ) Hashtbl.t;
        input_vars : nts_genrel_var list; (*Variable ordering is important*)
        output_vars : nts_genrel_var list;
        local_vars : nts_genrel_var list;
        transitions : (control, (control , nts_trans_label list ) Hashtbl.t) Hashtbl.t ;

	}

  type nts_system = (** Hierarchical numerical transition systems *)
      {
        nts_system_name : string;
        nts_global_vars : nts_genrel_var list;
        nts_automata : ( string , nts_automaton ) Hashtbl.t;
	nts_gvars_init : Nts_types.nts_gen_relation list option;
        nts_system_threads : (string * Big_int.big_int) list option;
      }
  

  val pprint_control : control -> string	
  val anot_parser : unit -> anotations

  (*val rename_nts_system : nts_system -> string -> unit*)
  val control_of_id_param : Param.t -> control
  (*val create_nts_cautomaton : unit -> nts_automaton (* Creates a new structure
					nts_automata*)*)
  (*val add_nts_int_vars_to_nts_system : nts_system -> string list -> unit 
  val add_nts_real_vars_to_nts_system : nts_system -> string list -> unit*) 
  (* string option check in a subsystem; string var name *)
  val get_varinfo_by_optname : nts_system -> string option -> string -> nts_genrel_var option 

  val get_varinfo_by_optcautomaton : nts_system -> nts_automaton option ->string -> nts_genrel_var option
 
    
  val get_transition_from :
    nts_automaton ->
    control -> control -> Nts_types.nts_trans_label list list option
  

  val pprint_inputvars : nts_automaton  -> string
  val pprint_outputvars : nts_automaton  -> string 
  val pprint_localvars : nts_automaton  -> string

  (**
     computes a numerical transition system in which all local variables
     list of each automaton has been cleared of non used varibles
  *)
  val nt_system_var_cleaner : nts_system -> nts_system 

  val pprint_to_nts : nts_automaton -> string
  val pprint_nts : nts_system -> string 
    (* Here for debuging purposes. Shall be removed for release
    versions*)
  val pprint_transitions : string -> nts_automaton -> string
  
  (** Compute the set of one step predecessors of all control states*)
  val compute_pred_relation : nts_automaton -> 
    (control, (control , unit) Hashtbl.t ) Hashtbl.t
end

