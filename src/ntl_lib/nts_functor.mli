open Nts_types
open Hashtbl


module type NTS_PARAM =
  sig
    type t         
      (*Type for key id of control states: e.g : int, string*)
    type anot_type (*Type for anotations*)
    val make_anot : 'a -> anot_type 
      (*Needed In case of anotation
	are using mutable container that
	need to be created, hashtbl for
	instance*)
      
    val pprint_keyid : t -> string (*Types for pprinting anotations*)
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
        states : (control , unit ) Hashtbl.t;
        init_states : (control , unit ) Hashtbl.t;
        final_states : (control , unit ) Hashtbl.t;
        error_states : (control , unit ) Hashtbl.t;
        mutable input_vars : nts_var list; (*Variable ordering is important*)
        mutable output_vars : nts_var list;
        mutable local_vars : nts_var list;
        transitions : (control, (control , cnt_trans_label list ) Hashtbl.t) Hashtbl.t ;

	}
  type nts_system (** Hierarchical numerical transition systems *)
  
  val control_of_id_param : Param.t -> control
  (*val create_nts_cautomaton : unit -> nts_automaton (* Creates a new structure
					nts_automata*)*)
  val add_nts_int_vars_to_nts_system : nts_system -> string list -> unit 
  val add_nts_real_vars_to_nts_system : nts_system -> string list -> unit 
  (* string option check in a subsystem; string var name *)
  val get_varinfo : nts_system -> string option -> string -> nts_var option 

  val add_cautomata_to_nts : nts_automaton -> nts_system -> unit
  
  (*One must be sure that any binding name->cautomaton in an nts is
  updated as well*)
  val rename_nts_automaton :  nts_automaton -> nts_system ->string -> unit
  
  (*val pprint : nts_automaton -> string*)
        
  val create_nts_system : string -> nts_system
  val add_globvar_to_nts_system : nts_var -> nts_system -> unit
  val create_nts_automaton : string -> nts_automaton
  val add_inputvar_left : nts_automaton -> Nts_types.nts_var -> unit
  val add_outputvar_left :
    nts_automaton -> Nts_types.nts_var -> unit
  val add_init_state : nts_automaton -> control -> unit
  val add_error_state : nts_automaton -> control -> unit
  val add_final_state : nts_automaton -> control -> unit
  val add_transition :
    nts_automaton ->
    control -> control -> Nts_types.cnt_trans_label list -> unit
  val get_transition_from :
    nts_automaton ->
    control -> control -> Nts_types.cnt_trans_label list list option
  
  val pprint_to_nts : nts_automaton -> string
  
end

