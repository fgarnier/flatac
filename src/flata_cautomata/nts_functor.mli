open Nts_types
open Hashtbl
open Option

module type NTS_PARAM =
  sig
    type t         (*Type for key id of control states: e.g : int, string*)
    type anot_type (*Type for anotations*)
    val make_anot : unit -> anot_type (*Needed In case of anotation
				      are using mutable container that
				      need to be created, hashtbl for
				      instance*)
    val pprint: anot_types -> string (*Types for pprinting anotations*)
  end 




module type Nts_gen =
  functor( Param : NTS_PARAM ) ->
sig 
  type anotations = Nts_Anot of Param.anot_type
  type control = Nts_state of Param.t (* Control state *)
      
  type nts_system 
  type nts_automaton
  val create_nts_cautomata : unit -> nts_automata (* Creates a new structure
					nts_automata*)
  val add_cautomata_to_nts : nts_automata -> nts_system -> unit
  val rename_nts_automaton :  nts_automata -> string -> unit
  val pprint : nts_automata -> string
  val add_nts_int_vars_to_nts_system : nts_system -> string list -> unit 
  val add_nts_real_vars_to_nts_system : nts_system -> string list -> unit 
  val add_inputvar_left : nts_automata -> nts_var -> nts_automata
  val add_outputvar_left : nts_automata -> nts_var -> nts_automata
  val add_transition : nts_automata -> control -> control -> cnt_trans_label -> unit
  val get_transition :  nts_automata -> control -> control -> cnt_trans_label list option (*None is returned if no transition exists*)
  val add_initial_state 
end

