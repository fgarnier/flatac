open Nts_types
open Hashtbl
open Option

module type ANOTATIONS_TYPES =
  sig
    type t
    val pprint: t -> string
  end


module type 


module type Nts_gen =
  functor( Anot : ANOTATIONS_TYPES ) ->
sig 
  type anotations = Nts_Anot of Anot.t
  type control = Nts_state of int (* Control state *)
      
  type nts_automata
  val create_nts : unit -> nts_automata (* Creates a new structure
					nts_automata*)
  val pprint : nts_automata -> string
  val add_inputvar_left : nts_automata -> nts_var -> nts_automata
  val add_outputvar_left : nts_automata -> nts_var -> nts_automata
  val add_transition : nts_automata -> control -> control -> cnt_trans_label -> unit
  val get_transition :  nts_automata -> control -> control -> cnt_trans_label list option (*None is returned if no transition exists*)
  val add_initial_state 
end

