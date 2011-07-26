(**
	This module contains the definition of an eCFG and implements a generic algorithm to
	fill it with the correct Abstract Interpretation and counter automata label.

	Maxime Gaudin - VERIMAG 2011 
	** THIS MODULE IS A PART OF FLATA-C, DEVELOPED AT VERIMAG (2011)
	For any question mail us to  maxime.gaudin [AT] imag [DOT] fr or florent.garnier [AT] imag [DOT] fr
*)
open Self
open Cil_types

(** First type variable -- i.e. 'a -- is the type of the abstract domain
and 'b is the type used to encode the transition label of the automaton.*)

(** A Flatac exception is composed by an ID, a level and an error message.
* Level 0 : Fatal
* Level 1 : Warning *)
exception Flatac_exception of int * int * string

class virtual ['a, 'b] sem_and_logic_front_end = 
object 
  method virtual get_entry_point_abstraction : unit -> 'a

  (** Returns a label without any consequence on sub transition label *)
  method virtual get_empty_transition_label : unit -> 'b 

  (* Returns true if the second abstraction, knowing the first abstraction, the
  * second one is accepted. *)
  method virtual accepts: 'a -> 'a -> bool

  (** Returns true if the given state is an error state. *)
  method virtual is_error_state: 'a -> bool
  (** Returns a the next couple of abstract interpretation and counter automata label based on the current abstraction, the current counter, and the statement kind. *)
  method virtual next : 'a -> 'b -> stmtkind -> ('a * 'b) list
  
  (** Shall we add some fix point decision procedure at this place ?*)  
  method virtual pretty : 'a -> string
  method virtual pretty_label : 'b -> string
end
