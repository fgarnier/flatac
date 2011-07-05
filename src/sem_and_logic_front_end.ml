(**
	This module contains the definition of an eCFG and implements a generic algorithm to
	fill it with the correct Abstract Interpretation and counter automata label.

	Maxime Gaudin - VERIMAG 2011 
	** THIS MODULE IS A PART OF FLATA-C, DEVELOPED AT VERIMAG (2011)
	For any question mail us to  maxime.gaudin [AT] imag [DOT] fr or florent.garnier [AT] imag [DOT] fr
*)
open Self
open Cil_types

(* Changer le type string en CounterLabel ou un truc générique dans le genre *)
type counter_expression = string

class virtual ['a] sem_and_logic_front_end = 
object 
  method virtual get_entry_point_abstraction : unit -> 'a
  method virtual get_entry_point_precondition : unit -> counter_expression 

  (** Returns true if the given state is an error state. *)
  method virtual is_error_state: 'a -> bool
  (** Returns a the next couple of abstract interpretation and counter automata label based on the current abstraction, the current counter, and the statement kind. *)
  method virtual next : 'a -> counter_expression -> stmtkind -> ('a * counter_expression)
  
  (** Shall we add some fix point decision procedure at this place ?*)  
  method virtual pretty : 'a -> string
end
