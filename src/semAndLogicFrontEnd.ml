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
type counterExpression = string

class virtual ['a] semAndLogicFrontEnd = 
object 
	method virtual getEntryPointAbstraction : unit -> 'a
	method virtual getEntryPointPrecondition : unit -> counterExpression 

	(** Returns true if the given state is an error state. *)
	method virtual isErrorState: 'a -> bool

	(** Returns a the next couple of abstract interpretation and counter automata label based 
	 on the current abstraction, the current counter, and the statement kind. 
	 The process must be markovian though. *)
	method virtual next : 'a -> counterExpression -> stmtkind -> ('a * counterExpression)

	method virtual pretty : 'a -> string
end
