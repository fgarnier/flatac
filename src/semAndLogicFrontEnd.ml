(*
	Maxime Gaudin - VERIMAG 2011 

	** THIS MODULE IS A PART OF FLATA-C, DEVELOPED AT VERIMAG (2011)

	This virtual class defines an interface between the eCFG and the underlying user logic 
	'a type is the abstract interpretation type.

	For any question mail us to :
	- maxime.gaudin@imag.fr
	- florent.garnier@imag.fr
*)
open Self
open Cil_types

class virtual ['a] semAndLogicFrontEnd = 
object 
	(** Returns the abstract interpretation of the entry point *)
	method virtual getEntryPointAbstraction : unit -> 'a

	method virtual getEntryPointPrecondition : unit -> 'a

	(** Returns true if the given state is an error state *)
	method virtual isErrorState: 'a -> bool

	(** Returns a the next couple of abstract interpretation and counter automata label based 
	 on the current abstraction, the current counter, and the statement kind *)
	method virtual next : 'a -> string -> stmtkind -> ('a * string)

	method virtual pretty : 'a -> string
end
