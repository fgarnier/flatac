open Self
open Cil_types

class virtual ['a] semAndLogicFrontEnd = 
object 
	method virtual getEntryPointAbstraction : unit -> 'a
	method virtual getEntryPointPrecondition : unit -> 'a
	method virtual isErrorState: 'a -> bool
	method virtual next : 'a -> string -> stmtkind -> ('a * string)
end
