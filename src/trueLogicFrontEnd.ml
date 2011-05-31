
open Cil_types
open SemAndLogicFrontEnd

class trueLogicFrontEnd = 
object 
inherit [bool] semAndLogicFrontEnd
	method getEntryPointAbstraction () = true
	method getEntryPointPrecondition () = ""
	method isErrorState state = (state = false) 
	method next currentAbstraction _ _ = (currentAbstraction, "" )
	method pretty abs = if abs then "TRUE" else "FALSE"
end
