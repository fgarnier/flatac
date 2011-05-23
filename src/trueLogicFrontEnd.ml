
open Cil_types
open SemAndLogicFrontEnd

class trueLogicFrontEnd = 
object 
inherit [bool] semAndLogicFrontEnd
	method getEntryPointAbstraction () = true
	method getEntryPointPrecondition () = true 
	method isErrorState state = (state = false) 
	method next currentAbstraction guard stmtData = ( not currentAbstraction, "" )
	method pretty abs = if abs then "TRUE" else "FALSE"
end
