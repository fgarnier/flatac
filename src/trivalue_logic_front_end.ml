
open Cil_types
open Sem_and_logic_front_end

class trivalue_logic_front_end = 
object 
inherit [string, string] sem_and_logic_front_end
	method get_entry_point_abstraction () = "A"
	method get_empty_transition_label () = ""
	method is_error_state state = (state = "X") 
  method entails abs1 abs2 = 
    match (abs1, abs2) with
      | ("B", "C") -> true
      | ("C", "B") -> true
      | _ -> false
  method next current_abstraction _ _ = 
    match current_abstraction with
      | "A" -> [("B", "NONE"); ("C", "NONE")]
      | "B" -> [("C", "NONE")]
      | "C" -> [("B", "NONE")]
      | _ -> [("X", "NONE")]

	method pretty abs = abs 
	method pretty_label lbl = lbl
end
