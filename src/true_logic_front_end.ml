
open Cil_types
open Sem_and_logic_front_end

class true_logic_front_end = 
object 
inherit [bool, string] sem_and_logic_front_end
	method get_entry_point_abstraction () = true
	method get_empty_transition_label () = ""
	method is_error_state state = (state = false) 
        method next current_abstraction _ _ = [(current_abstraction, "NONE" ) ; (not current_abstraction, "NONE" )]
	method pretty abs = if abs then "TRUE" else "FALSE"
	method pretty_label lbl = lbl
end
