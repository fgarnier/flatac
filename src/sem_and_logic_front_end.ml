(**
	This module contains the definition of an eCFG and implements a generic algorithm to
	fill it with the correct Abstract Interpretation and counter automata label.

	Maxime Gaudin - VERIMAG 2011 
	** THIS MODULE IS A PART OF FLATA-C, DEVELOPED AT VERIMAG (2011)
	For any question mail us to  maxime.gaudin [AT] imag [DOT] fr or florent.garnier [AT] imag [DOT] fr
*)
open Self
open Cil_types
open Global_mem

(** First type variable -- i.e. 'a -- is the type of the abstract domain
and 'b is the type used to encode the transition label of the automaton.*)

(** A Flatac exception is composed by an ID, a level and an error message.
* Level 0 : Fatal
* Level 1 : Warning *)
exception Flatac_exception of int * int * string

class virtual ['a, 'b] sem_and_logic_front_end = 
object 
 
  method virtual get_entry_point_abstraction : unit -> 'a
  method virtual get_entry_point_from_fundec : Cil_types.file -> Cil_types.fundec  -> 'a

  (** Returns a label without any consequence on sub transition label *)
  method virtual get_empty_transition_label : unit -> 'b 

  method virtual copy_absdom_label : 'a -> 'a
  method virtual copy_transit_label : 'b -> 'b
  method virtual positive_guard_from_error_guard : 'b -> 'b

  (* Returns true if, knowing the first abstraction, the second one is
  * accepted *)
  method virtual accepts: 'a -> 'a -> bool

  method virtual entails: 'a -> 'a -> bool

  (** Returns true if the given  abstract value of the current 
 state is an error state. *)
  method virtual is_error_state: 'a -> bool
 
  method virtual is_control_state_erroneous : Cil_types.stmtkind -> bool

  method virtual  make_absdom_errorval : 'a -> unit
    

  (** Returns a list of abstract interpretation and counter automata label
  * couple based on the current abstraction, the current counter, and the 
  * statement kind. *)
  method virtual next : 'a -> 'b -> stmtkind -> ('a * 'b) list  
  method virtual next_on_if_statement : 'a ->  Cil_types.exp -> (('a * 'b) *('a * 'b) * ('a * 'b))
  (** Abstraction pretty printer *)
  method virtual pretty : 'a -> string

  (** Label pretty printer *)
  method virtual pretty_label : 'b -> string

  method virtual  get_initialize_label : unit -> 'b

  method virtual havocise_label : 'b -> 'b

  method virtual need_split_transition : 'b -> bool
  method virtual static_unsat_label : 'b -> bool
   
  method virtual split_guard_call_transition : 'b -> ('b * 'b)
    


  method virtual number_of_valid_vars : 'a -> int

  method virtual equals_labels : 'b -> 'b -> bool 
  method virtual pprint_list_of_valid_var : 'a -> string 
  
  (*method virtual*)  
  
  method virtual  pprint_list_of_malloc_vars : unit -> string
  method virtual pprint_list_of_valid_locals_var : 'a -> Cil_types.fundec -> string (* The ones that are not formals variables.*)
   
end
