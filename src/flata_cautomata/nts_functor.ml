open Nts_types
open Hashtbl
open Option



module Nts_gen =
  functor( Anot : ANOTATIONS_TYPES ) =
struct 
  type anotations = Nts_Anot of Anot.t
  type control = Nts_state of int (* Control state *)
      
  type nts_automata =
      {
	states : (control , unit ) t;
	init_states : (control , unit )t;
	final_states : (control , unit ) t;
	error_states : (control , unit ) t;
	intput_vars : nts_var list; (*Variable ordering is important*)
	outpur_vars : nts_va list;
	
	transitions : (control, (control , cnt_trans_label list ) Hashtbl.t) Hashtbl.t ;
      }
  val create_nts : unit -> nts_automata (* Creates a new structure
					nts_automata*)
  val pprint : nts_automata -> string
  val add_inputvar_left : nts_automata -> nts_var -> nts_automata
  val add_outputvar_left : nts_automata -> nts_var -> nts_automata
  val add_transition : nts_automata -> control -> control -> cnt_trans_label -> unit
  val get_transition :  nts_automata -> control -> control -> cnt_trans_label list option (*None is returned if no transition exists*)
  val add_initial_state 
end
