module P :
  sig
    type t = string
    type anot_type = unit
    val anot_parser : unit -> unit
    val pprint_keyid : 'a -> 'a
    val compare_keyid : String.t -> String.t -> int
    val pprint_anot : 'a -> string
  end
module Nts_int :
  sig
    type anotations = Nts_functor.Make(P).anotations
    type control = Nts_functor.Make(P).control
    type nts_automaton =
      Nts_functor.Make(P).nts_automaton = {
      mutable nts_automata_name : string;
      mutable anot : anotations;
      init_states : (control, unit) Hashtbl.t;
      final_states : (control, unit) Hashtbl.t;
      error_states : (control, unit) Hashtbl.t;
      input_vars : Nts_types.nts_genrel_var list;
      output_vars : Nts_types.nts_genrel_var list;
      local_vars : Nts_types.nts_genrel_var list;
      transitions :
        (control, (control, Nts_types.nts_trans_label list) Hashtbl.t)
        Hashtbl.t;
    }
    type nts_system =
      Nts_functor.Make(P).nts_system = {
      nts_system_name : string;
      nts_global_vars : Nts_types.nts_genrel_var list;
      nts_automata : (string, nts_automaton) Hashtbl.t;
      nts_gvars_init : Nts_types.nts_gen_relation list option;
      nts_system_threads : (string * Big_int.big_int) list option;
    }
    val anot_parser : unit -> anotations
    val control_of_id_param : P.t -> control
    val get_varinfo_by_optname :
      nts_system ->
      string option -> string -> Nts_types.nts_genrel_var option
    val get_varinfo_by_optcautomaton :
      nts_system ->
      nts_automaton option -> string -> Nts_types.nts_genrel_var option
    val get_transition_from :
      nts_automaton ->
      control -> control -> Nts_types.nts_trans_label list list option
    val pprint_inputvars : nts_automaton -> string
    val pprint_outputvars : nts_automaton -> string
    val pprint_localvars : nts_automaton -> string
    val nt_system_var_cleaner : nts_system -> nts_system
    val pprint_to_nts : nts_automaton -> string
    val pprint_nts : nts_system -> string
    val pprint_transitions : string -> nts_automaton -> string
    val compute_pred_relation :
      nts_automaton -> (control, (control, unit) Hashtbl.t) Hashtbl.t
  end
