module Make :
  functor (Param : Nts_functor.NTS_PARAM) ->
    sig
      module NFParam :
        sig
          type anotations = Nts_functor.Make(Param).anotations
          type control = Nts_functor.Make(Param).control
          type nts_automaton =
            Nts_functor.Make(Param).nts_automaton = {
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
            Nts_functor.Make(Param).nts_system = {
            nts_system_name : string;
            nts_global_vars : Nts_types.nts_genrel_var list;
            nts_automata : (string, nts_automaton) Hashtbl.t;
            nts_gvars_init : Nts_types.nts_gen_relation list option;
            nts_system_threads : (string * Big_int.big_int) list option;
          }
          val pprint_control : control -> string
          val anot_parser : unit -> anotations
          val control_of_id_param : Param.t -> control
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
      type anotations = NFParam.anotations
      type control = NFParam.control
      type nts_automaton = NFParam.nts_automaton
      type nts_system = NFParam.nts_system
      val pprint_control : NFParam.control -> string
      val dot_of_init_nodes : NFParam.nts_automaton -> string
      val dot_of_final_nodes : NFParam.nts_automaton -> string
      val dot_of_error_nodes : NFParam.nts_automaton -> string
      val dot_of_transitions : NFParam.nts_automaton -> string -> string
      val dot_of_cautomaton :
        ?standalone_graph:bool -> NFParam.nts_automaton -> string
      val dot_of_nts : NFParam.nts_system -> string
    end
