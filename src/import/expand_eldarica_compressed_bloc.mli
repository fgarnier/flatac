module Make :
  functor (Param : Nts_functor.NTS_PARAM) ->
    sig
      module NFParam :
        sig
          type anotations = Nts_functor.Make(Param).anotations
          type control =
            Nts_functor.Make(Param).control =
              Nts_State of Param.t
          type transitions_container =
              Nts_functor.Make(Param).transitions_container
          type states_container = Nts_functor.Make(Param).states_container
          type inv_relation_container =
              Nts_functor.Make(Param).inv_relation_container
          val fold_states_containers :
            states_container -> ('a -> control -> 'a) -> 'a -> 'a
          val fold_transitions_container :
            transitions_container ->
            ('a -> control -> Nts_types.nts_trans_label list -> control -> 'a) ->
            'a -> 'a
          val add_transition_to_container :
            transitions_container ->
            control -> Nts_types.nts_trans_label list -> control -> unit
          val iter_transitions_container :
            transitions_container ->
            (control -> Nts_types.nts_trans_label list -> control -> unit) ->
            unit
          val iter_state_container :
            states_container -> (control -> unit) -> unit
          val is_state_in_inv_relation :
            inv_relation_container -> control -> bool
          val is_state_in_transition_container :
            control -> transitions_container -> bool
          type nts_automaton =
            Nts_functor.Make(Param).nts_automaton = {
            mutable nts_automata_name : string;
            mutable anot : anotations;
            init_states : states_container;
            final_states : states_container;
            error_states : states_container;
            input_vars : Nts_types.nts_genrel_var list;
            output_vars : Nts_types.nts_genrel_var list;
            local_vars : Nts_types.nts_genrel_var list;
            transitions : transitions_container;
          }
          type nts_system =
            Nts_functor.Make(Param).nts_system = {
            nts_system_name : string;
            nts_global_vars : Nts_types.nts_genrel_var list;
            nts_automata : (string, nts_automaton) Hashtbl.t;
            nts_gvars_init : Nts_types.nts_gen_relation list option;
            nts_system_threads : (string * Big_int.big_int) list option;
          }
          type num_subrel_in_cautomaton =
            Nts_functor.Make(Param).num_subrel_in_cautomaton = {
            subrel_root : control;
            sub_vertices : states_container;
            sub_transitions : transitions_container;
          }
          val is_state_in_cautomaton : control -> nts_automaton -> bool
          val pprint_control : control -> string
          val anot_parser : unit -> anotations
          val states_container_of_states_list :
            control list -> states_container
          val transitions_container_of_trans_list :
            (control * control * Nts_types.nts_trans_label list) list ->
            transitions_container
          val control_of_id_param : Param.t -> control
          val out_degree_of_control_state : control -> nts_automaton -> int
          val get_varinfo_by_optname :
            nts_system ->
            string option -> string -> Nts_types.nts_genrel_var option
          val get_varinfo_by_optcautomaton :
            nts_system ->
            nts_automaton option -> string -> Nts_types.nts_genrel_var option
          val is_error_state : nts_automaton -> control -> bool
          val is_initial_state : nts_automaton -> control -> bool
          val is_final_state : nts_automaton -> control -> bool
          val get_transition_from :
            nts_automaton ->
            control -> control -> Nts_types.nts_trans_label list list option
          val get_successor_of : nts_automaton -> control -> states_container
          val get_one_state : states_container -> control option
          val is_successor_of : nts_automaton -> control -> control -> bool
          val get_one_transition :
            nts_automaton ->
            control -> control * Nts_types.nts_trans_label list
          val pprint_inputvars : nts_automaton -> string
          val pprint_outputvars : nts_automaton -> string
          val pprint_localvars : nts_automaton -> string
          val nt_system_var_cleaner : nts_system -> nts_system
          val nt_system_uncalled_subsystem_cleaner : nts_system -> nts_system
          val pprint_to_nts : nts_automaton -> string
          val pprint_nts : nts_system -> string
          val get_cautomaton_by_name : nts_system -> string -> nts_automaton
          val pprint_transitions : string -> nts_automaton -> string
          val compute_pred_relation : nts_automaton -> inv_relation_container
          val subgraph_between :
            nts_automaton -> control -> control -> num_subrel_in_cautomaton
          val pprint_subgraph_transitions :
            num_subrel_in_cautomaton -> string
          val cautomaton_of_subrelation_cautomaton :
            string ->
            nts_automaton -> num_subrel_in_cautomaton -> nts_automaton
          val cautomaton_of_transitions_container :
            string -> nts_automaton -> transitions_container -> nts_automaton
        end
      (*
	type anotations = NFParam.anotations
      type control = NFParam.control
      type nts_automaton = NFParam.nts_automaton
      type nts_system = NFParam.nts_system
      val control_out_of_string : string -> Param.t
      type subsystem_call_count = (nts_automaton, int ref) Hashtbl.t
      val nts_out_of_subrelation : NFParam.num_subrel_in_cautomaton -> string
      val get_tran :
        'a ->
        NFParam.nts_automaton -> NFParam.control -> NFParam.control -> string
      val get_contextual_transitions_of_subgraph :
        'a ->
        NFParam.num_subrel_in_cautomaton ->
        ('a *
         (NFParam.control * Nts_types.nts_trans_label list * NFParam.control))
        list
      val debug_pprint_syscontrol : Trace_types.sys_control -> string
      val control_of_syscontrol : Trace_types.sys_control -> NFParam.control
      val ca_name_of_syscontrol : Trace_types.sys_control -> string
      val ca_of_syscontrol :
        NFParam.nts_system ->
        Trace_types.sys_control -> NFParam.nts_automaton
      val is_transition_a_call : Nts_types.nts_trans_label list -> bool
      val is_a_return :
        NFParam.nts_automaton -> 'a * 'b * NFParam.control -> bool
      val get_contextual_transition_list_from_pair_folder :
        NFParam.nts_system ->
        NFParam.nts_system ->
        (NFParam.nts_automaton *
         (NFParam.control * Nts_types.nts_trans_label list * NFParam.control))
        list * Trace_types.sys_control option ->
        Trace_types.sys_control ->
        (NFParam.nts_automaton *
         (NFParam.control * Nts_types.nts_trans_label list * NFParam.control))
        list * Trace_types.sys_control option
      val contextual_transition_list_of_trace :
        NFParam.nts_system ->
        NFParam.nts_system ->
        Trace_types.sys_control list ->
        (NFParam.nts_automaton *
         (NFParam.control * Nts_types.nts_trans_label list * NFParam.control))
        list
      val initial_context_of_ctl_list : ('a * 'b) list -> 'a * int
      val nts_subsystem_of_ca_cid : string -> int -> string
      val contextual_call_of_subsystem :
        Nts_types.nts_trans_label list ->
        int ref -> Nts_types.nts_trans_label list
      val get_called_subsystem_name :
        Nts_types.nts_trans_label list -> string
      val get_ca_by_name :
        NFParam.nts_system ->
        NFParam.nts_system -> string -> NFParam.nts_automaton
      val definition_of_called_ca :
        NFParam.nts_system ->
        NFParam.nts_system ->
        Nts_types.nts_trans_label list -> NFParam.nts_automaton
      val new_context_table_entry :
        ('a, 'b * 'c list) Hashtbl.t -> 'a -> 'b -> unit
      val is_context_switch_ahead :
        NFParam.nts_automaton -> (NFParam.nts_automaton * 'a) list -> bool
      val empty_tail : 'a list -> bool
      val add_transtion_in_contextual_trans_sys :
        ('a, 'b * 'c list) Hashtbl.t -> 'b -> 'a -> 'c -> unit
      val context_table_pprinter :
        (int,
         NFParam.nts_automaton *
         (NFParam.control * Nts_types.nts_trans_label list * NFParam.control)
         list)
        Hashtbl.t -> string
      val nts_of_transitions_rules_container :
        (int,
         NFParam.nts_automaton *
         (NFParam.control * Nts_types.nts_trans_label list * NFParam.control)
         list)
        Hashtbl.t -> (string, NFParam.nts_automaton) Hashtbl.t
      val build_nts_table_from_contextual_trace :
        NFParam.nts_system ->
        NFParam.nts_system ->
        Trace_types.sys_control list ->
        (string, NFParam.nts_automaton) Hashtbl.t

	*)


	  
      (** Computes the numerical transition system that corresponds 
	  to an execution trace on the subsystems define in a standard
	  library --first parameter, a numerical transition system --
	  second parameter, an a trace.
      *)  
      val nts_out_trace :
        NFParam.nts_system ->
        NFParam.nts_system ->
        Trace_types.sys_control list -> NFParam.nts_system
    end
