(** Functor for defining a nts using basic blocks and a control flow graph.
 Needed to perform the first of two stages for the translation from a nts
to the Interproc SPL language. 

 For any comment or question, please write to :

 florent dot garnier at imag dot fr
 or
 florent dot garnier at gmail dot com

 (c) Verimag 2012, until released Under an Open Licence.
*)

module Make :
  functor (Param : Nts_functor.NTS_PARAM) ->
    sig
      module NtsSys :
        sig
          type anotations = Nts_functor.Make(Param).anotations
          type control = Nts_functor.Make(Param).control
          type nts_automaton =
            Nts_functor.Make(Param).nts_automaton
          type nts_system =
            Nts_functor.Make(Param).nts_system
        end

      type control = NtsSys.control
      type anotations = NtsSys.anotations
      
      type nts_basic_block = {
        mutable head_label : string;
        mutable block : (control * Nts_types.nts_trans_label list) list;
        mutable block_succs :
          (nts_basic_block ref * Nts_types.nts_trans_label list) list option;
      }
	  
      type nts_automaton_cfg = {
        mutable nts_cfg_name : string;
        mutable cfg_anot : anotations;
        nts_cfg_init_block : (string, unit) Hashtbl.t;
        nts_cfg_final_block : (string, unit) Hashtbl.t;
        nts_cfg_error_block : (string, unit) Hashtbl.t;
        nts_input_vars : Nts_types.nts_genrel_var list;
        nts_output_vars : Nts_types.nts_genrel_var list;
        nts_local_vars : Nts_types.nts_genrel_var list;
        nts_blocks_transitions : (string, nts_basic_block) Hashtbl.t;
      }
    end
