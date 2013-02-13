(** 

 This functor aims at definiting an interface.
between nts control flow graphs and interproc SPL languge.

 For any comment or question, please write to :

 florent dot garnier at imag dot fr
 or
 florent dot garnier at gmail dot com

 (c) Verimag 2012, released under LGPL v.2.1.
	 

WARNING : This current developpement is on the verge to 
be modified soon --Febuary 2013.
*)


open Nts_types


module Make_NtsCfg :
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
        mutable block : (Nts_types.nts_trans_label list) list;
        mutable block_succs :
          (nts_basic_block ref * Nts_types.nts_trans_label list) list option;
      }
	  
      type nts_function_cfg = {
        mutable nts_cfg_name : string;
        mutable cfg_anot : anotations;
        nts_cfg_blocks : (string, unit) Hashtbl.t;
	
        nts_cfg_final_block : (string, unit) Hashtbl.t;
        nts_cfg_error_block : (string, unit) Hashtbl.t;
        nts_input_vars : Nts_types.nts_genrel_var list;
        nts_output_vars : Nts_types.nts_genrel_var list;
        nts_local_vars : Nts_types.nts_genrel_var list;
        nts_blocks_transitions : (string, nts_basic_block) Hashtbl.t;
      }

      type nts_functional_program = {
	nts_fun_glob_vars : nts_genrel_var list;
	nts_fun_function_definitions : nts_function_cfg list option;
	nts_fun_gvars_init : nts_gen_relation list option; 
	nts_fun_function_main : nts_function_cfg;

	nts_fun_system_threads : 
	  ( string * Big_int.big_int ) list  option;   
	
      }
	  (*
      val nts_cfg_of_automaton : NtsSys.nts_automaton -> nts_function_cfg
      val nts_cfg_of_nts_sys : NtsSys.nts_system -> nts_function_cfg
	  *)
    end



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
