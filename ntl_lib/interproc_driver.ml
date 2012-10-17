open Nts_types
open Nts_spl_intermediate_language_types

module Make =
  functor ( Param :  Nts_functor.NTS_PARAM ) ->
struct
  
  
  
  (** Types and functions used to generate a control flow graph
      from the numerical transition system description*)
  module NtsSys = Nts_functor.Make(Param)
  
  type control = NtsSys.control
  type anotations = NtsSys.anotations

  type nts_basic_block = {
    mutable head_label : string ;
    mutable block : (control * nts_trans_label list) list; 
    (** Current control state,
	nts_trans_label_list corresponds
	to what changes/is called before
	transiting*)
    
    mutable block_succs : ( nts_basic_block ref * nts_trans_label list ) list option;
  (** transitions between blocks. Nexts blocks and the transisions being
      described.
      None is in the case the last control state is an error state.
      It's also a convenience for the buiding process.   
  *) 
  } 
      
      
  type nts_automaton_cfg = {
    mutable nts_cfg_name : string; 
    mutable cfg_anot : anotations;
    (*states : (control , unit ) Hashtbl.t;*)
    nts_cfg_init_block : (string , unit ) Hashtbl.t;
    nts_cfg_final_block : (string , unit ) Hashtbl.t;
    nts_cfg_error_block : (string , unit ) Hashtbl.t;
    nts_input_vars : nts_genrel_var list; (*Variable ordering is important*)
    nts_output_vars : nts_genrel_var list;
    nts_local_vars : nts_genrel_var list;
    nts_blocks_transitions : ( string , nts_basic_block ) Hashtbl.t
  }

      

end







