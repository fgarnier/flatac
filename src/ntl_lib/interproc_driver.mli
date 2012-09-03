open Syntax (* Module that defines the syntax of the spl language*)
open nts_gentypes

let cntbool_of_apron_expr 'a Bddapron.Syntax.expr -> Nts_types.cnt_bool
let bddapronexpr_of_cntbool Nts_types.cnt_bool -> 'a Bddapron.Syntax.expr




type ntl_spl_il_label = string

type ntl_spl_il_instruction = NS_Skip 
			      | NS_Halt
			      | NS_Fail
			      | NS_Assume
			      | NS_If of nts_gen_relation * nts_spl_il_label * nts_spl_il_label option
			      | NS_Goto of nts_spl_il_label
			      | NS_Call of nts_var list option * string * cnt_arithm_exp list 
			      | NS_local of bool * nts_var list * NS_block

and ntl_spl_il_instr = {
  ns_instruction :  ntl_spl_il_instruction option ;
  ns_ipoint : point ; (* Type defined in syntax.mli of Interproc*)
} 


and ntl_spl_block = {
  ns_bpoint : point ;
  ns_instrs : ntl_spl_il_instr list;
}


type nts_spl_il_procedure = {
  ns_pname : string ;
  ns_pinput : cnt_var list;
  ns_poutput : cnt_var list;
  ns_pcode : ntl_spl_block ;
}


type nts_spl_il_program = {
  (*ns_typenumdef : unit*)
  ns_global : cnt_var list ;
  ns_initial : nts_genrel_arithm_exp ;
  ns_final : nts_genrel_arithm_exp ;
  ns_procedures : (string , nts_spl_il_procedure ) list ;
  ns_threads : string list; (* For concurr interproc*)
}
