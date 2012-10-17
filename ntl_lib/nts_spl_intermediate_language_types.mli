(**

 Types definitions for the ntl_spl intermediate language.  
 This file has been generated using ocamlc -i. 

 Further comments might be presents in the corresponding
 ml implementation file.

This language eases the translation of a transition systems to
the SPL language, where instructions are regrouped into basic
blocks and where the programs is represented as a listing.

This intermediate language aims at focusing on the translation
from the tansitions sytems to the control flow structure which
is proper to SPL.
A second phase consists in translating the variables, arithmetical
expressions and pressburger expressions from the NTL language to
BDD Apron expressions.


(c) Verimag 2012 upon completion. Will be released under a more
opened licence, LGPL might be a good one.

Contact florent dot garnier at gmail dot com for  further informations.

	
*)

type ntl_spl_il_label = string
type point = ntl_spl_il_label

type ntl_spl_il_instruction =
    NS_Skip
  | NS_Halt
  | NS_Fail
  | NS_Assume
  | NS_If of Nts_types.nts_gen_relation * ntl_spl_il_label *
      ntl_spl_il_label option
  | NS_Goto of ntl_spl_il_label
  | NS_Call of Nts_types.nts_var list option * string *
      Nts_types.cnt_arithm_exp list
  | NS_local of bool * Nts_types.nts_var list * ntl_spl_block

and ntl_spl_il_instr = {
  ns_insturction : ntl_spl_il_instruction option;
  ns_ipoint : point;
}

and ntl_spl_block = { ns_bpoint : point; ns_instrs : ntl_spl_il_instr list; }

type nts_spl_il_procedure = {
  ns_pname : string;
  ns_pinput : Nts_types.nts_var list;
  ns_poutput : Nts_types.nts_var list;
  ns_pcode : ntl_spl_block;
}

type nts_spl_il_program = {
  ns_global : Nts_types.nts_var list;
  ns_initial : Nts_types.nts_genrel_arithm_exp;
  ns_final : Nts_types.nts_genrel_arithm_exp;
  ns_procedures : (string * nts_spl_il_procedure) list;
  ns_threads : string list;
}
