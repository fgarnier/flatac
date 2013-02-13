(*
This files contains the implementation of the Numerical Transition Library 
--see http://richmodels.epfl.ch/ntscomp-- main objects, namely :

_ Numerical transitions subsystems, (i.e. parametric counter automaton
  with return values upon return)
_ Hyerarchical transistions subsystems .


Plus a parser, a pretty printer as well as cleanup functions.
A type checker will be added.



Written by Florent Garnier, at Verimag Labs  2012 
Contact florent dot garnier at gmail dot com for  further informations.

This files is released under the terms of the LGPL v2.1 Licence.

 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor,
 Boston, MA  02110-1301  USA


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
      Nts_types.nts_genrel_arithm_exp list
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
