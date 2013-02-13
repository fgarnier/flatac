(*
Interface file for prototyping an intermediate languages between the
Numerical Transition System library language and Interproc SPL
language.

This language eases the translation of a transition systems to
the SPL language, where instructions are regrouped into basic
blocks and where the programs is represented as a listing.

This intermediate language aims at focusing on the translation
from the tansitions sytems to the control flow structure which
is proper to SPL.
A second phase consists in translating the variables, arithmetical
expressions and pressburger expressions from the NTL languague to
BDD Apron expressions.


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


(*
open Syntax 
*)
(* Syntax is the module that defines the syntax of the spl language*)


open Nts_types

(*
let cntbool_of_apron_expr : 'a Bddapron.Syntax.expr -> Nts_types.cnt_bool
let bddapronexpr_of_cntbool :  Nts_types.cnt_bool -> 'a Bddapron.Syntax.expr
*)



type ntl_spl_il_label = string

type point = ntl_spl_il_label (* Type point is defined in
			      syntax.mli. Currently defined as follows*)

type ntl_spl_il_instruction = NS_Skip 
			      | NS_Halt
			      | NS_Fail
			      | NS_Assume
			      | NS_If of nts_gen_relation * ntl_spl_il_label * ntl_spl_il_label option
			      | NS_Goto of ntl_spl_il_label
			      | NS_Call of nts_var list option * string * nts_genrel_arithm_exp list 
			      | NS_local of bool * nts_var list * ntl_spl_block

and  ntl_spl_il_instr = {
  ns_insturction :  ntl_spl_il_instruction option ;
  ns_ipoint : point ; (* Type defined in syntax.mli of Interproc*)
} 


and ntl_spl_block = {
  ns_bpoint : point ;
  ns_instrs : ntl_spl_il_instr list;
}


type nts_spl_il_procedure = {
  ns_pname : string ;
  ns_pinput : nts_var list;
  ns_poutput : nts_var list;
  ns_pcode : ntl_spl_block ;
}


type nts_spl_il_program = {
  (*ns_typenumdef : unit*)
  ns_global : nts_var list ;
  ns_initial : nts_genrel_arithm_exp ;
  ns_final : nts_genrel_arithm_exp ;
  ns_procedures : (string * nts_spl_il_procedure ) list ;
  ns_threads : string list; (* For concurr interproc*)
}
