(*This files contains the implementation of the Numerical Transition Library 
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

(**
This module defines a set of function that allows to clean numerical 
transitions systems from non called subsystems and subsystems from
unused variables.
*)

(* Type of a variable diary *)
type vars_entry (*= UVars_diary of (string, unit) Hashtbl.t*)

type vars_entry_by_name (*=
    UNamedVarsDiary of (string, Nts_types.nts_var) Hashtbl.t*)

type called_subsystems_diary 

val create_empty_var_diary : unit -> vars_entry
val create_fun_name_in_call_table : unit -> called_subsystems_diary 

val get_diary_table : vars_entry -> (string, unit ) Hashtbl.t

val add_vars_of_cnt_trans_label_to_diary :
  vars_entry -> Nts_types.nts_trans_label -> unit


val register_called_subsystems : called_subsystems_diary -> Nts_types.nts_trans_label -> unit

val add_vars_of_trans_label_list_to_diary :
  vars_entry ->  Nts_types.nts_trans_label list -> unit
val add_fun_name_in_call_table :  called_subsystems_diary->string -> unit
val is_name_in_call_table  : called_subsystems_diary -> string -> bool 
(** Answer yes when the variable is listed in the entry, false
in the opposite case.*)

val contains_var : vars_entry -> Nts_types.nts_var -> bool
val contains_nts_genrel_var : vars_entry -> Nts_types.nts_genrel_var -> bool

val pprint_diary : vars_entry -> unit

