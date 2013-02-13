(*
This files defines the functions prototypes used to declare/use/transform
the numerical transitions systems and their relations. The intended relations
complies with the NTL lib documentations, see README.txt.


(c) Verimag 2012

Questions and remarks : write to florent_dot_garnier_AT_gmail_dot_com
  


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

exception Found_a_primed_var

(** Pretty prints a variable name, regardless of its type*)
val nts_pprint_genvar : Nts_types.nts_genrel_var -> string

(**Pretty prints a variable list, with each variable name separated by a comma.*)
val pprint_ntsgen_var_list : Nts_types.nts_genrel_var list -> string

(** Pretty prints a variable and its type*)
val nts_pprint_nts_typeinfo_genvar : Nts_types.nts_genrel_var -> string

(** Returns true whenever a variable type has type int.*)
val is_int_var : Nts_types.nts_genrel_var -> bool

(** Returns true whenever a variable type has type real.*)
val is_real_var : Nts_types.nts_genrel_var -> bool

(** Pretty prints both strings separared by a comma when the first one is non-empty,
or pretty prints the non empty argument when there is one, or returs an emptry 
string when both are empty.
*)
val concat_if_first_arg_nonzero : string -> string -> string

val concat_comma_both_arg_non_empty : string -> string -> string


(*Pretty prints all variables, sorted by types with their associated types.*)
val pprint_typeinfo_nts_genvar_list : Nts_types.nts_genrel_var list -> string

(** Shall be removed from this part. Used for pretty printing artithmetical terms
and booleans expressions.
*)

val size_genrel_arithm_deeper_than :
  Nts_types.nts_genrel_arithm_exp -> int -> bool

val size_genrel_deeper_than : Nts_types.nts_gen_relation -> int -> bool


(** Pprints a general expression --Constrainsts between prime and unprimed
variables.*)
val nts_pprint_genrel_arithm_exp : Nts_types.nts_genrel_arithm_exp -> string

val pprint_gen_rel_arithm_list :
  Nts_types.nts_genrel_arithm_exp list -> string

val nts_pprint_bool_binop :
  string -> Nts_types.nts_gen_bool_binop -> string -> string

val nts_pprint_aritm_binop : Nts_types.nts_gen_arithm_binop -> string

val nts_pprint_genrel : Nts_types.nts_gen_relation -> string

val boolean_relation : Nts_types.nts_genrel_arithm_exp -> bool

(**Removes some --the most ugly of them -- tautologies from booleans expressions*)
val simplify_genrel_bottom_top :
  Nts_types.nts_gen_relation -> Nts_types.nts_gen_relation

val simplify_gen_rel :
  Nts_types.nts_gen_relation -> Nts_types.nts_gen_relation

(** Does the input relation only depends on deterministic variables ?*)
val is_gen_bool_det : Nts_types.nts_gen_relation -> bool
(** Does the arithmecic expression only depends on deterministic variables ?*)
val is_gen_arithm_exp_a_function : Nts_types.nts_genrel_arithm_exp -> bool

(** Determines whether a boolean expression is false --e.g. contains a false*)
val static_check_if_gen_relation_false : Nts_types.nts_gen_relation -> bool

val static_check_if_gen_translist_unsat :
  Nts_types.nts_trans_label list -> bool

val nts_pprint_gen_trans_label : Nts_types.nts_trans_label -> string

val is_label_true : Nts_types.nts_trans_label -> bool

(** Pretty prints a transition label*)
val nts_pprint_gen_trans_label_list :
  Nts_types.nts_trans_label list -> string
