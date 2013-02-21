(**
Those functions are here to help to handle non determinitic support
type for nts arithmetical expressions.


This files is released as a part of the Flata-c distribution,
under the terms of the GNU LGPL v 2.1 Licence.
Written By Florent Garnier, Feb 2013.

Write me at fl*r*ent d*t g*rn**er at gm**l d*t c*m for 
questions or remarks of any kind.

*)


val type_if_type_eq :
  Nts_types.nts_genrel_arithm_exp ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_base_types

val type_of_ndet_supp_cnt_val_shallow :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> Nts_types.nts_base_types

val type_if_eq_det_ndet_aexp :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> Nts_types.nts_base_types

val aterm_binop_ndet_supp_cnt_val :
  Nts_types.nts_gen_arithm_binop ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val

val aterm_uop_ndet_supp_cnt_val :
  Nts_types.nts_gen_arithm_unop ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val

val bterm_genrel_comp :
  Nts_types.cnt_binop ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val bterm_logic_binop :
  Nts_types.nts_gen_bool_binop ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val neg_bterm :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool


(** Use this function to get the aritmetical value if its
is a deterministic one. 
Fails if its not.*)

val arithm_value_of_ndsupport_or_fails : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Nts_types.nts_genrel_arithm_exp
