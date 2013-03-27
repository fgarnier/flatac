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


val add_nd_arithm_expr : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val

val sub_nd_arithm_expr : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val

val mul_nd_arithm_expr : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val

val div_nd_arithm_expr : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val

val mod_nd_arithm_expr : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
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


val guard_nd_lt_aexpr :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val guard_nd_gt_aexpr :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool


val guard_nd_gt_zero : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val guard_nd_geq_zero : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val guard_nd_lt_zero : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val guard_nd_leq_zero : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool


val guard_nd_leq_aexpr :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val guard_nd_geq_aexpr :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val guard_nd_eq_aexpr :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val guard_nd_neq_aexpr :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool


(**
   Guard whose constraint is non-deterministic.
*)

val make_nd_guard_of_relation :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool
  -> Flatac_ndet_nts_support_types.ndet_supp_nts_trans_label

(** Express a non deterministic guard whose boolean expression
    is fully deterministic.
*)

val make_nd_cnt_bool_of_nts_genrel :
  Nts_types.nts_gen_relation -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val make_nd_guard_of_det_relation : 
  Nts_types.nts_gen_relation ->  
  Flatac_ndet_nts_support_types.ndet_supp_nts_trans_label

val make_nd_guard_if :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool
  -> Flatac_ndet_nts_support_types.ndet_supp_nts_trans_label

val make_nd_guard_else :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool
  -> Flatac_ndet_nts_support_types.ndet_supp_nts_trans_label
 
val neg_bterm :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool


(** Requires that the leftmost argument is a variable.
   Express an affectation fron the rhs to the left hand
    side variable. The left hand side variable is primed
    in the process.
*)

val make_affect_to_det_var_from_ndet_supp_cnt_val :
  Nts_types.nts_genrel_var ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val make_affect_to_var_from_nd_exp:
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

(** Use this function to get the aritmetical value if its
is a deterministic one. 
Fails if its not.*)

val arithm_value_of_ndsupport_or_fails : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_val ->
  Nts_types.nts_genrel_arithm_exp

(** Does a value have a deterministic evaluation.*)
val is_val_det: Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> bool

(* TODO : Implement that stuff.*)
val pprint_val : Flatac_ndet_nts_support_types.ndet_supp_cnt_val -> string 


val and_of_nd_genrel : 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val or_of_nd_genrel :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool ->
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool

val neg_of_nd_genrel :
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool -> 
  Flatac_ndet_nts_support_types.ndet_supp_cnt_bool






(*
let valid_name_of_var (vname : string ) =
  "validity__"^vname^"_"

let offset_name_of_var (vname : string ) =
  "offset__"^vname^"_"
*)

(** creates an offset name and a validity var name from a pointer variable name*)
val make_ntsvars_of_ptrvar : string -> Nts_types.nts_genrel_var list 
 
  
val make_ntsvars_of_intvars : string ->  Nts_types.nts_genrel_var list
  
    
val concat_if_first_arg_nonzero : string -> string -> string
 
val concat_comma_both_arg_non_empty : string -> string -> string
 
val pprint_typeinfo_int_nts_var_list : Nts_types.nts_var list -> string
 
    
val make_nd_fun_call : string -> 
  Nts_types.nts_genrel_var list option
  -> Flatac_ndet_nts_support_types.ndet_supp_cnt_val list
  -> Flatac_ndet_nts_support_types.ndet_supp_nts_trans_label


val havocize_var_list : Nts_types.nts_genrel_var list -> 
  Flatac_ndet_nts_support_types.ndet_supp_nts_trans_label


