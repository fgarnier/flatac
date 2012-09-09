(** 

This is the interface file for the flata-c numerical transition systems.
Most of those functions are about to be definitively relaced by more
generic function and interfaces.


See nts_generic.ml and nts_genreic.mli for type definitions and functions
to handle numerical transitions systems.

(C) Verimag 2012

Questions and/remarks : write to florent dot garnier at imag dot fr

*)

exception CntInvalidExpression
exception Neq_arithm_expr

val is_ntisvar_det : Nts_types.nts_var -> bool
val negate_cntbool_shallow : Nts_types.cnt_bool -> Nts_types.cnt_bool
val nts_pprint_nts_var : Nts_types.nts_var -> string
val nts_pprint_nts_typeinfo_var : Nts_types.nts_var -> string
val pprint_nts_var_list : Nts_types.nts_var list -> string
val pprint_nts_and_prime_var_list : Nts_types.nts_var list -> string
val valid_name_of_var : string -> string
val offset_name_of_var : string -> string
val make_ntsvars_of_ptrvar : string -> Nts_types.nts_var list
val make_ntsvars_of_intvars : string -> Nts_types.nts_var list
val concat_if_first_arg_nonzero : string -> string -> string
val concat_comma_both_arg_non_empty : string -> string -> string
val pprint_typeinfo_int_nts_var_list : Nts_types.nts_var list -> string
val pprint_typeinfo_nts_var_list : Nts_types.nts_var list -> string
val size_arithm_exp : Nts_types.cnt_arithm_exp -> int
val size_arithmexp_deeper_than : Nts_types.cnt_arithm_exp -> int -> bool
val size_boolexp_deeper_than : Nts_types.cnt_bool -> int -> bool
val cnt_pprint_arithm_exp : Nts_types.cnt_arithm_exp -> string
val simplify_bottom_top : Nts_types.cnt_bool -> Nts_types.cnt_bool
val simplify_cnt_boolexp : Nts_types.cnt_bool -> Nts_types.cnt_bool
val static_check_if_false : Nts_types.cnt_bool -> bool
val static_check_if_translist_unsat : Nts_types.cnt_trans_label list -> bool
val cnt_pprint_boolexp : Nts_types.cnt_bool -> string
val cnt_simplify_and_pprint_boolexp : Nts_types.cnt_bool -> string
val is_cnt_bool_det : Nts_types.cnt_bool -> bool
val is_cnt_arithm_exp_a_function : Nts_types.cnt_arithm_exp -> bool
val arg_expr_left_folder : string -> Nts_types.cnt_arithm_exp -> string
val pprint_arg_list : Nts_types.cnt_arithm_exp list -> string
val cnt_pprint_translabel : Nts_types.cnt_trans_label -> string
val need_split_transition : Nts_types.cnt_trans_label list -> bool
val havocise :
  Nts_types.cnt_trans_label list -> Nts_types.cnt_trans_label list
val split_guard_call_transition :
  Nts_types.cnt_trans_label list ->
  Nts_types.cnt_trans_label list * Nts_types.cnt_trans_label list
val name_ndet_arg : int -> Nts_types.nts_var
val havocise_label :
  Nts_types.cnt_trans_label list -> Nts_types.cnt_trans_label list
val compare_nts_var : Nts_types.nts_var -> Nts_types.nts_var -> bool
val compare_cnt_arithm_exp :
  Nts_types.cnt_arithm_exp -> Nts_types.cnt_arithm_exp -> bool
val compare_cnt_arithm_expr_list :
  Nts_types.cnt_arithm_exp list -> Nts_types.cnt_arithm_exp list -> bool
val compare_cnt_bool : Nts_types.cnt_bool -> Nts_types.cnt_bool -> bool
val compare_nts_var_list :
  Nts_types.nts_var list -> Nts_types.nts_var list -> bool
val compare_cnt_trans_label_guard :
  Nts_types.cnt_trans_label -> Nts_types.cnt_trans_label -> bool
val compare_tranlabel_list :
  Nts_types.cnt_trans_label list -> Nts_types.cnt_trans_label list -> bool
val hash_nts_var : Nts_types.nts_var -> int
val hash_cnt_arithm_constructor : Nts_types.cnt_arithm_exp -> int
val hash_cnt_bool_constructor : Nts_types.cnt_bool -> int
