exception Found_a_primed_var
val nts_pprint_genvar : Nts_types.nts_genrel_var -> string
val pprint_ntsgen_var_list : Nts_types.nts_genrel_var list -> string
val nts_pprint_nts_typeinfo_genvar : Nts_types.nts_genrel_var -> string
val pprint_typeinfo_nts_genvar_list : Nts_types.nts_genrel_var list -> string
val size_genrel_arithm_deeper_than :
  Nts_types.nts_genrel_arithm_exp -> int -> bool
val size_genrel_deeper_than : Nts_types.nts_gen_relation -> int -> bool
val nts_pprint_genrel_arithm_exp : Nts_types.nts_genrel_arithm_exp -> string
val pprint_gen_rel_arithm_list :
  Nts_types.nts_genrel_arithm_exp list -> string
val nts_pprint_bool_binop :
  string -> Nts_types.nts_gen_bool_binop -> string -> string
val nts_pprint_aritm_binop : Nts_types.nts_gen_arithm_binop -> string
val nts_pprint_genrel : Nts_types.nts_gen_relation -> string
val boolean_relation : Nts_types.nts_genrel_arithm_exp -> bool
val simplify_genrel_bottom_top :
  Nts_types.nts_gen_relation -> Nts_types.nts_gen_relation
val simplify_gen_rel :
  Nts_types.nts_gen_relation -> Nts_types.nts_gen_relation
val is_gen_bool_det : Nts_types.nts_gen_relation -> bool
val is_gen_arithm_exp_a_function : Nts_types.nts_genrel_arithm_exp -> bool
val static_check_if_gen_relation_false : Nts_types.nts_gen_relation -> bool
val static_check_if_gen_translist_unsat :
  Nts_types.nts_trans_label list -> bool
val nts_pprint_gen_trans_label : Nts_types.nts_trans_label -> string
val nts_pprint_gen_trans_label_list :
  Nts_types.nts_trans_label list -> string
