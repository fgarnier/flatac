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
