val valid_name_of_var : string -> string
val offset_name_of_var : string -> string
val make_ntsvars_of_ptrvar : string -> Nts_types.nts_genrel_var list
val valid_sym_cscal_sslv :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Intermediate_language_types.c_scal -> Var_validity_types.var_valid
val valid_sym_ptrexp_sslv :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Intermediate_language_types.c_ptrexp -> Var_validity_types.var_valid
val compile_ntsivar_of_int_cil_lval :
  Cil_types.lval -> Flatac_ndet_nts_support_types.ndet_supp_cnt_val
val compile_cil_exp_2_cnt :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Cil_types.exp -> Flatac_ndet_nts_support_types.ndet_supp_cnt_val
val compile_sym_validity_to_cnt :
  Var_validity_types.var_valid -> Nts_types.nts_genrel_arithm_exp
val compile_cil_scalar_argument_value :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Cil_types.exp -> Compilation_utils_types.il_fun_arguments
val compile_cil_fun_argexpr_2_cnt :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Cil_types.exp -> Compilation_utils_types.il_fun_arguments
val compile_param_list_2_cnt_list :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Cil_types.exp list -> Compilation_utils_types.il_fun_arguments list
val compile_cil_array_2_cnt :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  string -> Cil_types.typ -> Nts_types.nts_array_var
val compile_sizeof_array_type :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Cil_types.typ -> Nts_types.nts_genrel_arithm_exp
