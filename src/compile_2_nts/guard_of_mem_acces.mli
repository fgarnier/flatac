exception Unhandled_offset_type of string
exception Dont_know_how_to_generate_guard

val make_offset_locpvar : Ssl_types.SSL_lex.ptvar -> Nts_types.nts_genrel_var
val make_validity_varpvar :
  Ssl_types.SSL_lex.ptvar -> Nts_types.nts_genrel_var
val get_lsizename_of_locvar : Ssl_types.SSL_lex.locvar -> string
val get_lbasename_of_locvar : Ssl_types.SSL_lex.locvar -> string
val make_size_locvar :
  Ssl_types.SSL_lex.locvar ->
  Global_mem.global_mem_manager ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_gen_relation list
val make_size_locvar_genrel :
  Ssl_types.SSL_lex.locvar ->
  Global_mem.global_mem_manager ->
  Nts_types.nts_genrel_arithm_exp -> Nts_types.nts_gen_relation
val offset_of_mem_access_to_cnt :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Cil_types.typ -> Cil_types.offset -> Nts_types.nts_genrel_arithm_exp
val cnt_guard_of_array_access :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Cil_types.offset -> Cil_types.typ -> Nts_types.nts_gen_relation
val cnt_guard_of_mem_access :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Cil_types.exp -> Nts_types.nts_gen_relation
val cnt_guard_of_mem_access_enode :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Cil_types.exp_node -> Nts_types.nts_gen_relation
val mem_guards_of_funcall_arg_list :
  Ssl_valid_abs_dom_types.ssl_validity_absdom ->
  Cil_types.exp list -> Nts_types.nts_gen_relation
