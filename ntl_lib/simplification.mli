type vars_entry = UVars_diary of (Nts_types.nts_var, unit) Hashtbl.t
type vars_entry_by_name =
    UNamedVarsDiary of (string, Nts_types.nts_var) Hashtbl.t
val create_empty_var_diary : unit -> vars_entry
val get_diary_table : vars_entry -> (Nts_types.nts_var, unit) Hashtbl.t
val add_vars_of_cnt_trans_label_to_diary :
  vars_entry -> Nts_types.nts_trans_label -> unit
