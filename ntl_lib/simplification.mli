
(* Type of a variable diary *)
type vars_entry = UVars_diary of (Nts_types.nts_var, unit) Hashtbl.t


type vars_entry_by_name =
    UNamedVarsDiary of (string, Nts_types.nts_var) Hashtbl.t


val create_empty_var_diary : unit -> vars_entry

val get_diary_table : vars_entry -> (Nts_types.nts_var, unit) Hashtbl.t

val add_vars_of_cnt_trans_label_to_diary :
  vars_entry -> Nts_types.nts_trans_label -> unit

val add_vars_of_trans_label_list_to_diary :
  vars_entry ->  Nts_types.nts_trans_label list -> unit


(** Answer yes when the variable is listed in the entry, false
in the opposite case.*)

val contains_var : vars_entry -> Nts_types.nts_var -> bool
val contains_nts_genrel_var : vars_entry -> Nts_types.nts_genrel_var -> bool
