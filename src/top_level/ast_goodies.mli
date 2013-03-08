exception Debug_info of string
exception Not_an_array_offset
exception Not_Ablock
exception No_2_successors_for_if_successor of int
exception Less_than_two_elem_in_this_list
exception Bothparameter_are_None_option
val pprint_fist_elem_of_block : Format.formatter -> Cil_types.block -> unit
val pprint_first_elem_of_stmtkind :
  Format.formatter -> Cil_types.stmtkind -> unit
val pprint_statement_head : Format.formatter -> Cil_types.stmt -> unit
val dummy_position : Lexing.position * Lexing.position
val get_location_of_instr : Cil_types.instr -> Cil_types.location
val get_location_of_stmt_kind : Cil_types.stmtkind -> Cil_types.location
val pprint_position : Lexing.position -> string
type ast_li_ptr_field =
    AstGLiIntStarOfPtrField of string * string
  | AstGLiPtrStarOfPtrField of string * string
  | AstGLiPtrOfField of string * string
  | AstGliIntOfField of string * string
  | AstGLiPVar of string
  | AstGLiIntVar of string
  | AstGLiStarOfPVar of string
val is_lval_of_mem_access : Cil_types.lval -> bool
val is_exp_array : Cil_types.exp -> bool
val base_type_of_muldim_array_type : Cil_types.typ -> Cil_types.typ
val is_expnode_an_array : Cil_types.exp_node -> bool
val string_of_ptvar : Ssl_types.SSL_lex.ptvar -> string
val name_of_non_assigned_ret_val : unit -> string
val debug_out : Format.formatter
val pprint_lexing_infos : Cil_types.location -> string
val first_stmt_of_block : Cil_types.block -> Cil_types.stmt
val get_two_first_elem_of_list : 'a list -> 'a * 'a
val get_if_then_first_block_stmts :
  Cil_types.stmt ->
  Cil_types.block ->
  Cil_types.block -> Cil_types.stmt option * Cil_types.stmt option
val get_some_from_option_pair : 'a option -> 'a option -> 'a
val loc_of_instr : Cil_types.instr -> Cil_types.location
val loc_stmt_opt : Cil_types.stmt -> Cil_types.location option
val pprint_skind_basic_infos : Cil_types.stmtkind -> string
val pprint_unop_op : Cil_types.unop -> string
val pprint_binop_op : Cil_types.binop -> string
val pprint_comp_infos : Cil_types.compinfo -> string
val pprint_enum_item : Cil_types.enumitem -> string
val pprint_field_info : Cil_types.fieldinfo -> string
val pprint_offset : Cil_types.offset -> string
val pprint_type_infos : Cil_types.typeinfo -> string
val pprint_enum_infos : Cil_types.enuminfo -> string
val pprint_ciltypes : Cil_types.typ -> string
val pprint_cil_constant : Cil_types.constant -> string
val pprint_cil_exp : Cil_types.exp -> string
val pprint_attr_list_l_fold :
  int ref -> Cil_types.attrparam -> string -> string
val pprint_attrparam : Cil_types.attrparam -> string
val pprint_attribute : Cil_types.attribute -> string
val pprint_attributes : Cil_types.attributes -> string
val pprint_slocal_var : Cil_types.varinfo -> string
val pprint_slocal_vars : Cil_types.varinfo list -> string
val get_subfield_name :
  string -> Cil_types.fieldinfo -> Cil_types.offset -> string
val get_lval_under_cast : Cil_types.exp -> Cil_types.exp
val get_pvar_from_exp_node : Cil_types.exp_node -> Ssl_types.SSL_lex.ptvar
val get_pvar_from_exp : Cil_types.exp -> Ssl_types.SSL_lex.ptvar
val get_pvar_from_array_element_access :
  Cil_types.exp -> Ssl_types.SSL_lex.ptvar
val get_pvar_from_mem_access : Cil_types.exp_node -> Ssl_types.SSL_lex.ptvar
val get_first_ptvar_from_lparam :
  Cil_types.exp list -> Ssl_types.SSL_lex.ptvar
val max_args_numbers_of_callees : Cil_types.stmt list -> int
val is_default_label : Cil_types.label -> bool
val stmt_has_default_label : Cil_types.stmt -> bool
val has_default_label : Cil_types.stmt list -> bool

(*val big_int_of_my_bigint : My_bigint.t -> Big_int.big_int*) 
