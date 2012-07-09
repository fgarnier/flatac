exception Not_LiPVar
exception Not_LiVar
exception Not_Guard
exception Invalid_nts_expression
exception Not_an_if_then_else_condition_guard
type nts_quantifier = NtsExists | NtsForall
type nts_base_types = NtsIntType | NtsRealType | NtsBoolType | NtsNatType
type nts_var =
    NtsIVar of string
  | NtsINdetVar of string
  | NtsRVar of string
  | NtsBVar of string
  | NtsMiscType of string
  | NtsNVar of string
type nts_primed_type = NtsPrimed | NtsUnPrimed
type nts_genrel_var = NtsGenVar of nts_var * nts_primed_type
type cnt_binop = CntEq | CntNeq | CntLeq | CntLt | CntGt | CntGeq
type nts_gen_arithm_binop =
    CntGenSum
  | CntGenMinus
  | CntGenProd
  | CntGenDiv
  | CntGenMod
type nts_gen_arithm_unop = CntGenUMinus
type nts_gen_bool_binop = CntGenBAnd | CntGenBOr
type cnt_arithm_exp =
    CntCst of Big_int.big_int
  | CntNdet
  | CntNdetVar of string
  | CntSymCst of string
  | CntVar of nts_var
  | CntMinus of cnt_arithm_exp * cnt_arithm_exp
  | CntSum of cnt_arithm_exp * cnt_arithm_exp
  | CntProd of cnt_arithm_exp * cnt_arithm_exp
  | CntMod of cnt_arithm_exp * cnt_arithm_exp
  | CntUnMin of cnt_arithm_exp
  | CntDiv of cnt_arithm_exp * cnt_arithm_exp
  | CntInvalidExp
type nts_genrel_arithm_exp =
    CntGenCst of Big_int.big_int
  | CntGenNdet
  | CntGenNdetVar of string
  | CntGenSymCst of string
  | CntGenVar of nts_genrel_var
  | CntGenArithmBOp of nts_gen_arithm_binop * nts_genrel_arithm_exp *
      nts_genrel_arithm_exp
  | CntGenArithmUOp of nts_gen_arithm_unop * nts_genrel_arithm_exp
  | CntGenInvalidExp
type ref_nts_array =
    RefBasicTypeArray of nts_base_types
  | RefMulDimArray of ref_nts_array
type nts_array =
    RefNtsArray of ref_nts_array
  | FixedSizeNtsArray of fixed_size_nts_array
and fixed_size_nts_array =
    FixedSizeBasicTypeNtsArray of cnt_arithm_exp * nts_base_types
  | FixedSizeMulDimNtsArray of cnt_arithm_exp * nts_array
type nts_array_var = NtsArrayVar of string * nts_array
type cnt_bool =
    CntBool of cnt_binop * cnt_arithm_exp * cnt_arithm_exp
  | CntNot of cnt_bool
  | CntBTrue
  | CntBFalse
  | CntBAnd of cnt_bool * cnt_bool
  | CntBOr of cnt_bool * cnt_bool
type nts_gen_relation =
    CntGenRel of cnt_binop * nts_genrel_arithm_exp * nts_genrel_arithm_exp
  | CntGenRelComp of nts_gen_bool_binop * nts_gen_relation * nts_gen_relation
  | CntGenNot of nts_gen_relation
  | CntGenTrue
  | CntGenFalse
  | CntQVarsGenRel of nts_genrel_var list * nts_quantifier * nts_gen_relation
type cnt_trans_label =
    CntGuard of cnt_bool
  | CntGuardIf of cnt_bool
  | CntGuardElse of cnt_bool
  | CntCall of string * nts_var list option * cnt_arithm_exp list
  | CntAffect of nts_var * cnt_arithm_exp
  | CntNdetAssign of nts_var
  | CntHavoc of nts_var list
  | CntGenGuard of nts_gen_relation
type nts_trans_label =
    CntGenGuard of nts_gen_relation
  | CntGenCall of string * nts_genrel_var list option *
      nts_genrel_arithm_exp list
  | CntGenHavoc of nts_genrel_var list
