(**
 Type definition for numerical transition systems' relations.
This file is part of the Ocaml NTL lib, developped in the
Team DCS of the Verimag laboratory.


Contact florent dot garnier at imag dot fr for comments
suggestions or questions.


This file as well as all the files of the Ocaml Ntl library
are realeased under the terms of the GNU LGPL version 2.1 
Licence.


*)


exception Not_LiPVar
exception Not_LiVar
exception Not_Guard
exception Invalid_nts_expression
exception Not_an_if_then_else_condition_guard

type nts_quantifier = NtsExists | NtsForall
type nts_base_types = NtsIntType | NtsRealType | NtsBoolType | NtsUnTyped

type nts_var = NtsVar of string * nts_base_types

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

type nts_gen_bool_cst = CntBTrue | CntBFalse

type nts_gen_bool_binop = 
    CntGenBAnd 
  | CntGenBOr

type nts_base_type_cst = 
    CntGenICst of Big_int.big_int 
  | CntGenFCst of float
  | CntGenBCst of nts_gen_bool_cst

type nts_symbolic_constant = 
    CntSymCst of string * nts_base_types

type nts_genrel_arithm_exp =
    CntGenCst of nts_base_type_cst * nts_base_types
  | CntGenSymCst of nts_symbolic_constant * nts_base_types
  | CntGenVar of nts_genrel_var
  | CntGenArithmBOp of nts_gen_arithm_binop * nts_genrel_arithm_exp *
      nts_genrel_arithm_exp * nts_base_types
  | CntGenArithmUOp of nts_gen_arithm_unop * nts_genrel_arithm_exp *
      nts_base_types

type ref_nts_array =
    RefBasicTypeArray of nts_base_types
  | RefMulDimArray of ref_nts_array

type nts_array =
    RefNtsArray of ref_nts_array
  | FixedSizeNtsArray of fixed_size_nts_array
and fixed_size_nts_array =
    FixedSizeBasicTypeNtsArray of nts_genrel_arithm_exp * nts_base_types
  | FixedSizeMulDimNtsArray of nts_genrel_arithm_exp * nts_array

type nts_array_var = NtsArrayVar of string * nts_array * nts_base_types

type nts_gen_relation =
    CntGenRel of cnt_binop * nts_genrel_arithm_exp * nts_genrel_arithm_exp
  | CntGenRelComp of nts_gen_bool_binop * nts_gen_relation * nts_gen_relation
  | CntGenNot of nts_gen_relation
  | CntGenTrue
  | CntGenFalse
  | CntQVarsGenRel of nts_genrel_var list * nts_quantifier * nts_gen_relation

type nts_trans_label =
    CntGenGuard of nts_gen_relation
  | CntGenCall of string * nts_genrel_var list option *
      nts_genrel_arithm_exp list
  | CntGenHavoc of nts_genrel_var list
