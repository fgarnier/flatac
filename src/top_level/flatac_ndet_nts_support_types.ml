

open Nts_types

type ndet_supp_cnt_val =
  DetAVal of Nts_types.nts_genrel_arithm_exp
| NDetAVal of Nts_types.nts_genrel_arithm_exp 
| DetNdetBOp of nts_gen_arithm_binop * ndet_supp_cnt_val *ndet_supp_cnt_val
  * nts_base_types
| DetNdetUOp of nts_gen_arithm_unop * ndet_supp_cnt_val * nts_base_types 
| CntINdet
| CntRNdet
| CntBNdet
| CntNdet 



type ndet_supp_cnt_bool =
| ND_CntGenTrue
| ND_CntGenFalse
| ND_CntGenDK
| ND_CntGenNot of ndet_supp_cnt_bool
| ND_CntGenRelComp of cnt_binop * ndet_supp_cnt_val * ndet_supp_cnt_val
| ND_CntGenRel of nts_gen_bool_binop * ndet_supp_cnt_bool * ndet_supp_cnt_bool
| ND_Det_GenRel of Nts_types.nts_gen_relation  (** All deterministic relation*)

exception Type_mismatch_in_detndetarithmetic_operation 
  of ndet_supp_cnt_val * ndet_supp_cnt_val



type ndet_supp_nts_trans_label = ND_CntGenGuard of ndet_supp_cnt_bool
				 | ND_CntGenGuardIf of ndet_supp_cnt_bool
				 | ND_CntGenGuardElse of ndet_supp_cnt_bool
				 
				 | ND_CntGenCall of string * nts_genrel_var list option * ndet_supp_cnt_val list
				     
				     
				 | ND_CntGenHavoc of nts_genrel_var list 








