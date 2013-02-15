

open Nts_types

type ndet_supp_cnt_val =
  DetAVal of Nts_types.nts_genrel_arithm_exp
| NDetAVal of Nts_types.nts_genrel_arithm_exp 
| DetNdetBOp of nts_gen_arithm_binop * ndet_supp_cnt_val *ndet_supp_cnt_val
  * nts_base_types
| DetNdetUOp of nts_gen_arithm_binop * ndet_supp_cnt_val * nts_base_types 
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
exception Type_mismatch_in_detndetarithmetic_operation 
  of ndet_supp_cnt_val * ndet_supp_cnt_val
