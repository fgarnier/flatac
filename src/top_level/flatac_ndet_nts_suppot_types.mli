open Nts_types


type ndet_supp_cnt_val =
  DetAVal of Nts_types.nts_genrel_arithm_exp
| NDetAVal of Nts_types.nts_genrel_arithm_exp 
| DetNdetBOp of nts_gen_arithm_binop * ndet_supp_cnt_val *ndet_supp_cnt_val
  * nts_base_type
| DetNdetUOp of nts_gen_arithm_up * ndet_supp_cnt_val * nts_base_types 
| CntINdet
| CntRNdet
| CntBNdet
| CntNdet 

type ndet_supp_cnt_bool =
| ND_CntGenTrue
| ND_CntGenFalse
| Nd_CntGenDK
| ND_CntGenNot of ndet_supp_cnt_val
| ND_CntGenRelComp of nts_gen_rel_binop * ndet_supp_cnt_bool * ndet_supp_cnt_bool
| ND_CntGenRel of cnt_binop * ndet_supp_cnt_val * ndet_supp_cnt_val
