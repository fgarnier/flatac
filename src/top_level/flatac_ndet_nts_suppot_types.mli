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


