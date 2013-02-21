(**
During the compilation of the argument calls of a function,
we need to keep the fact that a value comes from a Pointer
evaluation or a scalar value.


This file is released under the terms of the GNU LGPL v 2.1
Licence.

It is part of the FRAMAC Flata-c plugin, developped
by Florent Garnier in the Verimag Laboratory.


2013.

*)



type il_ptr_fun_arg = {
  base_of_exp : Ssl_types.SSL_lex.locvar;
  offset_of_exp : Flatac_ndet_nts_support_types.ndet_supp_cnt_val;
  validity_of_ptr_exp : Flatac_ndet_nts_support_types.ndet_supp_cnt_val;
}


type il_int_fun_arg = {
  expr : Flatac_ndet_nts_support_types.ndet_supp_cnt_val;
  validity_of_exp : Flatac_ndet_nts_support_types.ndet_supp_cnt_val;
}


type il_fun_arguments =
    IlScalArg of il_int_fun_arg
  | IlPtrArg of il_ptr_fun_arg
