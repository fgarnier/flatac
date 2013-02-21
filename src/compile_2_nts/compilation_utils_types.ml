open Nts_types
open Flatac_ndet_nts_support_types

(* Types used to deal with function calls and returned values that
includes information concerning the validity of pointers and integer
values*)

type il_ptr_fun_arg =
    {
      base_of_exp : Ssl_types.SSL_lex.locvar ;
      offset_of_exp : ndet_supp_cnt_val;
      validity_of_ptr_exp : ndet_supp_cnt_val;
    }

type il_int_fun_arg =
    {
      expr : ndet_supp_cnt_val ;
      validity_of_exp : ndet_supp_cnt_val;
    }
    

type il_fun_arguments = IlScalArg of il_int_fun_arg
| IlPtrArg of il_ptr_fun_arg

