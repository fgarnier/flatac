open Nts_types

(* Types used to deal with function calls and returned values that
includes information concerning the validity of pointers and integer
values*)

type il_ptr_fun_arg =
    {
      base_of_exp : Ssl_types.SSL_lex.locvar ;
      offset_of_exp : nts_genrel_arithm_exp;
      validity_of_ptr_exp : nts_genrel_arithm_exp ;
    }

type il_int_fun_arg =
    {
      expr : nts_genrel_arithm_exp;
      validity_of_exp : nts_genrel_arithm_exp ;
    }
    

type il_fun_arguments = IlScalArg of il_int_fun_arg
| IlPtrArg of il_ptr_fun_arg

