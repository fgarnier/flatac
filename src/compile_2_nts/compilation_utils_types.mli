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
  offset_of_exp : Nts_types.nts_genrel_arithm_exp;
  validity_of_ptr_exp : Nts_types.nts_genrel_arithm_exp;
}


type il_int_fun_arg = {
  expr : Nts_types.nts_genrel_arithm_exp;
  validity_of_exp : Nts_types.nts_genrel_arithm_exp;
}


type il_fun_arguments =
    IlScalArg of il_int_fun_arg
  | IlPtrArg of il_ptr_fun_arg
