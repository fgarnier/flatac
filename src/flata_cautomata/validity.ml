open Cil_types
open Intermediate_language
open Ssl_types
open Ssl
open SSL_lex




let rec base_ptrexp (sslf : ssl_formula )( ptr_exp : c_ptrexp ) =
  match ptr_exp with 
      LiPVar ( _ , LiIntPtr(vname)) ->
	get_ptr_affectation sslf (PVar(vname))

    | LiPlusPI ( cptr , _ ) -> base_ptrexp sslf cptr
    | LiIndexPI ( cptr , _ ) -> base_ptrexp sslf cptr
    | LiMinusPI ( cptr , _ ) -> base_ptrexp sslf cptr
  

let rec valid_ptrexp ( ptrexp :  c_ptrexp ) =
  match ptrexp with 
      LiPvar
