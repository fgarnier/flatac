open Ssl_types
open Ssl
open SSL_lex
open Ssl_valid_abs_dom_types
open Var_validity_types
open Var_validity


let create_validity_abstdomain = 
  let sslf = create_ssl_f () in
  let val_map = new_valid_map () in
  {
    ssl_part = sslf ;
    validinfos = val_map ;
  }


(** the fiel ssl_part is mutable and peristant, whereas validinfo isn't.*)
let copy_validity_absdomain (v : ssl_validity_absdom ) =
  let sslf = Ssl.copy v.ssl_part in
  let ret_val = {
    ssl_part =  sslf;
    validinfos = v.validinfos ;
  }
  in 
  ret_val


(* This function returns a new domain value with the validity od vinfo updated
to valid. *)
let set_var_validity_in_absdomain  (domain : ssl_validity_absdom) ( vinfo : Cil_types.varinfo ) (valid : var_valid) =
  {
    ssl_part = domain.ssl_part ;
    validinfos = (set_validity_in domain.validinfos vinfo valid); 
  }
