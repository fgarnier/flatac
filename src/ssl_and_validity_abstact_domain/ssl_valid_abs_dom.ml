open Ssl_types
open Ssl
open SSL_lex
open Ssl_validity_absdomain


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
    validinfo = v.validinfo ;
  }
  in 
  ret_val
