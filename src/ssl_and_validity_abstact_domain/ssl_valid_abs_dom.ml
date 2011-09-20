open  Ssl_validity_absdomain

(** the fiel ssl_part is mutable and peristant, whereas validinfo isn't.*)
let copy_validity_absdomain (v : ssl_validity_absdom ) =
  let sslf = Ssl.copy v.ssl_part in
  let ret_val = {
    ssl_part =  sslf;
    validinfo = v.validinfo ;
  }
  in 
  ret_val
