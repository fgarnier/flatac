open Ssl_types
open Ssl
open SSL_lex
open Printf

class global_mem = object (self)
  val mutable gmalloc_id = 1
  

  method lvar_from_malloc () =
    let lval_name = sprintf "mid_%d" gmalloc_id in
    gmalloc_id <- (gmalloc_id + 1 );
    LVar(lval_name)

end;;
