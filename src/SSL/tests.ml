open Ssl_types
open Ssl
open Ssl_types.SSL_lex
open Printf
open Format

let main () =
   let newf = Ssl.create () in 
   let form = formatter_of_out_channel Pervasives.stdout in
   Format.fprintf  form "coucou \n";
     Ssl.print_affect form (newf.affectations); Format.fprintf form "%!"  
let () = main ()
