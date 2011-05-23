open Ssl_types
open Ssl
open Ssl_types.SSL_lex
open Printf
open Format

let main () =
   let newf = Ssl.create_ssl_f () in 
   and_atomic_ptnil (Pointsnil(PVar("x"))) newf; 
   and_atomic_affect (Pointsto(PVar("y"),LVar("l1"))) newf;
   let form = formatter_of_out_channel Pervasives.stdout in
   Format.fprintf  form "coucou \n";
   Ssl.pprint_ssl_formula form newf ;
   Format.fprintf form "%!"  

let () = main ()
