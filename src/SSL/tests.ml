open Ssl_types
open Ssl
open Ssl_types.SSL_lex
open Printf
open Format

let main () =
   let newf = Ssl.create_ssl_f () in 
   and_atomic_ptnil (Pointsnil(PVar("x"))) newf; 
   and_atomic_affect (Pointsto(PVar("y"),LVar("l1"))) newf;
   add_alloc_cell (LVar("y")) newf ;
   add_alloc_cell (LVar("y")) newf ;
   add_alloc_cell (LVar("x")) newf ;
   let form = formatter_of_out_channel Pervasives.stdout in
   Ssl.pprint_ssl_formula form newf ;
   Format.fprintf form "%!"  

let () = main ()
