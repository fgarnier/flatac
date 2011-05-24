open Ssl_types
open Ssl
open Ssl_types.SSL_lex
open Printf
open Format

let main () =
   let newf = Ssl.create_ssl_f () in 
   and_atomic_eq (Eqloc(LVar("l_1"),LVar("l_4"))) newf;
   and_atomic_ptnil (Pointsnil(PVar("x"))) newf;
   add_quant_var (LVar("l_1")) newf;
   and_atomic_affect (Pointsto(PVar("y"),LVar("l1"))) newf;
   add_alloc_cell (LVar("y")) newf ;
   add_alloc_cell (LVar("y")) newf ;
   add_alloc_cell (LVar("x")) newf ;
   let newf2 =Ssl.create_ssl_f () in
    add_quant_var(LVar("l_1")) newf2; add_quant_var(LVar("l_5")) newf2;
   and_atomic_ptnil (Pointsnil(PVar("x1"))) newf2; 
   and_atomic_affect (Pointsto(PVar("y1"),LVar("l3"))) newf2;
   add_alloc_cell (LVar("y")) newf2 ;
   add_alloc_cell (LVar("y4")) newf2 ;
   add_alloc_cell (LVar("x6")) newf2 ;
   let f4 = star_sep newf newf2 in
   let form = formatter_of_out_channel Pervasives.stdout in
   Ssl.pprint_ssl_formula form f4 ;
   Format.fprintf form "%!"  

let () = main ()
