open Ssl_types
open Ssl
open Ssl_types.SSL_lex
open Printf
open Format
open Union_find

let main () =
   let newf = Ssl.create_ssl_f () in
   and_atomic_eq (Eqloc(LVar("l_1"),LVar("l_4"))) newf;
   and_atomic_eq (Eqloc(LVar("l_4"),LVar("l_5"))) newf;  
   and_atomic_eq (Eqloc(LVar("l_2"),LVar("l_4"))) newf;
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
   Format.fprintf form "%!"  ;
   Format.fprintf form "\n Testing unify eq loc \n";
   let test_unif_eq = Ssl.create_ssl_f () in
   and_atomic_affect (Pointsto(PVar("y1"),LVar("l3"))) test_unif_eq;
   and_atomic_affect (Pointsto(PVar("y1"),LVar("j"))) test_unif_eq;
   and_atomic_affect (Pointsto(PVar("y1"),LVar("k"))) test_unif_eq;
   and_atomic_affect (Pointsto(PVar("y1"),LVar("w3"))) test_unif_eq;
   and_atomic_affect (Pointsto(PVar("y1"),LVar("jl"))) test_unif_eq;
   and_atomic_affect (Pointsto(PVar("z"),LVar("f"))) test_unif_eq;
   and_atomic_affect (Pointsto(PVar("x"),LVar("d56"))) test_unif_eq;
   and_atomic_affect (Pointsto(PVar("z"),LVar("jlo"))) test_unif_eq;
   and_atomic_affect (Pointsto(PVar("y1"),LVar("mk"))) test_unif_eq;


   let aff_y1 = Hashtbl.find test_unif_eq.pure.affectations (PVar("y1")) in
   let aff_z = Hashtbl.find test_unif_eq.pure.affectations (PVar("z")) in
   let list_eq = ( unify_eq aff_y1 ) @ ( unify_eq aff_z) in
   print_eqlist form  list_eq;
   Format.fprintf form "\n %!";
   let part = eqlist_to_partition list_eq in
     pprint_partition form part
   
   

let () = main ()
