

open Ssl_types
open Ssl
open Ssl_types.SSL_lex
open Printf
open Format
open Union_find
open Hashtbl
open List
open Ssl_substitution
open Ssl_normalization
open Ssl_decision
open Debug_printers
open Ssl_entailement
open Ssl_biabduction

let main () =   	 
  let taille_test = 256000 in
  let form = formatter_of_out_channel Pervasives.stdout in
  let phi_g = create_ssl_f () in
  let phi_d = create_ssl_f () in
  for i=1 to taille_test do 
    let pt_name = sprintf "x_%d" i in
    let lv_name = sprintf "l_%d" i in
    and_atomic_affect (Pointsto(PVar(pt_name),LVar(lv_name))) phi_g;
  done;
  for i=1 to ( taille_test - 1 ) do
     let pt_name = sprintf "x_%d" i in
     and_atomic_affect (Pointsto(PVar(pt_name),LVar("z500"))) phi_g
  done;
  
    add_alloc_cell (LVar("l_1")) phi_g;
  
  and_atomic_ptnil (Pointsnil(PVar("z"))) phi_g;
 (* and_atomic_ptnil (Pointsnil(PVar("z"))) phi_d;*)
  (*add_quant_var (LVar("l1")) phi_g;
  and_atomic_affect (Pointsto(PVar("x"),LVar("m1"))) phi_d;
  and_atomic_affect (Pointsto(PVar("y"),LVar("l2"))) phi_d;
  add_alloc_cell (LVar("l1")) phi_d;
  add_quant_var (LVar("m1")) phi_d;*)
  
  normalize_ssl phi_g;
  pprint_ssl_formula  form  phi_g
  
(*
  let entp = {left = phi_g; right = phi_d ;} in
  Format.fprintf form "********* Entailement problem ********* \n";
  pprint_entailproblem form entp;
  let biabduct_res = biabduction entp in
  Format.fprintf form "********* Entailement problem After biabduction ********* \n";
  Format.fprintf form "********* Enunciate after renaming ********* \n";
  pprint_entailproblem form biabduct_res.enunciate;
  Format.fprintf form "********* Frame and antiframe ********* \n";
  pprint_entailproblem form biabduct_res.frame_antiframe;
  
  Format.fprintf form " Star of Frame * left and Antiframe * right \n";
  let etpf = { 
    left = (star_sep biabduct_res.enunciate.left biabduct_res.frame_antiframe.right ) ;
    right = (star_sep biabduct_res.enunciate.right biabduct_res.frame_antiframe.left ) 
  } in
  pprint_entailproblem form etpf;
  Format.fprintf form " computing the entailement of the previous entailement \n";
  ssl_entailement etpf;
  pprint_entailproblem form etpf
*)
  
	 



           


let () = main ()
