(** This files contains the implementation of the biabduction algorithm.*)

open Ssl_types
open Ssl
open SSL_lex
open Ssl_entailement
open Ssl_normalization
open Ssl_substitution
open Debug_printers

type biabduct_sol = {
enunciate : entail_problem ;
frame_antiframe : entail_problem ;
}

let biabduction (etp : entail_problem ) =
  let overall_subst =  ref (subst_id) in
  normalize_ssl etp.left;
  normalize_ssl etp.right;
  let enun = { left = (Ssl.copy etp.left) ; right =( Ssl.copy etp.right) } in
  entail_r4 (Some(overall_subst)) etp;
  Format.printf "Debug :  %s \n %!" ( subst_to_string !overall_subst );
  entail_r6 etp;
  entail_r1 etp;
  entail_r2 etp;
  subst_against_ssl !overall_subst enun.left;
  subst_against_ssl !overall_subst enun.right;
  

  let ret = {
    enunciate = enun;
    frame_antiframe = etp
  } in ret
  
  

  
  
