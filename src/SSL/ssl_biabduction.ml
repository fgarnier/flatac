(** This files contains the implementation of the biabduction algorithm.*)

open Ssl_types
open Ssl
open SSL_lex
open Ssl_entailement
open Ssl_normalization
open Ssl_substitution
open Debug_printers


let biabduction (etp : entail_problem ) =
  let overall_subst =  ref (subst_id) in
  normalize_ssl etp.left;
  normalize_ssl etp.right;
  entail_r4 (Some(overall_subst)) etp;
  Format.printf "Debug : \n %s %!" ( subst_to_string !overall_subst );
  entail_r6 etp;
  entail_r1 etp;
  entail_r2 etp
  
  
  
  
