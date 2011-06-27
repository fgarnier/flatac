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

(** Takes as input an entailement problem and returns the
a biabduction_sol structures such that :

 _ enunciate is a entailement problem which both formulae are
logically equivalent to the correspondig etp formulae.
_ frame_antiframe contains two formulae such that :
     enunciate.left *  frame_antiframe.right |- enunciate.right *  frame_antiframe.left \leadsto (true|Emp) |- (true |Emp) iff the biabdcution problem is SAT. 
*)
let biabduction (etp : entail_problem ) =
  let overall_subst =  ref (subst_id) in
  normalize_ssl etp.left;
  normalize_ssl etp.right;
  let enun = { left = (Ssl.copy etp.left) ; right =( Ssl.copy etp.right) } in
 
  (*Format.printf "Debug :  %s \n %!" ( subst_to_string !overall_subst );*)
  
  entail_r4 (Some(overall_subst)) etp;
  subst_against_ssl !overall_subst enun.left;
  subst_against_ssl !overall_subst enun.right;
 
  subst_against_ssl !overall_subst etp.left;
  subst_against_ssl !overall_subst etp.right;
 
  entail_r1 etp;
  entail_r2 etp;
  entail_ptnil etp;
  (*entail_r6 etp; *)  
  
   let ret = {
    enunciate = enun;
    frame_antiframe = etp
  } in ret
  
  

  
  
