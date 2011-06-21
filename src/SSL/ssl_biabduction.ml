(** This files contains the implementation of the biabduction algorithm.*)

open Ssl_types
open Ssl
open SSL_lex
open Ssl_entailement
open Ssl_normalization
open Ssl_substitution


let biabduction (etp : entail_problem ) =
  normalize_ssl etp.left;
  normalize_ssl etp.right;
  entail_r4 etp;
  entail_r6 etp;
  entail_r1 etp;
  entail_r2 etp
  
  
  
  
