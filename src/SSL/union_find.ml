open Ssl_types
open Ssl_types.SSL_lex
open Ssl
open Hashtbl

(** This file contains the current implementations of the
"union find" algorithm used to compute the representant 
of equivalences classes defined by a set of equalities *)

type eqclass = Eq_class of locvar *  ( locvar , unit ) t  
type partition = Partition of (locvar , eqclass ) t
(** Key of partition corresponds to the greatest key of 
the eq class. partition encodes a forest.
*)

exception Element_not_found 


(** find of an element returns the key of the class of this element
or an exception if this element does'nt belong to  *)

let find_repr (lvar : locvar )( part : partition ) =
  let ret = ref (LVar("")) in 
  let eqclass_iterator ( key  :  locvar) ( iterande : locvar ) ( eqc : eqclass) =
    match eqc with 
	Eq_class ( _ , table_lvar ) -> if (Hashtbl.mem table_lvar key) ==true 
	  then ret := iterande
	  else ()
  in
  match part with 
      Partition (table ) ->
	if ( Hashtbl.mem table lvar ) == true (* Case where the query is a key*)
	then let eqcl = (Hashtbl.find table lvar) 
	     in match eqcl with
		 Eq_class (representant , _ ) -> representant
	(* One returns the equivqlence class representant *)
	else 
	  begin
	    Hashtbl.iter ( eqclass_iterator lvar ) table;
	    match (!ret) with 
		LVar("") -> raise Element_not_found 
	      | _ -> !ret
	  end


(** Merges two equivances classes of the partition part into a single one *)

let union (eq_1: eq_class) (eq_2 : eq_class) (part : partition ) =
  
