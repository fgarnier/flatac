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
exception Non_membership (* An equivalence class, or an equivalence
			    class referenced by its representant does 
			    not belong to a partition.*)

let is_rep_of_a_class (lvar : locvar ) ( part : partition ) =
  match part with
      Partition ( table ) -> Hashtbl.mem table lvar 

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

(*  A refaire dans l'autre sens. Il faut trouver un representant d
'une classe d'equivalence vide ou par defaut *)

(** find_class raises  Element_not_found  if lvar is not an element
in the partition *)

let find_class (lvar : locvar ) ( part : partition ) =
  let repres = find_repr lvar part in 
  match part with 
      Partition( table ) ->
	Hashtbl.find table repres (* We get the whole class, hence the
				  return value.*)
(** Merges two equivances classes of the partition part into a single one *)
(** copies all elements of eqmin in the class of eqmax, then removes eqmin
from the partition *)

let union_wrt_order (eqmax : eqclass ) (eqmin : eqclass ) (part : partition) =
 
  match eqmax , eqmin with
      (Eq_class(LVar(key1) , table_1 ) , Eq_class( LVar(key_2) , table_2)) ->
	 let copy_iterator lvar () =
	   Hashtbl.add table_1 lvar ()
	 in
	 Hashtbl.iter copy_iterator table_2;
	 Hashtbl.add table_1 (LVar(key_2)) ();
	 match part with
	     Partition(tablepart) ->
	       Hashtbl.remove tablepart (LVar(key_2))
	

let union (eq_1: eqclass) (eq_2: eqclass) (part : partition ) = 
  match eq_1 , eq_2 with
      (Eq_class(LVar(key_1) , table_1 ), Eq_class(LVar(key_2), table_2)) ->
	if ( (is_rep_of_a_class (LVar(key_1)) part ) && ((is_rep_of_a_class (LVar(key_2)) part )  ) ) then begin
	if ( SSL_lex.order_relation key_1 key_2 ) then 
	   union_wrt_order eq_1 eq_2 part
	else 
	   union_wrt_order eq_2 eq_1 part
	end
	else raise Non_membership





