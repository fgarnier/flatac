open Ssl_types
open Ssl_types.SSL_lex
open Ssl
open Hashtbl

(** This file contains the current implementations of the
"union find" algorithm used to compute the representant 
of equivalences classes defined by a set of equalities 

for questions or comment, write to florent-dot-garnier!at!imag^dot^fr

*)

type eqclass = {
  mutable repres : locvar ;      (*Member representing the class*)
  members : ( locvar , unit ) t; (*All members but the representant*)
}
  
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
    if (Hashtbl.mem eqc.members key) ==true 
	  then ret := iterande
	  else ()
  in
  match part with 
      Partition (table ) ->
	if ( Hashtbl.mem table lvar ) == true (* Case where the query is a key*)
	then let eqcl = (Hashtbl.find table lvar) 
	     in  eqcl.repres
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
from the partition. One must ensure that eqmax and eqmin are both
member of part. The function union gurantee that while calling to union_wrt_order.*)

let merge_wrt_order (eqmax : eqclass ) (eqmin : eqclass ) (part : partition) =
  let copy_iterator lvar () =
    Hashtbl.add eqmax.members lvar ()
  in
  Hashtbl.iter copy_iterator eqmin.members;
  Hashtbl.add eqmax.members eqmin.repres ();
  match part with
      Partition(tablepart) ->
	Hashtbl.remove tablepart (eqmin.repres)

(* Merges two classes of the partition part, into a single one *)	

let merge_class (eq_1: eqclass) (eq_2: eqclass) (part : partition ) = 
 
  if  ((is_rep_of_a_class eq_1.repres part ) && (is_rep_of_a_class eq_2.repres part   ) ) then begin
    if ( Ssl.cmp_lex_lvar eq_1.repres eq_2.repres ) then 
      merge_wrt_order eq_1 eq_2 part
    else 
      merge_wrt_order eq_2 eq_1 part
  end
  else raise Non_membership


let merge_from_equality (equality : SSL_lex.eq )(part : partition ) =
  match equality with 
      Eqloc(l1,l2)->
	let classe1 = find_class l1 part in
	let classe2 = find_class l2 part in
	merge_class classe1 classe2 part
	  (** Et puis basta !*)




(** 
This fonctions performs the following operation :
If both members of an equation are not member of a 
partition, then one creates a new class that contains
those two elements.

If the greatest element belongs to a class and the
smallest doesn't then, one add both element to
this class.


If the smallest element belong to a class but the biggest
doesn't, then :

the biggest element of the equation becomes the representant
of the class, and the former representant is moved together 
with the other element of the class.

The last case corresponds to the fact that both elements
already belong to equivalence classe,
 In this case, one shall merge both classes, whenever those
two classes are different.
 
 *)

let add_elem_to_class ( lvar : SSL_lex.locvar )( ecl : eqclass )( part : partition) =
  if (( cmp_lex_lvar lvar  ecl.repres ) == true) 
  then
    match part with
	Partition ( table ) ->
	  try
	    let update_class = Hashtbl.find table ecl.repres in
	    Hashtbl.remove table update_class.repres ; (*removing
						       binding from
						       patition*)
	    
	    update_class.repres <- lvar ;
	    Hashtbl.add update_class.members ecl.repres ();
	    Hashtbl.add table  lvar update_class
	  with
	      
  
  else
  
    



(********************************************************)


(*

let add_eq_to_partition (equation : SSL_lex.eq)(part : partition) =
  let eqsort = ref equation in
  match equation with 
      Eqloc ( lg , ld ) -> 
	if ( cmp_lex_lvar lg ld ) == false
	then eqsort := (Eqloc ( ld , lg ));
	
*)
  

let eqlists_to_partition( eqlist : SSL_lex.eq list ) =
  let order_elem (s : SSL_lex.eq) =
    match s with 
	Eqloc (LVar (lg) , LVar (ld) ) ->  
	  if (SSL_lex.order_relation lg ld)!= true then
	    Eqloc (LVar (lg) , LVar (ld) ) 
	  else 
  	    Eqloc (LVar (lg) , LVar (ld) )
  in
  let part_table = Hashtbl.create SSL_lex.size_hash in
  let ordered_list = List.map  order_elem eqlist in
  let create_part_iterator (s : SSL_lex.eq) =
    match s with 
	Eqloc ( lg , ld ) -> 
	  let eq_class_table = Hashtbl.create SSL_lex.size_hash in
	  Hashtbl.add eq_class_table ld ();
	  Hashtbl.add part_table  lg { repres = lg ; members = eq_class_table }
  in  
  List.iter create_part_iterator ordered_list;
  
