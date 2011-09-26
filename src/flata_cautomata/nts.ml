open Nts_types

(*  
This files contains the functions used to deal with Numerical Transition
Systems, a.k.a. counter automata.

_ Translating intermediate language into nts language.

If you have any questions or suggestions, please
type the following address starting from the
rightmost caracter until you reach the leftmost
one in your favorite mail editor : rf.gami-at-reinrag.tnerolf,
and then send me your message.
*)





(* This part defines the function used to export the nts trees into
human readable format.*)

let nts_pprint_nts_ivar (x : nts_var ) = 
  match x with 
      NtsIVar( vname ) -> vname

let rec size_arithm_exp ( exp : cnt_arithm_exp ) =
  match exp with 
       CntCst(_) -> 1
    | CntSymCst ( _ ) -> 1
    | CntVar (_) -> 1
    | CntInvalidExp -> 1
    | CntUnMin ( exp' ) -> 1 +    size_arithm_exp exp'
    | CntMinus ( eg ,  ed ) ->
      1 + max (size_arithm_exp eg ) (size_arithm_exp eg ) 
    | CntMinus ( eg ,  ed ) -> 
      1 + max (size_arithm_exp eg ) (size_arithm_exp eg )  
    | CntSum ( eg ,  ed ) ->
      1 + max (size_arithm_exp eg ) (size_arithm_exp eg )
    | CntProd ( eg ,  ed ) -> 
      1 + max (size_arithm_exp eg ) (size_arithm_exp eg )    
    | CntMod ( eg ,  ed ) -> 
      1 + max (size_arithm_exp eg ) (size_arithm_exp eg )       
    | CntDiv ( eg ,  ed ) -> 
      1 + max (size_arithm_exp eg ) (size_arithm_exp eg )       

(* This function answers true if there exists a subtree of exp which size
is greater or equal that deepness. We use this function to decide wheter
some expression shall be parenthesed or not. *)
let rec size_arithmexp_deeper_than  (exp : cnt_arithm_exp ) (deepness : int ) =
  if deepness <= 0 then true
  else 
    let deepness' = deepness - 1 in
    match exp with 
       CntCst(_) 
    | CntSymCst (_ )
    | CntVar (_) 
    | CntInvalidExp -> false
    | CntUnMin ( exp' ) ->   size_arithmexp_deeper_than exp' deepness'
    | CntMinus ( eg ,  ed ) ->
      (size_arithmexp_deeper_than eg deepness' ) || (size_arithmexp_deeper_than ed deepness' )
    | CntMinus ( eg ,  ed ) -> 
      (size_arithmexp_deeper_than eg deepness' ) || (size_arithmexp_deeper_than ed deepness' )
    | CntSum ( eg ,  ed ) ->
     (size_arithmexp_deeper_than eg deepness' ) || (size_arithmexp_deeper_than ed deepness' )
    | CntProd ( eg ,  ed ) -> 
      (size_arithmexp_deeper_than eg deepness' ) || (size_arithmexp_deeper_than ed deepness' )
    | CntMod ( eg ,  ed ) -> 
      (size_arithmexp_deeper_than eg deepness' ) || (size_arithmexp_deeper_than ed deepness' ) 
    | CntDiv ( eg ,  ed ) -> 
      (size_arithmexp_deeper_than eg deepness' ) || (size_arithmexp_deeper_than ed deepness' )

(*let rec cnt_arithm_exp ( exp : cnt_arithm_exp ) =*)
  
  
  
