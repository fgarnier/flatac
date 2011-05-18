open List
open Ssl_types
open Ssl_types.SSL_lex
open Hashtbl 
module SSL = struct
       
  let orient_eq  equ   =
    match equ with
	SSL_lex.Eqloc ( SSL_lex.LVar (x) , SSL_lex.LVar( y ) ) -> if (( SSL_lex.order_relation x y ) == true )
	then
	  equ
	else  
	  SSL_lex.Eqloc ( SSL_lex.LVar( y ) , SSL_lex.LVar ( x ))
	    
  (* This function orients all equation w.r.t. the oredering relation
     on the naming of the variables. *)

  let orient_eqlist   eqlist   =
    List.map ( fun s -> ( orient_eq s ) ) eqlist 

   

  let fold_max (lvari : SSL_lex.locvar)()(lvinit : SSL_lex.locvar ) =
    match (lvari , lvinit ) with 
	(LVar (x) , LVar (y)) -> if (order_relation x y ) == true
	then LVar (x)
	else LVar (y)


  (* Returns the biggest key of hash table which keys are varlocs*)
  (* Any variable name in C contains at least a character, therefore
     is bigger than "".
  *)

  let biggest_loc_var ( tble : (( locvar , unit ) t) ) =
    Hashtbl.fold fold_max tble ( LVar("") )
 
 (*
  let unif_loc_vars_iterande ( x : ptvar  ) =
    unif_loc_vars ( puref : SSL_lex.pure_formula ) =
 *) 



  let rec _del_tautologies ( lg : eq list )( ld : eq list) =
    match lg , ld  with 
	(x, []) -> x
      | (x, Eqloc(LVar(g),LVar(d))::ld' ) -> 
	  if equals_to g d
	  then _del_tautologies x ld'
	  else _del_tautologies ( Eqloc(LVar(g),LVar(d))::x) ld'

	 

  let del_tautologies (l : eq list ) =
      _del_tautologies [] l





  let subst_loc (xv : locvar)(yv : locvar)( equality : eq) =
   match xv , yv with
    (LVar(x),LVar(y)) ->
      match equality with
	  Eqloc ( LVar (a) , LVar (b) ) ->
	    if ( ( SSL_lex.equals_to x a ) && (SSL_lex.equals_to x b))
	    then Eqloc ( LVar (y) , LVar (y))
	    else 
	      if ( SSL_lex.equals_to x a ) then Eqloc ( LVar(y), LVar(b))
	    else
	      if ( SSL_lex.equals_to x b ) then Eqloc (LVar(a),LVar (y))
	   else Eqloc ( LVar (a) , LVar (b) )
	      
    

  let subst_eqlist (xv : locvar) (yv :locvar )  (lst : eq list ) =
    List.map (subst_loc xv yv ) lst


      
end;;
(** Substitutes x by y  in pure formula f *)
 
(* let subst_in_pure_f ( x : SSL_lex.locvar ) ( y : SSL_lex.locvar ) ( f : SSL_types.pure_formula ) =
    
*)
	    
