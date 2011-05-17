open List
open SSL_lex
open Hashtbl 

module SSL = struct
 

 (* module SSL_lex = SSL_types_gen (
    struct
      let order_relation = (<)
    end )
 *)

    
    
  let orient_eq ( equation : SSL_lex.eq ) =
    match equation with
	Eqloc ( LVar (x) , LVar( y ) ) -> if ( SSL_lex.order_relation x y ) == true
	then
	  equation
	else  
	  Eqloc ( LVar( y ) , LVar ( x ))
	    
  (* This function orients all equation w.r.t. the oredering relation
     on the naming of the variables. *)

  let orient_eqlist  ( eqlist : SSL_lex.eq list ) =
    List.map ( fun s -> ( orient_eq s ) ) eqlist 
      

  let fold_max (lvari : SSL_lex.locvar)()(lvinit : SSL_lex.locvar ) =
    match (lvari , lvinit ) with 
	(LVar (x) , LVar (y)) -> if (SSL_lex.order_relation x y ) == true
	then LVar (x)
	else LVar (y)


  (* Returns the biggest key of hash table which keys are varlocs*)
  (* Any variable name in C contains at least a character, therefore
     is bigger than "".
  *)

  let biggest_loc_var ( tble : (( SSL_lex.locvar , unit ) t) ) =
    Hashtbl.fold fold_max tble ( LVar("") )
 
 (*
  let unif_loc_vars_iterande ( x : ptvar  ) =
    
  let unif_loc_vars ( puref : SSL_lex.pure_formula ) =
 *) 

	    
end;;
