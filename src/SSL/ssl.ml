(** This files contains the operation use to create, normalize, and check 
properties of SSL formulae

 For questions, comment or any improvement proposal, please contact
 florent<dot>garnier\\at//imag/dot\fr or gaudin.maxime\\at//gmail-DoT-fr.

*)

open List
open Ssl_types
open Ssl_types.SSL_lex
open Hashtbl 



  let create () = 
    {equations = [] ; affectations = Hashtbl.create (SSL_lex.size_hash) ; ptnil = Hashtbl.create (SSL_lex.size_hash)  }
    
 
 let orient_eq  equ =
    match equ with
	SSL_lex.Eqloc ( SSL_lex.LVar (x) , SSL_lex.LVar( y ) ) -> if (( SSL_lex.order_relation x y ) == true )
	then
	  equ
	else  
	  SSL_lex.Eqloc ( SSL_lex.LVar( y ) , SSL_lex.LVar ( x ))
	    
  (* This function orients all equation w.r.t. the oredering relation
     on the naming of the variables. 
     Shall appear in the Interface file.
  *)

  let orient_eqlist  eqlist   =
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
 

  let extract_eq_from_hashtbl ( l : SSL_lex.eq list Pervasives.ref ) 
      ( maxi : SSL_lex.locvar ) (iterande : SSL_lex.locvar)() =
    
    match maxi, iterande with
	(LVar(iterande_name),LVar(maxi_name)) ->
	  if (SSL_lex.equals_to iterande_name maxi_name ) == true 
	  then ()
	  else 
	    l := (Eqloc(maxi,iterande):: !l )

   (** keep the biggest locvar in the hashtable, remove all the other
   elements and returns the set of equations, following the 
   rule  Unif Loc Var*)
  
  let unify_eq ( tble : ((locvar , unit )t )) =
    if ( Hashtbl.length tble ) == 0
    then []
    else
      let eq_list_res = ref [] in
      let repres = biggest_loc_var tble in
      Hashtbl.iter  ( extract_eq_from_hashtbl eq_list_res repres )  tble ; 
      Hashtbl.clear tble;Hashtbl.add tble repres (); 
      !eq_list_res (* return the value contained in the refered
		      list of equations *)
      
 

 (* Called by del_tautologies. Shall not appear in the Interface. *)

  let rec _del_tautologies ( lg : eq list )( ld : eq list) =
    match lg , ld  with 
	(x, []) -> x
      | (x, Eqloc(LVar(g),LVar(d))::ld' ) -> 
	  if equals_to g d
	  then _del_tautologies x ld'
	  else _del_tautologies ( Eqloc(LVar(g),LVar(d))::x) ld'


 (** This fuction is used to remove trivial equalities, such as l1=l1*)
	 

  let del_tautologies (l : eq list ) =
      _del_tautologies [] l


(**********************************************************************)
(*   The part of the file that follows contains the functions that
     encodes the substitutions of the formulae.
*)


(**********************************************************************)

 (* Not for the interface. Called by subst_eqlist*)
 
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
	          
 (** Use this to replace all instance of xv by yv in list lst. 
Shall appear in the interface file. *)

  let subst_eqlist (xv : locvar) (yv :locvar ) (lst : eq list ) =
    List.map (subst_loc xv yv ) lst



(* Shall not appear in the interface file *)
  let subst_loc_affect_ite (table : (( locvar , unit )t) ) (xv : locvar) (yv : locvar) (iterande : locvar)() = 
    match iterande, xv with 
	(LVar ( iname ) , LVar ( xvname )) -> if ( SSL_lex.equals_to iname xvname ) 
	  then 
		  begin
		    Hashtbl.remove table iterande; 
		    Hashtbl.add table  yv  () 
		  end

 (*Shall not appear in the interface file *)
  let  inter_tabl_ptvar_loc (xv:locvar)(yv:locvar)(pointer : ptvar)( iterand_table :  ((locvar , unit) t ))=
    Hashtbl.iter (subst_loc_affect_ite iterand_table xv yv ) iterand_table

  

(**  Performs the substitution of all location variables which name equals to xv by renaming
them yv. This is done by iterating on tabl and by iterating on each subtables .*)
(* To be added in the interface file .*)

  let subst_lvar_affect (xv : locvar) (yv :locvar) ( tabl : ( ( ptvar, (locvar, unit )t ) t) ) = 
    Hashtbl.iter ( inter_tabl_ptvar_loc xv yv  ) tabl

  (**

     Pretty print related stuffs


  *)


  let print_eq_iterator ( out : Format.formatter ) (last_elem : bool ) ( equ : SSL_lex.eq ) =
    match equ with
	Eqloc(LVar(x) , LVar(y) )-> 
	  if last_elem then Format.fprintf out "(%s==%s) ]" x y
	  else  Format.fprintf out "(%s==%s) ; " x y

  let print_eqlist (out :Format.formatter ) ( equ : SSL_lex.eq list ) =
    let taille = List.length equ in
    let cmp= ref 0 in
    Format.fprintf out "[";
    List.iter (fun s -> print_eq_iterator out (taille == !cmp ) s;
      cmp:=!cmp+1 )
      

 
  let print_pointstonil  (out :Format.formatter ) ( aff :  (ptvar , unit) t  ) =
    let ptnil_iterator out_channel s () = 
      match s with 
	  PVar( sname ) ->
	  Format.fprintf out " %s -> NIL and " sname
    in
    Hashtbl.iter ( ptnil_iterator out ) aff

      

  let print_affect_iter2  (out :Format.formatter) ( p : ptvar ) ( loc : locvar)() =
    match p , loc with
	(PVar(pt), LVar(l))->
	  Format.fprintf out "%s -> %s and " pt l

  let print_affect_iter1 (out :Format.formatter ) ( p : ptvar) ( aff :  (locvar , unit) t  ) =
    Hashtbl.iter (print_affect_iter2 out p ) aff
    
    
  let print_affect  (out :Format.formatter ) ( aff : (ptvar , (locvar , unit) t ) t ) =
     Hashtbl.iter (print_affect_iter1 out ) aff
    

(** Substitutes x by y  in pure formula f *)
 
(* let subst_in_pure_f ( x : SSL_lex.locvar ) ( y : SSL_lex.locvar ) ( f : SSL_types.pure_formula ) =
    
*)
	    

