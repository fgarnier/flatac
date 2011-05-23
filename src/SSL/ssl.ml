(** This files contains the operation use to create, normalize, and check 
properties of SSL formulae

 For questions, comment or any improvement proposal, please contact
 florent<dot>garnier\\at//imag/dot\fr or gaudin.maxime\\at//gmail-DoT-fr.

*)

open List
open Ssl_types
open Ssl_types.SSL_lex
open Hashtbl 
open Format


  let create_pure_f () = 
    {equations = [] ; affectations = Hashtbl.create (SSL_lex.size_hash) ; ptnil = Hashtbl.create (SSL_lex.size_hash)  }
    

  let create_space_f () =
    let table = Hashtbl.create Ssl_types.SSL_lex.size_hash in
    Space ( table )

 (** Creates an empty SSL formula *)
  let create_ssl_f () =
    {
      quant_vars = Hashtbl.create Ssl_types.SSL_lex.size_hash;
      pure = create_pure_f ();
      space = create_space_f ()
    }
 
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




 let print_qvars_iterator ( out : Format.formatter ) (last_elem : bool ) ( qvar : SSL_lex.locvar )() =
    match qvar with
	LVar(x) -> 
	  if last_elem then Format.fprintf out " %s ]" x 
	  else  Format.fprintf out "%s ; " x 


  let print_exist_vars (out : Format.formatter ) ( qvars : (locvar , unit ) t) =
    let taille = Hashtbl.length qvars in
    let cmp= ref 0 in
    Format.fprintf out " Exist [";
    Hashtbl.iter (fun s ()-> print_qvars_iterator out (taille == !cmp ) s ();
      cmp:=!cmp+1 ) qvars;
      Format.fprintf out "]"

  let print_eq_iterator ( out : Format.formatter ) (last_elem : bool ) ( equ : SSL_lex.eq ) =
    match equ with
	Eqloc(LVar(x) , LVar(y) )-> 
	  if last_elem then Format.fprintf out "(%s==%s) ]" x y
	  else  Format.fprintf out "(%s==%s) and ; " x y

  let print_eqlist (out :Format.formatter ) ( equ : SSL_lex.eq list ) =
    let taille = List.length equ in
    let cmp= ref 0 in
    List.iter (fun s -> print_eq_iterator out (taille == !cmp ) s;
      cmp:=!cmp+1 ) equ
      

 
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
    


  let print_space_iter (out : Format.formatter )(is_last : bool )( lvar : locvar  )( occurence : int) =
    match lvar with 
	LVar ( x ) ->
	  if (is_last == true )
	  then  Format.fprintf out " ( Alloc( %s ) , %d )" x occurence
	  else Format.fprintf out " ( Alloc ( %s) , %d ) *"  x occurence 

 
  let  print_space ( out: Format.formatter ) ( space : (space_formula)) =
    match space with 
	 Space( table ) -> 
	   if (Hashtbl.length table == 0 ) then
	     fprintf out "Emp" 
	   else
	     let taille  = Hashtbl.length table in
	     let cmt = ref 1 in
	     Hashtbl.iter ( fun lv occ -> print_space_iter out (taille == !cmt) lv occ;  cmt := !cmt + 1 ) table
      
      | Top_heap  -> fprintf out "TOP"
    



  let print_pure_formula (out: Format.formatter) (puref : Ssl_types.SSL_lex.pure_formula) =
    fprintf out " Equations : [";  print_eqlist out (puref.equations); fprintf out "] and "; 
    fprintf out "Affectations : ["; print_affect out puref.affectations ;
    fprintf out "] and ";
    fprintf out "Set to nil : [";
    print_pointstonil out puref.ptnil; fprintf  out "]"


  let pprint_ssl_formula (out: Format.formatter)(sslf :  ssl_formula) =
    if ( ((Hashtbl.length sslf.pure.affectations)
	   + (Hashtbl.length sslf.pure.ptnil )
	     +(List.length sslf.pure.equations ))>0
    )
    then 
      begin
    fprintf out "{";print_exist_vars out sslf.quant_vars;fprintf out "}";
    fprintf out "PURE{"; print_pure_formula out sslf.pure ; 
    fprintf out "}";
      end
    else
      fprintf out "Pure{true}";
    
    fprintf out " || ";
    fprintf out "SPACE {"; print_space out sslf.space;
    fprintf out "}"
     
   
      
    
(* ********************************************************************* **)

(* The part that follows, contains the basic operations on SSL formulae,
namely ;
_ And of an  atomic propositions and a SSL formula
_ And of two pure formulae
_ Computing the separation of two ssl formulae
 *)

(**************************************************************************)



  let  and_atomic_eq (equ : SSL_lex.eq )( sslf : SSL_lex.ssl_formula) =
    sslf.pure.equations <- (equ::sslf.pure.equations) (*It's damnes convenient,
						      isn't it ?*)

  let and_atomic_affect (equ : SSL_lex.affect)(sslf : SSL_lex.ssl_formula) =
    match equ with 
	Pointsto ( ptr, lv ) ->
	  if ( (Hashtbl.mem sslf.pure.affectations ptr) == true )
	  then
	    let tble = Hashtbl.find sslf.pure.affectations ptr in
	    Hashtbl.add tble lv ()
	  else
	    let tble = Hashtbl.create size_hash in
	    Hashtbl.add tble lv (); (* One now add the binding
				       lv->unit in the new table*)
	    Hashtbl.add sslf.pure.affectations ptr tble (*And add this
							 new this table associated
to the key ptr*)
       
  let and_atomic_ptnil (ptnil : SSL_lex.affectnil )( sslf :SSL_lex.ssl_formula )=
    match ptnil with 
	Pointsnil ( ptr ) ->
	  if ( (Hashtbl.mem sslf.pure.affectations ptr) == false ) 
	  then Hashtbl.add sslf.pure.ptnil ptr ()
	    (*One adds x->nil iff it is not yet present*)
	  
