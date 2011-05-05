(**  In this file, we define the function that allows to check whether
an expression of the intermediate language is expressible using flata's
grammar.

The intermediate language allows to express things like "x'=x*y", for instance,
which is of course not possible to express using the grammar of FLATA.

An arithmetic operation is expressible using a FLATA term if it contains
no monomial whose degree is greater than one. Here, by polynomial variables,
we mean counters.




*)

exception Not_a_monome 
exception Unsound_product 
exception Compare_integer_with_pointer


open Intermediate_language
open Cautomata
open String
open List


(*Var name and exponent*)
(* XPow of c_int_var * 0 means a constant *)

type var_pol = VarI of c_int_var
	       | VarIPtr of c_int_ptr

type var_pow = XPow of var_pol * c_int_cst
	      (* | XConst of int *) 

type inmon = InMon of int * var_pow list

(* Here we assume that the list are sorted w.r.t. the lexicographical 
order on the name of the variables.*)


let rec prod_var_list  (res : var_pow list ) ( l1: var_pow list)
 ( l2: var_pow list) =
  match l1 , l2 with
    | ( [] , _ :: _ ) -> let res = (List.rev res) in  res@l2 

    | ( _::_ , [] ) -> let res = (List.rev res ) in res@l1 
						 
    | ([],[]) -> []

    | (  (XPow(VarI(LiIntVar(vnameg)) ,  LiIConst(cg))::l1') , 
	 ( XPow(VarI(LiIntVar(vnamed)), LiIConst(cd))::l2') ) ->
	  let cmpr = ( String.compare vnameg vnamed ) in
	    if (cmpr = 0 )
	    then  
	      let res = (XPow(VarI(LiIntVar(vnamed)), 
			      LiIConst(cg * cd ) )::res) 
	      in
	      ( prod_var_list res l1' l2' )
          
	    else if (cmpr = -1) then 
	      let res = (XPow(VarI(LiIntVar(vnameg)),LiIConst(cg))::res) in
	      ( prod_var_list res l1' l2 )
	     
	    else if (cmpr = 1) then
	      let res = (XPow(VarI(LiIntVar(vnamed)),LiIConst(cd))::res)
	      in
	      ( prod_var_list res l1 l2' ) 
	    
	    else [] (* This case should never happen*)
	  
    | ( (XPow(VarIPtr(LiIntPtr(vnameg)) ,  LiIConst(cg))::l1') , 
	( XPow(VarI(LiIntVar(vnamed)), LiIConst(cd))::l2') ) ->
      
       let cmpr = ( String.compare vnameg vnamed ) in
	    if (cmpr = 0 )
	    then  
	      let res = (XPow(VarIPtr(LiIntPtr(vnamed)),
			      LiIConst(cg * cd ) )::res) in
	      ( prod_var_list res l1' l2' )
          
	    else if (cmpr = -1) then 
	      let res = (XPow(VarIPtr(LiIntPtr(vnameg)) , 
			      LiIConst(cg))::res) 
	      in
	      ( prod_var_list res l1' l2 )
	     
	    else if (cmpr = 1) then
	      let res = (XPow(VarIPtr(LiIntPtr(vnamed)) ,
			      LiIConst(cd))::res) 
	      in
	      ( prod_var_list res l1 l2' ) 
	    else [] (* This case should never happen*)
   (* This case corresonds to some invalid operations, for instance the product of an int value with a pointer. The product of two pointers never
appears at this stage of the analysis, as gcc generates an error. *)
	      
    | (_,_) -> raise Unsound_product
  


let prod_inmon (i1 : inmon )( i2 :  inmon ) =
  match i1 , i2 with
      (InMon(cg,lmong ),InMon(cd,lmond )) ->
	let coef=cg*cd in
	InMon(coef, ( prod_var_list [] lmong lmond ) )

(*Takes an intermediate language scalar expression and expends it. *)

(** Returns  0 if M1=alpha\times M2, -1 if M1 >_{lex} M2 and 1 if M1<_{lex} M2 *)
let rec lex_mon (l1 : var_pow list ) (l2 : var_pow list ) =
  let type_match vnameg vnamed l1' l2' pg pd =
    let cmp = String.compare vnameg vnamed  in
	if ( cmp <> 0 ) then cmp
	else
	  begin
	    if pg > pd then -1
	    else if pg < pd then 1
	    else
	      lex_mon l1' l2'
	  end
  in

  match l1 , l2 with 
      ( XPow(VarI(LiIntVar(vnameg)),LiIConst(pg))::l1' ,
	XPow(VarI(LiIntVar(vnamed)),LiIConst(pd))::l2') ->
	type_match vnameg vnamed l1' l2' pg pd
   
    |  ( XPow(VarIPtr(LiIntPtr(vnameg)),LiIConst(pg))::l1' , 
	 XPow(VarIPtr(LiIntPtr(vnamed)),LiIConst(pd))::l2') ->
      type_match vnameg vnamed l1' l2' pg pd

    | ( XPow(_,_)::_ , XPow(_,_)::_ ) ->
      raise Compare_integer_with_pointer
      
    | ( [] , _ :: _ ) -> -1
    | ( _::_ , []) -> 1
    | ([],[]) ->  0 (*Equality case*)


let cmp_inmon (i1 : inmon) (i2 : inmon) =
  match i1 , i2 with
      (InMon(_,lmong), InMon(_,lmond)) -> 
	lex_mon lmong lmond


let rec expend_expr ( scal_exp : Intermediate_language.c_scal ) =
  match scal_exp with
      LiSum( a , b ) -> LiSum ( expend_expr a , expend_expr b )
    | LiMinus( a , b ) -> LiMinus ( expend_expr a , expend_expr b )
    | LiUnMin ( a ) -> LiUnMin ( expend_expr a ) 
    | LiProd ( LiSum(a,b) , c ) -> LiSum( expend_expr(LiProd(a,c)) , 
					  expend_expr(LiProd(b,c)))
    | LiProd ( LiMinus(a,b) , c) -> LiMinus( expend_expr(LiProd(a,c)), 
					     expend_expr(LiProd(b,c)) )
    | LiProd ( a , LiSum( b , c)) ->  LiSum( expend_expr(LiProd(a,b)), 
					     expend_expr(LiProd(a,c)))
    | LiProd ( a , LiMinus( b , c)) -> LiMinus( expend_expr(LiProd(a,b)),
						expend_expr(LiProd(a,c)))
    | LiMod(a , b) -> LiMod(expend_expr a , expend_expr b)
    | _ -> scal_exp
  


(* traverses a scal tree and *)
(*returns the list of monomials contained in the *)


   
(** After normalisation, all subtree whose root are a node \times
are monomials. *)

(*Requires that the scalar expression has been expended before the call.*)
let rec mon_of_scal_prod_tree ( scal_expr : Intermediate_language.c_scal)
 = 
  match scal_expr with
    | LiPtr ( LiIntPtr ( ptr_name) ) -> 
      InMon ( 1 ,  XPow (VarIPtr(LiIntPtr(ptr_name)), LiIConst (1))::[])
    | LiVar ( _ , var ) -> InMon (  1  , XPow( VarI(var) , LiIConst ( 1 ) )::[])
    | LiConst ( LiIConst( i ) ) -> InMon ( i  , XPow ( VarI (LiIntVar("__Constante__C2CA")), LiIConst(0) )::[])
    | LiProd ( a , b ) -> 
      let tree_a =  mon_of_scal_prod_tree a in
      let tree_b = mon_of_scal_prod_tree b in
      prod_inmon tree_a tree_b
    | _ -> raise Not_a_monome
	


let rec monom_list_of_scal ( res_list : inmon list )( scal_expr : 
Intermediate_language.c_scal ) =
  match scal_expr with 
    | LiProd ( _ , _ ) ->  ( mon_of_scal_prod_tree scal_expr )::res_list
    | LiVar ( _ , var ) -> InMon (  1 ,
				   XPow( var , LiIConst ( 1 ) )::[])::res_list
    | LiConst( LiIConst ( i ) ) -> InMon ( LiIConst ( i ) ,
				XPow ( VarI( LiIntVar ("__Constante__C2CA")),
						      LiIConst(0) )::[])::res_list

    | LiSum( a , b ) ->  ( monom_list_of_scal a )@( monom_list_of_scal b )
    | LiMinus ( a , b ) -> ( monom_list_of_scal a )@( monom_list_of_scal b )
    | LiMod ( a , b ) -> ( monom_list_of_scal a )@( monom_list_of_scal b )
    | LiUnMin ( a ) ->   monom_list_of_scal a
    | _ ->  raise  
      

  (*

let  monomials_of_scal  ( scal_expr : Intermediate_language.c_scal ) = 
  let scal_nf = expend_expr scal_expr in
  
  *)
  
  
  





