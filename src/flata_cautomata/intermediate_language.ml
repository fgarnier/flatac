(* In this files, we define the syntax, the types of objects used to
analyse C code, and whose syntax doesn't belong to FLATA syntax and
grammar.

Laboratoire Verimag, 2 Avenue de Vignate, 38610 GiÃ¨res, France.

For questions or remarks, contact florent.garnier__at__imag__dot__fr

*)


open Cil_types
open Int64


(* 
Boolean doesn't have a peculiar type in ANSI C. 
Any non-zero scalar expression (char, Int, float, double, signed or unsigned)
is evaluated to true. Zero values are interpreted as false.

(** Scalars "type" contains as well pointers values. NULL is evaluated
to false and any other non-zero address is evaluated as true. *)
*)
type primed = Primed
	      | Unprimed
type c_int_var = LiIntVar of string
type c_int_cst = LiIConst of int
type c_int_sym_const = LiSymIConst of string
type c_ptr = LiIntPtr of string (*The represented type is indeed an int*)


(* We define the type of  the scalars as follows.
basicaly, the scalar we consider in this language are all the scalars
that are the evaluation of arithmetic
expressions which ground terms are free integer variables
and integers constants.

*)

exception Bad_expression_type of string

(** The type of integers scalar expressions*)
type c_scal = LiVar of primed * c_int_var
	      | LiConst of c_int_cst
	      | LiSymConst of c_int_sym_const  (*Like sizeof(char)*)
	      | LiProd of c_scal * c_scal
	      | LiSum of c_scal * c_scal
	      | LiMinus of c_scal * c_scal
	      | LiUnMin of c_scal
	      | LiMod of c_scal * c_scal   (*Modulo operator*)
	      | LiMinusPP of c_ptrexp * c_ptrexp
		  
and c_ptrexp = LiPVar of primed * c_ptr (* Type of pointer variables *)
	       | LiPlusPI of c_ptrexp * c_scal
	       | LiIndexPI of c_ptrexp * c_scal 
	       | LiMinusPI of c_ptrexp * c_scal
	       


type c_bool = LiBNot of c_bool 
 	      | LiBAnd of c_bool * c_bool 
	      | LiBOr of c_bool * c_bool  
	      | LiBTrue
	      | LiBFalse
	      | LiBEq of c_scal * c_scal
	      | LiBNeq of c_scal * c_scal
	      | LiBLt of c_scal * c_scal
	      | LiBGt of c_scal * c_scal
	      | LiBLeq of c_scal * c_scal
	      | LiBGeq of c_scal * c_scal
	      | LiBScal of c_scal (* true iff != 0 *)
	      | LiBPtrEq of c_ptrexp *  c_ptrexp
	      | LiBPtrNeq of c_ptrexp * c_ptrexp
	      | LiBPtrGt of c_ptrexp * c_ptrexp
	      | LiBPtrLt of c_ptrexp * c_ptrexp
	      | LiBPtrGeq of c_ptrexp * c_ptrexp
	      | LiBPtrLeq of c_ptrexp * c_ptrexp



type valid_counter = PtValid of string
		     | IntValid of string
		     | AndValid of valid_counter * valid_counter
		     | TrueValid
		     | FalseValid


 
let rec cil_expr_2_scalar (expr : Cil_types.exp ) =
  match expr.enode with 
      Const(CInt64(i,_,_))-> LiConst( LiIConst(Int64.to_int i))
    | Lval(Var(f),_)->
      begin
	match f.vtype with
	    TInt(_,_) -> LiVar(Unprimed,LiIntVar(f.vname))
	  | _-> begin 
	    let msg = "This variable : "^f.vname ^"isn't of type TInt, but appears in a scalar expression \n" in let exc =  Bad_expression_type msg in
														 raise  exc
	  end
      end	
	
    | BinOp (PlusA, expg, expd, TInt(_,_) ) ->
      LiSum (cil_expr_2_scalar expg , cil_expr_2_scalar expd)
	
    | BinOp (MinusA, expg, expd, TInt(_,_) ) ->
      LiMinus (cil_expr_2_scalar expg ,cil_expr_2_scalar  expd)
	
    | BinOp (Mult, expg, expd, TInt(_,_) ) -> 
      LiProd (cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
	  
    | BinOp (Mod, expg, expd, TInt(_,_)) -> 
      LiMod(cil_expr_2_scalar expg ,cil_expr_2_scalar expd )

    | BinOp (MinusPP , expg , expd , _ ) ->
      LiMinusPP(cil_expr_2_ptr expg , cil_expr_2_ptr expd)
    
    | UnOp (Neg, exp , TInt(_,_)) ->
      LiUnMin ( cil_expr_2_scalar exp)
       
    | _ -> raise( Bad_expression_type "'Can't parse expression in cil_expr_2_scalar \n")

and cil_expr_2_ptr expr =
  match expr.enode with
    
      BinOp (PlusPI, expg, expd , _ ) ->
	LiPlusPI(cil_expr_2_ptr expg, cil_expr_2_scalar expd )
    
    | BinOp (IndexPI , expg , expd , _ ) ->
      LiIndexPI ( cil_expr_2_ptr expg, cil_expr_2_scalar expd)
    
    | BinOp ( MinusPI , expg , expd , _ ) ->
      LiMinusPI ( cil_expr_2_ptr expg , cil_expr_2_scalar expd )
	
    |  Lval (Var(vinfo), _ ) ->
      begin
	match vinfo.vtype with
	    TPtr( TInt(_,_), _ ) -> LiPVar(Unprimed, LiIntPtr(vinfo.vname))
	  | _ ->  begin 
	    let msg = "This variable : "^vinfo.vname ^"is a pointer which isn't of  type TInt, but that appears in a scalar expression \n" 
	    in let exc =  Bad_expression_type msg in 
	       raise exc
	  end
      end

    |  (_) ->    begin 
	    let msg = " There is something I was unable to properly
parse in the ci_expr_2_ptr function" 
	    in let exc =  Bad_expression_type msg in 
	       raise exc
	  end
      


let rec cil_expr_2_bool (expr : Cil_types.exp) =
  match expr.enode with 
       BinOp(LAnd,expg ,expd , _) ->
	 LiBAnd( cil_expr_2_bool expg, cil_expr_2_bool expd)
       
    |  BinOp(LOr, expg, expd , _) ->
	 LiBOr( cil_expr_2_bool expg, cil_expr_2_bool expd )
    |  UnOp(LNot , exp , TInt(IBool,_) ) ->
	 LiBNot(cil_expr_2_bool exp)
	 
    | BinOp(Lt,expg,expd,_) -> 
	LiBLt(cil_expr_2_scalar expg ,cil_expr_2_scalar expd) 
	
    | BinOp(Gt,expg,expd,_) ->
	LiBGt(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp(Le,expg,expd,_) ->
	LiBLeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp(Ge,expg,expd,_) ->
	LiBGeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp(Ne,expg,expd,_) ->
	LiBNeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp (Eq , expg , expd , TPtr (TInt(_,_) , _)) ->
      LiBPtrEq ( cil_expr_2_ptr expg ,  cil_expr_2_ptr expd )
	
    | BinOp(Eq,expg,expd,_) ->
      LiBEq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
	
    | Const(CInt64(value,_,_)) ->  
      LiBScal(LiConst( LiIConst (Int64.to_int value)))
    
    | _-> raise ( Bad_expression_type "Trying to parse an expression \
 that can't be evaluated as a boolean \n")





(********************************************************************)

(** This function transforms a list of cil expression into a list
of scalar expression, whenever possible. It raises a Bad_expression_type
exception if something wrong occured.  *)


     (** Replace that by a List.map*)
(*******************************************************************)
let cil_expr_list_2_scalar_list (expr_list : Cil_types.exp list ) =
  let rec rec_call (ret_list : c_scal list) (expr_list: Cil_types.exp list )= 
    match expr_list with
	[] -> ret_list
      | l::l' -> rec_call ( (cil_expr_2_scalar l)::ret_list ) l'
  in
  List.rev (rec_call [] expr_list) (* The args have been added in head, therefore the list need to be reversed to respect argument order.
 List reversal O(n) whereas adding elements on list tail costs 0(n²).
*)



let rec cil_expr_2_scalar (expr : Cil_types.exp ) =
  match expr.enode with 
      Const(CInt64(i,_,_))-> LiConst( LiIConst(Int64.to_int i))
    | Lval(Var(f),_)->
      begin
	match f.vtype with
	    TInt(_,_) -> LiVar(Unprimed,LiIntVar(f.vname))
	  | _-> begin 
	    let msg = "This variable : "^f.vname ^"isn't of type TInt, but appears in a scalar expression \n" in let exc =  Bad_expression_type msg in
														 raise  exc
	  end
      end	
	
    | BinOp (PlusA, expg, expd, TInt(_,_) ) ->
      LiSum (cil_expr_2_scalar expg , cil_expr_2_scalar expd)
	
    | BinOp (MinusA, expg, expd, TInt(_,_) ) ->
      LiMinus (cil_expr_2_scalar expg ,cil_expr_2_scalar  expd)
	
    | BinOp (Mult, expg, expd, TInt(_,_) ) -> 
      LiProd (cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
	  
    | BinOp (Mod, expg, expd, TInt(_,_)) -> 
      LiMod(cil_expr_2_scalar expg ,cil_expr_2_scalar expd )

    | BinOp (MinusPP , expg , expd , _ ) ->
      LiMinusPP(cil_expr_2_ptr expg , cil_expr_2_ptr expd)
    
    | UnOp (Neg, exp , TInt(_,_)) ->
      LiUnMin ( cil_expr_2_scalar exp)
       
    | _ -> raise( Bad_expression_type "'Can't parse expression in cil_expr_2_scalar \n")

and cil_expr_2_ptr expr =
  match expr.enode with
    
      BinOp (PlusPI, expg, expd , _ ) ->
	LiPlusPI(cil_expr_2_ptr expg, cil_expr_2_scalar expd )
    
    | BinOp (IndexPI , expg , expd , _ ) ->
      LiIndexPI ( cil_expr_2_ptr expg, cil_expr_2_scalar expd)
    
    | BinOp ( MinusPI , expg , expd , _ ) ->
      LiMinusPI ( cil_expr_2_ptr expg , cil_expr_2_scalar expd )
	
    |  Lval (Var(vinfo), _ ) ->
      begin
	match vinfo.vtype with
	    TPtr( TInt(_,_), _ ) -> LiPVar(Unprimed, LiIntPtr(vinfo.vname))
	  | _ ->  begin 
	    let msg = "This variable : "^vinfo.vname ^"is a pointer which isn't of  type TInt, but that appears in a scalar expression \n" 
	    in let exc =  Bad_expression_type msg in 
	       raise exc
	  end
      end

    |  (_) ->    begin 
	    let msg = " There is something I was unable to properly
parse in the ci_expr_2_ptr function" 
	    in let exc =  Bad_expression_type msg in 
	       raise exc
	  end
      


let rec cil_expr_2_bool (expr : Cil_types.exp) =
  match expr.enode with 
       BinOp(LAnd,expg ,expd , _) ->
	 LiBAnd( cil_expr_2_bool expg, cil_expr_2_bool expd)
       
    |  BinOp(LOr, expg, expd , _) ->
	 LiBOr( cil_expr_2_bool expg, cil_expr_2_bool expd )
    |  UnOp(LNot , exp , TInt(IBool,_) ) ->
	 LiBNot(cil_expr_2_bool exp)
	 
    | BinOp(Lt,expg,expd,_) -> 
	LiBLt(cil_expr_2_scalar expg ,cil_expr_2_scalar expd) 
	
    | BinOp(Gt,expg,expd,_) ->
	LiBGt(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp(Le,expg,expd,_) ->
	LiBLeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp(Ge,expg,expd,_) ->
	LiBGeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp(Ne,expg,expd,_) ->
	LiBNeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp (Eq , expg , expd , TPtr (TInt(_,_) , _)) ->
      LiBPtrEq ( cil_expr_2_ptr expg ,  cil_expr_2_ptr expd )
	
    | BinOp(Eq,expg,expd,_) ->
      LiBEq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
	
    | Const(CInt64(value,_,_)) ->  
      LiBScal(LiConst( LiIConst (Int64.to_int value)))
    
    | _-> raise ( Bad_expression_type "Trying to parse an expression \
 that can't be evaluated as a boolean \n")





(********************************************************************)

(** This function transforms a list of cil expression into a list
of scalar expression, whenever possible. It raises a Bad_expression_type
exception if something wrong occured.  *)


     (** Replace that by a List.map*)
(*******************************************************************)
let cil_expr_list_2_scalar_list (expr_list : Cil_types.exp list ) =
  let rec rec_call (ret_list : c_scal list) (expr_list: Cil_types.exp list )= 
    match expr_list with
	[] -> ret_list
      | l::l' -> rec_call ( (cil_expr_2_scalar l)::ret_list ) l'
  in
  List.rev (rec_call [] expr_list) (* The args have been added in head, therefore the list need to be reversed to respect argument order.
 List reversal O(n) whereas adding elements on list tail costs 0(n²).
*)





(** One need to translate C-boolean evaluation into the language of FLATA
constrainsts. That's to say : 
Translating C-booleans expressions in the  "intermediate language " 
into the FLATA grammar, if there exists a matching transformation.
 Takes a c_bool expression as parameter then returns its negation.
The negation unary operators are pushed in the bottmost position
in the expression tree.
*)
let rec negate_bool_bot ( b_exp : c_bool ) =
  match b_exp with
      LiBNot ( exp ) -> exp
    |  LiBAnd( expg , expd ) -> 
      LiBOr ( negate_bool_bot( expg ), negate_bool_bot( expd ))
    |  LiBOr( expg , expd ) -> 
      LiBAnd ( negate_bool_bot( expg ), negate_bool_bot( expd ))
    | LiBTrue -> LiBFalse
    | LiBFalse -> LiBTrue
    | LiBEq( expg , expd ) ->  LiBNeq ( expg, expd )
    | LiBNeq( expg , expd ) ->  LiBEq ( expg, expd )
    | LiBLt( expg , expd ) ->  LiBGeq ( expg, expd )
    | LiBGt( expg , expd ) ->  LiBLeq ( expg, expd ) 
    | LiBLeq( expg , expd ) ->  LiBGt ( expg, expd ) 
    | LiBGeq( expg , expd ) ->  LiBLt ( expg, expd )
    | LiBScal( exp ) -> LiBEq ( exp, LiConst(LiIConst(0) ))
    | LiBPtrEq ( expg , expd ) -> LiBPtrNeq ( expg , expd )
    | LiBPtrNeq  ( expg , expd ) ->  LiBPtrEq ( expg , expd )
    | LiBPtrGt ( eg ,ed ) -> LiBPtrLeq (eg ,ed )
    | LiBPtrLt ( eg , ed ) -> LiBPtrGeq (eg ,ed )
    | LiBPtrGeq ( eg ,ed ) -> LiBPtrLt (eg ,ed)
    | LiBPtrLeq (eg ,ed ) -> LiBPtrGt (eg ,ed)



let rec cil_expr_2_scalar (expr : Cil_types.exp ) =
  match expr.enode with 
      Const(CInt64(i,_,_))-> LiConst( LiIConst(Int64.to_int i))
    | Lval(Var(f),_)->
      begin
	match f.vtype with
	    TInt(_,_) -> LiVar(Unprimed,LiIntVar(f.vname))
	  | _-> begin 
	    let msg = "This variable : "^f.vname ^"isn't of type TInt, but appears in a scalar expression \n" in let exc =  Bad_expression_type msg in
														 raise  exc
	  end
      end	
	
    | BinOp (PlusA, expg, expd, TInt(_,_) ) ->
      LiSum (cil_expr_2_scalar expg , cil_expr_2_scalar expd)
	
    | BinOp (MinusA, expg, expd, TInt(_,_) ) ->
      LiMinus (cil_expr_2_scalar expg ,cil_expr_2_scalar  expd)
	
    | BinOp (Mult, expg, expd, TInt(_,_) ) -> 
      LiProd (cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
	  
    | BinOp (Mod, expg, expd, TInt(_,_)) -> 
      LiMod(cil_expr_2_scalar expg ,cil_expr_2_scalar expd )

    | BinOp (MinusPP , expg , expd , _ ) ->
      LiMinusPP(cil_expr_2_ptr expg , cil_expr_2_ptr expd)
    
    | UnOp (Neg, exp , TInt(_,_)) ->
      LiUnMin ( cil_expr_2_scalar exp)
       
    | _ -> raise( Bad_expression_type "'Can't parse expression in cil_expr_2_scalar \n")

and cil_expr_2_ptr expr =
  match expr.enode with
    
      BinOp (PlusPI, expg, expd , _ ) ->
	LiPlusPI(cil_expr_2_ptr expg, cil_expr_2_scalar expd )
    
    | BinOp (IndexPI , expg , expd , _ ) ->
      LiIndexPI ( cil_expr_2_ptr expg, cil_expr_2_scalar expd)
    
    | BinOp ( MinusPI , expg , expd , _ ) ->
      LiMinusPI ( cil_expr_2_ptr expg , cil_expr_2_scalar expd )
	
    |  Lval (Var(vinfo), _ ) ->
      begin
	match vinfo.vtype with
	    TPtr( TInt(_,_), _ ) -> LiPVar(Unprimed, LiIntPtr(vinfo.vname))
	  | _ ->  begin 
	    let msg = "This variable : "^vinfo.vname ^"is a pointer which isn't of  type TInt, but that appears in a scalar expression \n" 
	    in let exc =  Bad_expression_type msg in 
	       raise exc
	  end
      end

    |  (_) ->    begin 
	    let msg = " There is something I was unable to properly
parse in the ci_expr_2_ptr function" 
	    in let exc =  Bad_expression_type msg in 
	       raise exc
	  end
      


let rec cil_expr_2_bool (expr : Cil_types.exp) =
  match expr.enode with 
       BinOp(LAnd,expg ,expd , _) ->
	 LiBAnd( cil_expr_2_bool expg, cil_expr_2_bool expd)
       
    |  BinOp(LOr, expg, expd , _) ->
	 LiBOr( cil_expr_2_bool expg, cil_expr_2_bool expd )
    |  UnOp(LNot , exp , TInt(IBool,_) ) ->
	 LiBNot(cil_expr_2_bool exp)
	 
    | BinOp(Lt,expg,expd,_) -> 
	LiBLt(cil_expr_2_scalar expg ,cil_expr_2_scalar expd) 
	
    | BinOp(Gt,expg,expd,_) ->
	LiBGt(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp(Le,expg,expd,_) ->
	LiBLeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp(Ge,expg,expd,_) ->
	LiBGeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp(Ne,expg,expd,_) ->
	LiBNeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)

    | BinOp (Eq , expg , expd , TPtr (TInt(_,_) , _)) ->
      LiBPtrEq ( cil_expr_2_ptr expg ,  cil_expr_2_ptr expd )
	
    | BinOp(Eq,expg,expd,_) ->
      LiBEq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
	
    | Const(CInt64(value,_,_)) ->  
      LiBScal(LiConst( LiIConst (Int64.to_int value)))
    
    | _-> raise ( Bad_expression_type "Trying to parse an expression \
 that can't be evaluated as a boolean \n")





(********************************************************************)

(** This function transforms a list of cil expression into a list
of scalar expression, whenever possible. It raises a Bad_expression_type
exception if something wrong occured.  *)


     (** Replace that by a List.map*)
(*******************************************************************)
let cil_expr_list_2_scalar_list (expr_list : Cil_types.exp list ) =
  let rec rec_call (ret_list : c_scal list) (expr_list: Cil_types.exp list )= 
    match expr_list with
	[] -> ret_list
      | l::l' -> rec_call ( (cil_expr_2_scalar l)::ret_list ) l'
  in
  List.rev (rec_call [] expr_list) (* The args have been added in head, therefore the list need to be reversed to respect argument order.
 List reversal O(n) whereas adding elements on list tail costs 0(n²).
*)





(** One need to translate C-boolean evaluation into the language of FLATA
constrainsts. That's to say : 
Translating C-booleans expressions in the  "intermediate language " 
into the FLATA grammar, if there exists a matching transformation.
 Takes a c_bool expression as parameter then returns its negation.
The negation unary operators are pushed in the bottmost position
in the expression tree.
*)
let rec negate_bool_bot ( b_exp : c_bool ) =
  match b_exp with
      LiBNot ( exp ) -> exp
    |  LiBAnd( expg , expd ) -> 
      LiBOr ( negate_bool_bot( expg ), negate_bool_bot( expd ))
    |  LiBOr( expg , expd ) -> 
      LiBAnd ( negate_bool_bot( expg ), negate_bool_bot( expd ))
    | LiBTrue -> LiBFalse
    | LiBFalse -> LiBTrue
    | LiBEq( expg , expd ) ->  LiBNeq ( expg, expd )
    | LiBNeq( expg , expd ) ->  LiBEq ( expg, expd )
    | LiBLt( expg , expd ) ->  LiBGeq ( expg, expd )
    | LiBGt( expg , expd ) ->  LiBLeq ( expg, expd ) 
    | LiBLeq( expg , expd ) ->  LiBGt ( expg, expd ) 
    | LiBGeq( expg , expd ) ->  LiBLt ( expg, expd )
    | LiBScal( exp ) -> LiBEq ( exp, LiConst(LiIConst(0) ))
    | LiBPtrEq ( expg , expd ) -> LiBPtrNeq ( expg , expd )
    | LiBPtrNeq  ( expg , expd ) ->  LiBPtrEq ( expg , expd )
    | LiBPtrGt ( eg , ed ) -> LiBPtrLeq ( eg , ed)
    | LiBPtrLt ( eg , ed ) ->  LiBPtrGeq ( eg , ed )
    | LiBPtrGeq ( eg , ed ) ->  LiBPtrLt ( eg , ed )
    | LiBPtrLeq ( eg , ed ) -> LiBPtrGt ( eg , ed )

(*Equals 0 means false in C*)


(* Negate a boolean expression by adding, resp removing, a negate fun symbol
on the root of the term, removing neg fun symbol if present.
 We use this function when it comes to let flata to deal with simplifying
logical expressions*)
let negate_bool_sym ( b_exp : c_bool ) =
   match b_exp with
       LiBNot ( exp ) -> exp
     | LiBEq( expg , expd ) ->  LiBNeq ( expg, expd )
     | LiBNeq( expg , expd ) ->  LiBEq ( expg, expd )
     | _ -> LiBNot ( b_exp )


(**Ajouter une fonction prenant en compte l'arité/distributivité des sous termes
pour effectuer (ou non) un parentésage.
It is essential, that the output syntax complies with the NTS-comp library.
*)


let rec scal_to_string ( b_exp : c_scal ) =
  match b_exp with 
      LiVar(Unprimed,LiIntVar(vname)) -> vname (* returns the name of the variable*)
    | LiVar(Primed,LiIntVar(vname)) -> vname^"'" 
    | LiConst(LiIConst(i)) -> (Printf.sprintf "%d" i )
    | LiSymConst(LiSymIConst(const_name)) -> const_name   
    | LiProd( sg , sd ) ->
      let rhs= ref "" in
      let lhs = ref "" in
      begin 
	match sg with 
	    LiSum (_,_) | LiMinus (_,_) -> rhs := "("^(scal_to_string  sg)^")"
	  | _ -> rhs := (scal_to_string  sg)
      end;
      begin
	 match sd with 
	    LiSum (_,_) | LiMinus (_,_) -> lhs := "("^(scal_to_string  sd)^")"
	  | _ -> lhs := (scal_to_string  sd)
      end;
      (!lhs)^"*"^(!rhs) (*Returned value*)
	(*End of the LiProd pretty print*)

    | LiSum ( sg , sd ) -> (scal_to_string sg) ^"+" ^ (scal_to_string sd)
    | LiMinus( sg , sd ) -> 
     begin
      match sd with
	  LiConst(_) | LiSymConst (_) | LiVar(_,_) ->
	    (scal_to_string sg) ^"-" ^ (scal_to_string sd)
	| _ ->(scal_to_string sg )^"-("^(scal_to_string sd)^")"
     end
     
    | LiUnMin ( s ) -> "-"^(scal_to_string s)
    
    | LiMod ( sg , sd ) ->  (scal_to_string sg)^"%"^(scal_to_string sd)
    
    | LiMinusPP (ptrg , ptrd ) ->
      "("^( ptrexp_to_str  ptrg )^"-"^ ( ptrexp_to_str  ptrd )^")"

and ptrexp_to_str ( cptr : c_ptrexp ) =
  match cptr with 
    
      LiPVar ( Primed , LiIntPtr ( vname) ) ->
	vname^"'"
    
    | LiPVar ( Unprimed , LiIntPtr ( vname )) ->
      vname
    
    | LiPlusPI ( ptr_in , offset ) ->
      ( ptrexp_to_str  ptr_in )^"["^(scal_to_string offset)^"]"
    
    | LiIndexPI ( ptr_in , offset ) ->
       ( ptrexp_to_str  ptr_in )^"["^(scal_to_string offset)^"]"
    
    | LiMinusPI (ptr_in , offset ) ->
      ( ptrexp_to_str  ptr_in )^"["^(scal_to_string offset)^"]"
    
   

  
(** One need to make sure that the output syntax complies with the NTS-lib
syntax.*) 
let rec c_bool_to_string ( b_exp :  c_bool) = 
  match b_exp with
        LiBNot ( b ) -> 
	  begin
	    match b with
		(* Here we manage not to add a Lot of Insane and Stupid Parentheses*)
		LiBAnd (_,_) | LiBOr (_,_) | LiBNeq (_,_) 
	      | LiBEq (_,_) | LiBLt (_,_)
	      | LiBGt(_,_) | LiBGeq(_,_) 
	      | LiBLeq (_,_)|  LiBPtrLeq (_ , _) 
	      |LiBPtrGeq (_,_) | LiBPtrLt (_, _) 
	      |LiBPtrGt (_, _) | LiBPtrNeq (_, _) 
	      	-> 
		
"!"^(c_bool_to_string b) 
		
	      | _ -> "!("^(c_bool_to_string b)^")" 
	  end
    | LiBAnd ( b1 , b2 ) -> "("^(c_bool_to_string b1) ^"&&"^(c_bool_to_string b2) ^")" 
    | LiBOr ( b1 , b2 ) ->  "("^(c_bool_to_string b1) ^"||"^(c_bool_to_string b2) ^")"   
    | LiBTrue -> "1"
    | LiBFalse -> "0"
    | LiBEq( bg , bd) -> "("^(scal_to_string bg) ^"="^(scal_to_string bd) ^")"
    | LiBNeq ( bg , bd ) -> "("^(scal_to_string bg) ^"!="^(scal_to_string bd) ^")" 
    | LiBLt( bg , bd ) ->  "("^(scal_to_string bg) ^"<"^(scal_to_string bd) ^")" 
    | LiBGt ( bg , bd ) ->  "("^(scal_to_string bg) ^">"^(scal_to_string bd) ^")" 
    | LiBLeq ( bg , bd ) ->  "("^(scal_to_string bg) ^"<="^(scal_to_string bd) ^")" 
    | LiBGeq ( bg , bd ) ->  "("^(scal_to_string bg) ^">="^(scal_to_string bd) ^")" 
    | LiBScal ( c_scal ) ->  
      "("^(scal_to_string c_scal)^")!=0"(* true iff != 0 *)
    
    | LiBPtrEq ( ptrg , ptrd ) ->
      "("^(ptrexp_to_str ptrg)^"="^(ptrexp_to_str ptrd)^")"

    | LiBPtrLeq ( eg , ed )  ->
      "("^(ptrexp_to_str eg )^"<="^(ptrexp_to_str ed )^")"
    
    | 	LiBPtrGeq  ( eg , ed )  -> 
      "("^(ptrexp_to_str eg )^">="^(ptrexp_to_str ed )^")"
    
    | 	LiBPtrGt  ( eg , ed )  -> 
      "("^(ptrexp_to_str eg )^">"^(ptrexp_to_str ed )^")"
    
    | LiBPtrLt ( eg , ed )  -> 
      "("^(ptrexp_to_str eg )^"<"^(ptrexp_to_str ed )^")" 
    
    | LiBPtrNeq ( ptrg , ptrd ) ->
      "("^(ptrexp_to_str ptrg)^"!="^(ptrexp_to_str ptrd)^")"

(** Call the function above and prints the output in the out formater.*)
let pretty_print_c_bool ( out_channel :Format.formatter) (b_exp : c_bool ) =
  Format.fprintf  out_channel "%s" (c_bool_to_string b_exp)

