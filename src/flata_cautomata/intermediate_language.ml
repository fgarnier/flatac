(* In this files, we define the syntax, the types of objects used to
analyse C code, and whose syntax doesn't belong to FLATA syntax and
grammar.

Laboratoire Verimag, 2 Avenue de Vignate, 38610 GiÃ¨res, France.

For questions or remarks, contact florent.garnier__at__imag__dot__fr

*)


open Cil_types
open Int64
open Validity_types
open Ast_goodies
open Composite_type_types
open Composite_types

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
		       | LiTypeSizeof of Cil_types.typ
		       | LiCAliasTypeSizeof of Composite_type_types.c_type_name

type c_ptr = LiIntPtr of string (*The represented type is indeed an int*)



(* We define the type of  the scalars as follows.
basicaly, the scalar we consider in this language are all the scalars
that are the evaluation of arithmetic
expressions which ground terms are free integer variables
and integers constants.

*)

exception Bad_expression_type of string

let get_name_of_c_ptr p =
  match p with
      LiIntPtr s -> s


(** The type of integers scalar expressions*)
type c_scal = LiVar of primed * c_int_var
	      | LiConst of c_int_cst
	      | LiSymConst of c_int_sym_const  (*Like sizeof of types or 
					       defined constant *)
	      | LiProd of c_scal * c_scal
	      | LiSum of c_scal * c_scal
	      | LiMinus of c_scal * c_scal
	      | LiUnMin of c_scal
	      | LiMod of c_scal * c_scal   (*Modulo operator*)
	      | LiMinusPP of c_ptrexp * c_ptrexp *  Cil_types.typ
	      | LiScalOfAddr of c_ptrexp * Cil_types.typ 
                                        (* While casting a ptr to an 
					 integer type*) 
		  
and c_ptrexp = LiPVar of primed * c_ptr *  Cil_types.typ
	       | LiBaseAddrOfArray (* Base address of an array*)
		   of primed * c_ptr * li_array_size * Cil_types.typ
	       | LiPlusPI of c_ptrexp * c_scal  * Cil_types.typ
	       | LiIndexPI of c_ptrexp * c_scal * Cil_types.typ
	       | LiMinusPI of c_ptrexp * c_scal * Cil_types.typ
	       | LiAddrOfScal of c_scal * Cil_types.typ
	       
and li_array_size = LiArraySize of c_scal
		     | LiArraySizeUnknown


type il_expr = IlScal of c_scal
	       | IlPtr of c_ptrexp
	       
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

  Format.printf "In cil_expr_2_scalar %s \n" (Ast_goodies.pprint_cil_exp expr );
  Cil.d_exp Ast_goodies.debug_out expr;
  match expr.enode with 
      Const(CInt64(i,_,_))-> LiConst( LiIConst(My_bigint.to_int i))
    | Const(CEnum(e)) -> cil_enumitem_2_scalar e
    	  
    | Lval(Var(f),_)->
      begin
	match f.vtype with
	    TInt(_,_) -> LiVar(Unprimed,LiIntVar(f.vname))
	  | TPtr(_,_) ->  (* Modified on the 20-10-11, need to check 
						impact.*)

	    let msg = "This variable : "^f.vname ^"Has a pointer type, but appears in a scalar expression, and I don't know what to do with it \n" in 
	    let exc =  Bad_expression_type msg in
	    raise  exc
	  
	  | TComp(_,_,_) -> 
	    LiVar(Unprimed,LiIntVar(f.vname))

	  
	  | TEnum(e,_) -> 
	    LiVar(Unprimed,LiIntVar(f.vname))

	  | _-> begin 
	      let alias_tname = Composite_types.is_integer_type f.vtype in
		begin
		  match alias_tname with
		    | Some(_) -> LiVar(Unprimed,LiIntVar(f.vname))
		    | None ->
			let msg = "This variable : "^f.vname ^"isn't of type TInt, but appears in a scalar expression \n" in 
			let exc =  Bad_expression_type msg in
			  raise  exc
		end  
	    end
      end	

    | Lval(Mem(e),_) ->
      begin
	let t = Cil.typeOf e in
	match t with
	    TPtr(t,_) ->
	      let ptr_addr_e = cil_expr_2_ptr e in
	      LiScalOfAddr(ptr_addr_e,t)

	  | _ -> 
	    let msg = Format.sprintf "[Cil_expr_2_scalar :] Accessing LVal(Mem(e),_) where
e is not of type TPtr(_,_), e : %s\n" (Ast_goodies.pprint_cil_exp e) in
	    raise (Bad_expression_type(msg))
      end
	
    | SizeOf ( t ) -> LiSymConst( LiTypeSizeof ( t ) ) (*  Added 9-9-11 *)

    | CastE ( t , expr ) -> 
      begin (* If here, one expects the wildcarded
	       type to be an integer type.*) 
	let exp_type = Cil.typeOf expr in
	match exp_type with
	    TComp (_,_,_)  |
		TNamed(_,_) |
		TInt(_,_) ->  cil_expr_2_scalar expr (* Added 9-9-11,
							  Completed 21-10-11*)
	  | TPtr(_,_) ->
	    let ptr_val = cil_expr_2_ptr expr in
	    LiScalOfAddr(ptr_val , t )
	      
	  | TEnum(_,_) ->
	    begin
	      match expr.enode with
		  Const(CEnum(e)) -> cil_enumitem_2_scalar e
		| _ -> 
		  let msg = Format.sprintf  "[Cil_exp_2_scalar :] Trying to
cast an expression whose type is TEnum but which is not embeded in a CEnum constructor \n"  in
	    raise ( Bad_expression_type(msg))
	    end
	      
	  | _ ->  
	    let msg = Format.sprintf  "Trying to cast a value to an
integer type, which type is neither TInt nor TPtr : %s , type : %s .\n" (pprint_cil_exp expr) (pprint_ciltypes exp_type)in
	    raise ( Bad_expression_type(msg))
      end
	

    | BinOp (PlusA, expg, expd, t ) ->
      let alias_tname = Composite_types.is_integer_type t in
      begin
	match alias_tname with
	    Some(_) ->
	      LiSum (cil_expr_2_scalar expg , cil_expr_2_scalar expd)
	  | None -> 
	    let msg = "[cil_expr_2_scalar :] Trying to define a Sum with non integer type"^(Ast_goodies.pprint_cil_exp expr)
	    in
	    raise( Bad_expression_type (msg))
      end


    | BinOp (MinusA, expg, expd, t ) ->
      let alias_tname = is_integer_type t in
      begin
	match alias_tname with
	    Some(_) -> LiMinus (cil_expr_2_scalar expg ,cil_expr_2_scalar  expd)
	  | None -> 
	    let msg = "[cil_expr_2_scalar :] Trying to define a Difference with non integer type"^(Ast_goodies.pprint_cil_exp expr)
	    in
	    raise( Bad_expression_type (msg))
      end
	
    | BinOp (Mult, expg, expd, t ) -> 
      let alias_tname = is_integer_type t in
      begin
	match alias_tname with
	    Some(_) ->
	      LiProd (cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
	  
	  | None -> 
	    let msg = "[cil_expr_2_scalar :] Trying to define a Product with non integer type"^(Ast_goodies.pprint_cil_exp expr)
	    in
	    raise( Bad_expression_type (msg))
      end	
	  
    | BinOp (Mod, expg, expd, t) ->
      let alias_tname = is_integer_type t in
      begin
	match alias_tname with
	    Some(_) ->
	      LiMod(cil_expr_2_scalar expg ,cil_expr_2_scalar expd )
		
	  | None -> 
	    let msg = "[cil_expr_2_scalar :] Trying to define a Modulo operation with non integer type"^(Ast_goodies.pprint_cil_exp expr)
	    in
	    raise( Bad_expression_type (msg))
      end	

    | BinOp (MinusPP , expg , expd , optype ) ->
      LiMinusPP(cil_expr_2_ptr expg , cil_expr_2_ptr expd, optype)
    
    | UnOp (Neg, exp , TInt(_,_)) ->
      LiUnMin ( cil_expr_2_scalar exp)
       
    | _ -> 
      Cil.d_exp Ast_goodies.debug_out expr;
      raise( Bad_expression_type ("Can't parse expression in cil_expr_2_scalar : %s \n"^(Ast_goodies.pprint_cil_exp expr)))

and cil_expr_2_ptr (expr : Cil_types.exp ) =
   Format.printf "In cil_expr_2_ptr %s \n" (Ast_goodies.pprint_cil_exp expr );
  Cil.d_exp Ast_goodies.debug_out expr;
  match expr.enode with
    
      BinOp (PlusPI, expg, expd , optype ) ->
	LiPlusPI(cil_expr_2_ptr expg, cil_expr_2_scalar expd , optype )
    
    | BinOp (IndexPI , expg , expd ,  optype ) ->
      LiIndexPI ( cil_expr_2_ptr expg, cil_expr_2_scalar expd, optype )
    
    | BinOp ( MinusPI , expg , expd , optype ) ->
      LiMinusPI ( cil_expr_2_ptr expg , cil_expr_2_scalar expd , optype )
	
    | Lval (Var(vinfo), _ ) ->
      begin
	match vinfo.vtype with
	    TPtr( vtypeptr, _ ) -> LiPVar(Unprimed, LiIntPtr(vinfo.vname),vtypeptr)
	  | _ ->  begin 
	    let msg = "This variable : "^vinfo.vname ^"is a pointer which isn't of  type TPtr, but that appears in a Lvalue expression that should have type pointer type \n" 
	    in let exc =  Bad_expression_type msg in 
	       raise exc
	  end
      end

    | Lval (Mem(e), _ ) -> 
      let type_of_e = Cil.typeOf e in
      begin
	match type_of_e with
	    TInt(_,_) -> LiAddrOfScal((cil_expr_2_scalar e), type_of_e)
	  | TPtr(_,_) -> cil_expr_2_ptr e
	  | _ -> 
	    begin
	      match  (Composite_types.is_integer_type type_of_e) with
		  Some(_) ->
		    LiAddrOfScal((cil_expr_2_scalar e), type_of_e)
		| None ->
		  raise  ( Bad_expression_type "Lval Mem(e) has neither an interger 
type nor pointer type TPtr .\n")
	    end
      end

    | CastE ( t , expression ) -> (* If here, one expects the wildcarded
				  type to be a pointer type.*) 
      let exp_type = Cil.typeOf expression in
      begin
	match exp_type with
	    TInt(_,_) -> 
	      let int_val = cil_expr_2_scalar expression in
	      LiAddrOfScal ( int_val , t)
		
	  | TPtr(_,_) ->
	    cil_expr_2_ptr expression
	      
	  | _ -> 
	    begin
	      match (Composite_types.is_integer_type exp_type) with
		  Some(_) ->
		    LiAddrOfScal((cil_expr_2_scalar expression), exp_type)
	
		| None ->
		  raise ( Bad_expression_type "Trying to cast a value to an
address type, which type is neither TInt nor TPtr.\n")
	    end
      end
      	

    | StartOf((Var(v),NoOffset))-> (* Implicit conversion form 
				  an array to a pointer.*)
      begin
	match v.vtype with
	    TArray(t,e,_,_)->
	      begin
		match e with
		  |  Some(size_exp) ->
		    let size_c_scal =  cil_expr_2_scalar size_exp in 
		    LiBaseAddrOfArray(Unprimed, LiIntPtr(v.vname),
				      LiArraySize( size_c_scal), t )
		      
		  | None -> 
		    LiBaseAddrOfArray(Unprimed, LiIntPtr(v.vname),LiArraySizeUnknown, t)
	      end

      end
	

    | StartOf((Var(v),Index(index,_)))->
      (*Getting the address that corresponds to the Indexed element
      of the array --Type, name contained in Var(v)
	nota bene : This algorithm doesn't handle multi dimentional
	arrays.
      *)
      begin
	match v.vtype with
	    TArray(t,e,_,_)->
	      begin
		let expr_of_index = cil_expr_2_scalar index in
		let base_addre =  
		  begin
		    match e with
		      |  Some(size_exp) ->
			let size_c_scal =  cil_expr_2_scalar size_exp in
			LiBaseAddrOfArray(Unprimed, LiIntPtr(v.vname),
					  LiArraySize( size_c_scal), t )
			  
		      | None ->
			LiBaseAddrOfArray(Unprimed, LiIntPtr(v.vname),LiArraySizeUnknown, t)
		  end
		in
		let addr =  LiIndexPI(base_addre,expr_of_index,t)
		in addr
	      end

      end
	
    | Const(CStr(s))->
	begin
	  let l = String.length s in
	  let t = TInt(IChar,[]) in
	    (LiBaseAddrOfArray(Unprimed,LiIntPtr(""),LiArraySize(LiConst(LiIConst(l))),t))
	end

	
    |  _ -> 
      begin 
	let msg = Format.sprintf " There is something I was unable to properly
parse in the cil_expr_2_ptr function %s" (Ast_goodies.pprint_cil_exp expr) 
	in 
	let format_warning = Format.formatter_of_out_channel stdout in
	Format.fprintf format_warning "[INtermediate language ] : FATAL, can't interpret expression :";
	Cil.d_exp format_warning expr;
	Format.fprintf format_warning " \n[INtermediate language ] raising exception \n %!";
	
	let exc =  Bad_expression_type msg in 
	   raise exc
      end

and cil_enumitem_2_scalar (enum : Cil_types.enumitem ) =
  let eval = cil_expr_2_scalar enum.eival in
  eval
      

let rec cil_expr_2_bool (expr : Cil_types.exp) =
  match expr.enode with 
       BinOp(LAnd,expg ,expd , _) ->
	 LiBAnd( cil_expr_2_bool expg, cil_expr_2_bool expd)
       
    |  BinOp(LOr, expg, expd , _) ->
	 LiBOr( cil_expr_2_bool expg, cil_expr_2_bool expd )
    |  UnOp(LNot , exp , TInt(IBool,_) ) ->
	 LiBNot(cil_expr_2_bool exp)
	 
    | BinOp(Lt,expg,expd,_) -> 
      begin
	let targs = Cil.typeOf expr in
	match targs with
	    TPtr(_,_) -> LiBPtrLt(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
	  | _ -> 	LiBLt(cil_expr_2_scalar expg ,cil_expr_2_scalar expd) 
      end
	
    | BinOp(Gt,expg,expd,_) ->
      begin
	let targs = Cil.typeOf expr in
	match targs with
	    TPtr(_,_) -> LiBPtrGt(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
	  | _ -> LiBGt(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
      end

    | BinOp(Le,expg,expd,_) ->
      begin
	let targs = Cil.typeOf expr in
	match targs with
	    TPtr(_,_) ->  LiBPtrLeq(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
	  | _->   LiBLeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
      end

    | BinOp(Ge,expg,expd,_) ->
      begin
	let targs = Cil.typeOf expr in
	match targs with
	    TPtr(_,_) ->  LiBPtrGeq(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
	  | _ ->LiBGeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
      end
	
    | BinOp(Ne,expg,expd,_) ->
       begin
	let targs = Cil.typeOf expr in
	match targs with
	    TPtr(_,_) ->  LiBPtrNeq(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
	  |_-> LiBNeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
       end


    | BinOp (Eq , expg , expd , TPtr ( _ , _)) ->
       begin
	 LiBPtrEq(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
   
       end


    | BinOp (Eq , expg, expd, _) ->
      begin
	LiBEq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
      end

    | Const(CInt64(value,_,_)) ->  
      LiBScal(LiConst( LiIConst (My_bigint.to_int value)))
    
    | _-> 

      begin
	let etype = Cil.typeOf expr in
	match etype with
	    TInt(_,_) -> 
	      begin
		let cscal_exp =cil_expr_2_scalar expr in
		LiBNeq(cscal_exp,LiConst(LiIConst(0)))
	      end
		
	  | _ ->
	    begin
	      let alias_tname = Composite_types.is_integer_type etype in
	      match alias_tname with
		| Some(_) -> 
		  let cscal_exp =cil_expr_2_scalar expr in
		  LiBNeq(cscal_exp,LiConst(LiIConst(0)))
		    
		| None ->
		  let msg = Format.sprintf "Trying to parse an expression \
 that can't be evaluated as a boolean : %s\n" (Ast_goodies.pprint_cil_exp expr )
		  in
		  raise ( Bad_expression_type msg )

	    end  
      end
	
	
     


(********************************************************************)

(** This function transforms a list of cil expression into a list
of scalar expression, whenever possible. It raises a Bad_expression_type
exception if something wrong occured.  *)

(*******************************************************************)


let cil_expr_list_2_scalar_list (expr_list : Cil_types.exp list ) =
  List.map cil_expr_2_scalar expr_list



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

let pprint_ciltypes_size (ciltype : Cil_types.typ ) =
  match ciltype with
      TInt(IBool,_) -> "sizeof_bool"
    | TInt(IChar,_) -> "sizeof_char"
    | TInt(ISChar,_) -> "sizeof_signed_char"
    | TInt(IUChar,_) -> "sizeof_unsigned_char"	
    | TInt(IInt,_) -> "sizeof_int"
    | TInt(IUInt,_) -> "sizeof_unsigned_int"
    | TInt(IShort,_) -> "sizeof_short"
    | TInt(IUShort,_) -> "sizeof_unsigned_short"
    | TInt(ILong,_) -> "sizeof_long"
    | TInt(IULong,_) -> "sizeof_unsigned_long"
    | TInt(ILongLong,_) -> "sizeof_long_long"
    | TInt(IULongLong,_) -> "sizeof_unsigned_long_long"	
    | TFloat(FFloat,_) -> "sizeof_float"
    | TFloat(FDouble,_) -> "sizeof_double"
    | TFloat(FLongDouble,_) -> "sizeof_long_double"
   (* | TNamed(tinfo, _ ) -> sizeof_cil_tinfo tinfo *)
    | _ -> " Unhandled_valuetype_in_interpretciltypesize"


let rec scal_to_string ( b_exp : c_scal ) =
  match b_exp with 
      LiVar(Unprimed,LiIntVar(vname)) -> vname (* returns the name of the variable*)
    | LiVar(Primed,LiIntVar(vname)) -> vname^"'" 
    | LiConst(LiIConst(i)) -> (Printf.sprintf "%d" i )
    | LiSymConst(LiSymIConst(const_name)) -> const_name
    | LiSymConst(LiTypeSizeof(t)) -> let s = pprint_ciltypes_size t in s
    | LiScalOfAddr(e , t)->"(TINT of Addr cast)"^(ptrexp_to_str e)
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
    
    | LiMinusPP (ptrg , ptrd, _ ) ->
      "("^( ptrexp_to_str  ptrg )^"-"^ ( ptrexp_to_str  ptrd )^")"

and ptrexp_to_str ( cptr : c_ptrexp ) =
  match cptr with 
    
      LiPVar ( Primed , LiIntPtr ( vname), _ ) ->
	vname^"'"

    | LiBaseAddrOfArray(_,LiIntPtr(vname),LiArraySize(size),t) ->
      Format.sprintf "&(%s : %s[%s])" vname (Ast_goodies.pprint_ciltypes t) (scal_to_string size)
	
    | LiBaseAddrOfArray(_,LiIntPtr(vname),LiArraySizeUnknown,t) ->
      Format.sprintf "&(%s : %s[Array of unknown size]" vname (pprint_ciltypes t) 	
    
    | LiPVar ( Unprimed , LiIntPtr ( vname ), _) ->
      vname

    |  LiAddrOfScal (e , _) -> "(Addr of TINT)"^(scal_to_string e)

    | LiPlusPI ( ptr_in , offset, _ ) ->
      ( ptrexp_to_str  ptr_in )^"["^(scal_to_string offset)^"]"
    
    | LiIndexPI ( ptr_in , offset , _ ) ->
       ( ptrexp_to_str  ptr_in )^"["^(scal_to_string offset)^"]"
    
    | LiMinusPI (ptr_in , offset , _ ) ->
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
	      | LiBPtrGeq (_,_) | LiBPtrLt (_, _) 
	      | LiBPtrGt (_, _) | LiBPtrNeq (_, _) 
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
	
    | LiBPtrGeq  ( eg , ed )  -> 
      "("^(ptrexp_to_str eg )^">="^(ptrexp_to_str ed )^")"
	
    | LiBPtrGt  ( eg , ed )  -> 
      "("^(ptrexp_to_str eg )^">"^(ptrexp_to_str ed )^")"
    
    | LiBPtrLt ( eg , ed )  -> 
      "("^(ptrexp_to_str eg )^"<"^(ptrexp_to_str ed )^")" 
    
    | LiBPtrNeq ( ptrg , ptrd ) ->
      "("^(ptrexp_to_str ptrg)^"!="^(ptrexp_to_str ptrd)^")"

(** Call the function above and prints the output in the out formater.*)
let pretty_print_c_bool ( out_channel :Format.formatter) (b_exp : c_bool ) =
  Format.fprintf  out_channel "%s" (c_bool_to_string b_exp)

