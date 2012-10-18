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
open Ssl 
open Ssl_types
open Ssl_types.SSL_lex
open Intermediate_language_types

exception Bad_expression_type of string
exception Array_elements_not_integers
exception Not_Array_Index
exception Not_Array_type
exception UnnammedLocalArray
exception Contains_no_ivar
exception Contains_no_fvar
exception Array_elements_not_scalar (*Neither integer nor real *)



(** One need to translate C-boolean evaluation into the language of FLATA
constrainsts. That's to say : 
Translating C-booleans expressions in the  "intermediate language " 
into the FLATA grammar, if there exists a matching transformation.
 Takes a c_bool expression as parameter then returns its negation.
The negation unary operators are pushed in the bottmost position
in the expression tree.
*)



let get_name_of_c_ptr p =
  match p with
      LiIntPtr s -> s


let lipvar_of_pvar ( pvar : SSL_lex.ptvar)(typ : Cil_types.typ) =
  match pvar with
      SSL_lex.PVar(name)-> LiPVar(Unprimed,LiIntPtr(name),typ)


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
    | LiBScal( exp ) -> LiBEq ( exp, LiConst(LiIConst((My_bigint.of_int(0)))))
    | LiBPtrEq ( expg , expd ) -> LiBPtrNeq ( expg , expd )
    | LiBPtrNeq  ( expg , expd ) ->  LiBPtrEq ( expg , expd )
    | LiBPtrGt ( eg ,ed ) -> LiBPtrLeq (eg ,ed )
    | LiBPtrLt ( eg , ed ) -> LiBPtrGeq (eg ,ed )
    | LiBPtrGeq ( eg ,ed ) -> LiBPtrLt (eg ,ed)
    | LiBPtrLeq (eg ,ed ) -> LiBPtrGt (eg ,ed)


let get_base_type_of_array (ttab : Cil_types.typ) =
  let rec base_type vtype =
    match vtype with
	TArray(t,_,_,_) ->
	  base_type t
      | _ -> vtype
  in
  match ttab with
      TArray(subtype,_,_,_) -> base_type subtype
    | _ -> 
      begin
	Format.fprintf Ast_goodies.debug_out "\n Type given as a parameter is not an array type. Type is : ";
	Cil.d_type Ast_goodies.debug_out ttab;
	Format.fprintf Ast_goodies.debug_out "\n Abort  \n %! ";
	raise Not_Array_type
      end
      



let rec cil_expr_2_scalar (expr : Cil_types.exp ) =

  Format.printf "In cil_expr_2_scalar %s \n" (Ast_goodies.pprint_cil_exp expr );
  Cil.d_exp Ast_goodies.debug_out expr;
  match expr.enode with 
      Const(CInt64(cil_cst,_,_))-> LiConst( LiIConst(cil_cst))
    | Const(CChr(c)) -> LiSymConst(LiSymIConst(String.make 1 c))
    | Const(CEnum(e)) -> cil_enumitem_2_scalar e
    | Const(CReal(v,_,_)) -> LiFConst(LiFloatConst(v))
    	  
    | Lval(Var(f),offset)->
      begin
	let typeofexp = Cil.typeOf expr in
	let alias_tname = Composite_types.is_integer_type typeofexp in
	begin
	  match alias_tname with
	    | Some(_) ->
	      get_ivar_from_exp expr 
	      (*LiVar(Unprimed,LiIntVar(name_of_var))*)
	    | None ->
	      begin
		let alias_tname = Composite_types.is_float_type typeofexp in
		 match alias_tname with
		   | Some(_) ->
		     get_fvar_from_exp expr 
	    (*LiVar(Unprimed,LiIntVar(name_of_var))*)
		   | None ->
		     let msg = "This variable : "^f.vname ^"is neither an integer nor a real value, but appears in a scalar expression \n" in 
		     let exc =  Bad_expression_type msg in
		     raise  exc
	      end
	end  
      end
      	

    | Lval(Mem(e),offset) ->
      begin
	let t = Cil.typeOf e in
	match t with
	    TPtr(tparam,_) ->
	      Format.fprintf  debug_out "[cil_expr_2_cscal] : lval is a Mem(e) \n";
	      Cil.d_lval debug_out  ( Mem(e), offset);
	      Format.fprintf  debug_out "\n";
	      let ptr_addr_e = cil_expr_2_ptr e in
	      LiScalOfAddr(ptr_addr_e,t)

	  | _ -> 
	    let msg = Format.sprintf "[Cil_expr_2_scalar :] Accessing LVal(Mem(e),_) where
e is not of type TPtr(_,_), e : %s\n" (Ast_goodies.pprint_cil_exp e) in
	    raise (Bad_expression_type(msg))
      end
	
    | SizeOf ( t ) -> LiSymConst( LiTypeSizeof ( t ) ) (*  Added 9-9-11 *)


    | SizeOfE (expr) -> 
      begin
	let t =  Cil.typeOf expr in
	LiSymConst( LiTypeSizeof ( t ) )
      end
	
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
	      
	  | _ -> 
	    begin
	      if (Composite_types.is_scalar_type exp_type) then
		  
		cil_expr_2_scalar expr 
		      
	      else
		  raise ( Bad_expression_type "This is not an integer value, as I expected \n")
	    end 
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

    | BinOp (Div, expg, expd , t ) ->
      begin
	let lg = cil_expr_2_scalar expg in
	let ld = cil_expr_2_scalar expd in
	LiDiv(lg,ld)
      end


    | BinOp (MinusPP , expg , expd , optype ) ->
      LiMinusPP(cil_expr_2_ptr expg , cil_expr_2_ptr expd, optype)
    
    | UnOp (Neg, exp , TInt(_,_)) ->
      LiUnMin ( cil_expr_2_scalar exp)


    | UnOp(LNot,exp,_) ->
      Format.fprintf Ast_goodies.debug_out " Unop detected \n";
      let bool_expr = cil_expr_2_bool exp
      in LiScalOfLiBool(LiBNot(bool_expr))

    | BinOp(op,fg,fd,t) ->
      begin
	match op with
	    Lt | Gt | Le | Ge |  Eq | Ne|  LAnd | LOr
		-> 
		  let bool_exp  = cil_expr_2_bool expr
		  in LiScalOfLiBool(bool_exp)
		   
	  | BAnd | BXor | BOr 
	    ->
	    raise (Bad_expression_type (" Logical operation on bits are not yet implemented"))
	  | _ -> 
	 
	    begin
	      Format.printf "I Crash here with that :\n";
	      Cil.d_exp Ast_goodies.debug_out expr;
	      raise (Bad_expression_type("Unknown Binop it seems"))
	    end
	    
      end
    | _ ->
      Format.fprintf Ast_goodies.debug_out "Cil_expr_2_scal crash \n Can't parse expression : ";
      Cil.d_exp Ast_goodies.debug_out expr;
      Format.fprintf  Ast_goodies.debug_out " \n ";
      raise( Bad_expression_type ("Can't parse expression in cil_expr_2_scalar : %s \n"^(Ast_goodies.pprint_cil_exp expr)))


and get_int_var_field expr =
  let dummyexp = Cil.dummy_exp expr in
  let typeofexp = Cil.typeOf dummyexp in
  match expr with 
      Lval(Var(f),offset) ->
	begin 
	  let alias_tname = Composite_types.is_integer_type typeofexp in
	  begin
	    match alias_tname with
	      | Some(_) ->
		get_ivar_from_exp dummyexp 
	    (*LiVar(Unprimed,LiIntVar(name_of_var))*)
	      | None ->
		let msg = "This variable : "^f.vname ^"isn't of type TInt, but appears in a scalar expression \n" in 
		let exc =  Bad_expression_type msg in
		raise  exc
	  end  
	end

    | _ ->  let msg = "Function get_int_var_field only accepts expressions that matches the following constructor Lval(Var(_),_) " in 
	    let exc =  Bad_expression_type msg in
	    raise  exc


and get_float_var_field expr =
  let dummyexp = Cil.dummy_exp expr in
  let typeofexp = Cil.typeOf dummyexp in
  match expr with 
      Lval(Var(f),offset) ->
	begin 
	  let alias_tname = Composite_types.is_float_type typeofexp in
	  begin
	    match alias_tname with
	      | Some(_) ->
		get_fvar_from_exp dummyexp 
	    (*LiVar(Unprimed,LiIntVar(name_of_var))*)
	      | None ->
		let msg = "This variable : "^f.vname ^"isn't of type TReal, but appears in a scalar expression that has type Float \n" in 
		let exc =  Bad_expression_type msg in
		raise  exc
	  end  
	end

    | _ ->  let msg = "Function get_float_var_field only accepts expressions that matches the following constructor Lval(Var(_),_) " in 
	    let exc =  Bad_expression_type msg in
	    raise  exc
  
  




and cil_expr_2_ptr (expr : Cil_types.exp ) =
   Format.printf "In cil_expr_2_ptr %s \n" (Ast_goodies.pprint_cil_exp expr );
  Cil.d_exp Ast_goodies.debug_out expr; Format.fprintf Ast_goodies.debug_out "\n";
  match expr.enode with
    
      BinOp (PlusPI, expg, expd , optype ) ->
	LiPlusPI(cil_expr_2_ptr expg, cil_expr_2_scalar expd , optype )
    
    | BinOp (IndexPI , expg , expd ,  optype ) ->
      LiIndexPI ( cil_expr_2_ptr expg, cil_expr_2_scalar expd, optype )
    
    | BinOp ( MinusPI , expg , expd , optype ) ->
      LiMinusPI ( cil_expr_2_ptr expg , cil_expr_2_scalar expd , optype )
	
    | Lval (Var(vinfo), _ ) ->
      begin
	let type_of_exp = Cil.typeOf expr in
	match type_of_exp with
	    TPtr( vtypeptr, _ ) ->
	      begin  
		let pvar = Ast_goodies.get_pvar_from_exp_node expr.enode  in
		let vname = Ssl.get_name_of_ptvar pvar in
		Format.printf "[cil_expr_2_ptr Lval (Var(vinfo), _ )] : TPtr, vname =  %s \n" vname;
		LiPVar(Unprimed,LiIntPtr(vname),vtypeptr)
	      end
	
	  | _ ->  begin 
	    let msg = "This variable : "^vinfo.vname ^"is a pointer which isn't of  type TPtr, but that appears in a Lvalue expression that should have type pointer type \n" 
	    in let exc =  Bad_expression_type msg in 
	       raise exc
	  end
      end

    | Lval (Mem(e), offset ) -> 
      
      let type_of_e = Cil.typeOf e in
      
      begin
	Format.printf "[cil_expr_2_ptr] :  Lval (Mem(e), offset ) has type :";
	let type_of_lval = Cil.typeOfLval ( Mem(e) , offset ) in
	Cil.d_type debug_out type_of_lval;
	Format.printf "\n LVal expression is";
	Cil.d_lval Ast_goodies.debug_out ( Mem(e) , offset );
	Ast_goodies.pprint_cil_exp (Cil.dummy_exp (Lval( Mem(e) , offset)));
	Format.fprintf Ast_goodies.debug_out "\n";
	
	
	match type_of_lval with
	    TInt(_,_) -> (* Addresses of constants such as 0x0, NULL *)
	      LiAddrOfScal((cil_expr_2_scalar e), type_of_e) 
	  | TPtr(_,_) ->
	    begin
	      Format.fprintf Ast_goodies.debug_out "[cil_exp_2_ptr :]Calling Ast_goodies.get_pvar_from_exp_node upon : \n";
	      Cil.d_lval Ast_goodies.debug_out (Mem(e), offset); 
	      Format.fprintf Ast_goodies.debug_out "\n paramter infos %s \n%!" (Ast_goodies.pprint_cil_exp (Cil.dummy_exp (Lval(Mem(e),offset))));
	      let pvar = 
		Ast_goodies.get_pvar_from_exp_node (Lval (Mem(e), offset ))
	      in  
	      let li_pvar = lipvar_of_pvar  pvar type_of_lval in
	      LiStarOfPtr(li_pvar,type_of_lval)
	    end
	      
	  | _ ->  raise (Bad_expression_type ("In cil_expre_2_ptr, trying to accessed the value of a pointer, which value isn't a pointer."))
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
      	

    | StartOf((Var(v),offset_access))-> (* Implicit conversion form 
					   an array to a pointer.*)
      begin
	(*match v.vtype with
	    TArray(t,e,_,_)->
	      begin
		let c_array_dim = array_dim v.vtype [] in 
		let base_type = get_base_type_of_array v.vtype  in
		let c_array= LiTab(Some(v.vname),c_array_dim, base_type) in
		let index_access =  get_array_index offset_access [] in
		LiBaseAddrOfArray(index_access,c_array)
     
	      end
	*)
	get_array_access_info_from_expr_node (Lval(Var(v),offset_access))
      end
	

    | StartOf(Mem(e),off) -> 
      begin
	
	(*
	  let array_access_info = 
	  get_array_access_info_from_expr_node 
	    (Lval(Mem(e),off))  in 
	array_access_info
	*)
	let type_of_exp = Cil.typeOf (Cil.dummy_exp (Lval(Mem(e),off))) 
	in
	let array_ptr_access_info =
	  get_accessed_array_pointer_info (Lval(Mem(e),off))  
	in
	LiStarOfPtr(array_ptr_access_info,type_of_exp)
     
      (*in
	Li*)
	(*
	Cil.d_lval Ast_goodies.debug_out (Mem(e),off);
	Format.fprintf Ast_goodies.debug_out "\n Offset :";
	Cil.d_offset Ast_goodies.debug_out off;
	Format.fprintf Ast_goodies.debug_out "\n %! Expression in Mem(e) is ";
	Cil.d_exp Ast_goodies.debug_out e;
	Format.fprintf Ast_goodies.debug_out "\n %!";
	
	
	assert false
	*)
      end

    | AddrOf( Var(v), offset ) ->
      begin
	match offset with
	    Field(finf,off) -> 
	      begin
		let sfield_name = Ast_goodies.get_subfield_name 
		  v.vname finf off in
		LiDerefCVar(sfield_name,v.vtype)
	      end
		
	  | Index(exp,off) ->
	    begin
	      let indexes = get_array_index offset [] in
	      let dim = array_dim v.vtype [] in
	      let parsed_tab = LiTab( Some(v.vname) , dim, v.vtype ) in
	      LiBaseAddrOfArray(indexes,parsed_tab)
	    end
	      
	  | NoOffset ->
	    begin
	      match v.vtype with
		  TArray(_,_,_,_) -> 
		    begin
		      let dim = array_dim v.vtype [] in
		      let parsed_tab = LiTab( Some(v.vname) , dim, v.vtype ) 
		      in
		      LiBaseAddrOfArray([],parsed_tab)
		    end
		
		| _->
		  begin
		    LiDerefCVar(v.vname,v.vtype)
		  end
	    end
	      
      end

    | AddrOf( Mem(e), offset ) ->
      begin
	match offset with
	    NoOffset -> cil_expr_2_ptr e
	  | _ -> 
	    let format_warning = Format.formatter_of_out_channel stdout in
	    Format.fprintf format_warning "AddrOf( Mem(e),offset), with offset != NoOffset : Operation not handled: expression is \n";
	    Cil.d_exp format_warning expr;
	    raise  ( Bad_expression_type("AddrOf( Mem(e),offset), with offset != NoOffset : Operation not handled") )
      end
	
      
    | Const(CStr(s))->
	begin
	  let l = LiConst(LiIConst((My_bigint.of_int (String.length s)))) in
	  let t = TInt(IChar,[]) in
	  let str_array = LiTab(None,(Some(l))::[],t) in
	  LiBaseAddrOfArray([],str_array)   
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

and cil_expr_2_bool (expr : Cil_types.exp) =
  match expr.enode with 
       BinOp(LAnd,expg ,expd , _) ->
	 LiBAnd( cil_expr_2_bool expg, cil_expr_2_bool expd)
       
    |  BinOp(LOr, expg, expd , _) ->
	 LiBOr( cil_expr_2_bool expg, cil_expr_2_bool expd )
    |  UnOp(LNot , exp , TInt(IBool,_) ) ->
	 LiBNot(cil_expr_2_bool exp)
	 
    | BinOp(Lt,expg,expd,_) -> 
      begin
	let targs = Cil.typeOf expg in
	match targs with
	    TPtr(_,_) -> LiBPtrLt(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
	  | _ -> 	LiBLt(cil_expr_2_scalar expg ,cil_expr_2_scalar expd) 
      end
	
    | BinOp(Gt,expg,expd,_) ->
      begin
	let targs = Cil.typeOf expg in
	match targs with
	    TPtr(_,_) -> LiBPtrGt(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
	  | _ -> LiBGt(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
      end

    | BinOp(Le,expg,expd,_) ->
      begin
	let targs = Cil.typeOf expg in
	match targs with
	    TPtr(_,_) ->  LiBPtrLeq(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
	  | _->   LiBLeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
      end

    | BinOp(Ge,expg,expd,_) ->
      begin
	let targs = Cil.typeOf expg in
	match targs with
	    TPtr(_,_) ->  LiBPtrGeq(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
	  | _ ->LiBGeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
      end
	
    | BinOp(Ne,expg,expd,_) ->
       begin
	let targs = Cil.typeOf expg in
	match targs with
	    TPtr(_,_) ->  LiBPtrNeq(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
	  |_-> LiBNeq(cil_expr_2_scalar expg ,cil_expr_2_scalar expd)
       end


    | BinOp (Eq , expg , expd , _) ->
	begin
	  let targs = Cil.typeOf expg in
	    match targs with
		TPtr(_,_) ->  LiBPtrEq(cil_expr_2_ptr expg, cil_expr_2_ptr expd)
	      | _ -> LiBEq(cil_expr_2_scalar expg, cil_expr_2_scalar expd)	
	end


    | Const(CInt64(value,_,_)) ->  
      LiBScal(LiConst( LiIConst ( value )))
    
    | _->

      begin
	let etype = Cil.typeOf expr in
	match etype with
	    TInt(_,_) -> 
	      begin
		let cscal_exp =cil_expr_2_scalar expr in
		LiBNeq(cscal_exp,LiConst(LiIConst(My_bigint.zero)))
	      end
		
	  | _ ->
	    begin
	      let alias_tname = Composite_types.is_integer_type etype in
	      match alias_tname with
		| Some(_) -> 
		  let cscal_exp =cil_expr_2_scalar expr in
		  LiBNeq(cscal_exp,LiConst(LiIConst(My_bigint.zero)))
		    
		| None ->
		  let msg = Format.sprintf "Trying to parse an expression \
 that can't be evaluated as a boolean : %s\n" (Ast_goodies.pprint_cil_exp expr )
		  in
		  raise ( Bad_expression_type msg )

	    end  
      end

and cil_enumitem_2_scalar (enum : Cil_types.enumitem ) =
  let eval = cil_expr_2_scalar enum.eival in
  eval

(*Computes the index at which an array is accessed *) 
and  get_array_index (offset : Cil_types.offset) 
    ( i_list : c_scal list ) =
  match offset with 
      Index( e , off) -> 
	let i_list = i_list@((cil_expr_2_scalar e)::[]) in
	get_array_index off i_list
    
    | NoOffset -> i_list
    | _ -> raise Not_Array_Index 

(* Computes the dimention of an array when this information is
available at compilation time.*)
and  array_dim (tinfo : Cil_types.typ)
    (index_list : (c_scal option) list)=
  match tinfo with
      TArray(tinfo,Some(size),_,_)->
	begin
	  let size_array = Some((cil_expr_2_scalar size)) in
	  let index_list = index_list@(size_array::[]) in
	  array_dim tinfo index_list
	end
    |  TArray(tinfo,None,_,_)->
	begin
	  let size_array = None in
	  let index_list = index_list@(size_array::[]) in
	  array_dim tinfo index_list
	end

   (* | TPtr(TArray(tinfo,None,_,_),_)->
	begin
	  let size_array = None in
	  let index_list = index_list@(size_array::[]) in
	  array_dim tinfo index_list
	end 

    | TPtr(TArray(tinfo,Some(size),_,_),_)->
      begin
	 let size_array = Some((cil_expr_2_scalar size)) in
	 let index_list = index_list@(size_array::[]) in
	 array_dim tinfo index_list
	   
      end *) 
	
    | _ -> 
      if Composite_types.is_scalar_type tinfo
      then
	index_list
      else
	raise Array_elements_not_scalar

(* Extracts array dimentions from pointer of type Array(tinfo,_)* *)
and  pointed_array_dim (tinfo : Cil_types.typ)
    (index_list : (c_scal option) list)=
  match tinfo with
      TPtr(TArray(tinfo,Some(size),_,_),_)->
	begin
	  let size_array = Some((cil_expr_2_scalar size)) in
	  let index_list = index_list@(size_array::[]) in
	  array_dim tinfo index_list
	end
    |  TPtr(TArray(tinfo,None,_,_),_)->
	begin
	  let size_array = None in
	  let index_list = index_list@(size_array::[]) in
	  array_dim tinfo index_list
	end
   | _ -> 
      if Composite_types.is_scalar_type tinfo
      then
	index_list
      else
	raise Array_elements_not_scalar

(* Requires that e consists *)
and get_array_access_info_from_expr_node e =
  match e with
    | Lval(Var(v),off) ->
      begin
	match v.vtype with
	    TArray(t,e,_,_) 
	  (*| TPtr(TArray(t,e,_,_),_)*)
	    ->
	      begin
		let c_array_dim = array_dim v.vtype [] in 
		let base_type = get_base_type_of_array v.vtype  in
		let c_array= LiTab(Some(v.vname),c_array_dim, base_type) in
		let index_access =  get_array_index off [] in
		LiBaseAddrOfArray(index_access,c_array)
	      end
	  | _ ->
	    begin
	      Format.fprintf Ast_goodies.debug_out "Stuck on this expression \n";
	      Cil.d_exp Ast_goodies.debug_out ( Cil.dummy_exp e);
	      Format.fprintf  Ast_goodies.debug_out " \n %!";
              Format.fprintf Ast_goodies.debug_out "%s \n %!" (Ast_goodies.pprint_cil_exp ( Cil.dummy_exp e) );
	      assert false
	    end
      end
    | Lval(Mem(e),off) ->
      begin
	
	let ptr = 
	get_array_access_info_from_expr e
	in 
	ptr
	(*in
	LiStarOfPtr(prt,t) *)
      end

    | BinOp (IndexPI,array_info,index,typ)
    | BinOp (PlusPI,array_info,index,typ)
      ->
      begin
	let ptr = get_array_access_info_from_expr array_info in
	let index_info = cil_expr_2_scalar index in
	let ret_val =
	  ( match ptr with
	      	LiBaseAddrOfArray(index_access,c_array) ->
		  (LiBaseAddrOfArray(index_info::index_access,c_array))
	  )
	in
	ret_val
      end

    | _ ->
      begin
	Format.fprintf Ast_goodies.debug_out "Stuck on this expression \n";
	Cil.d_exp Ast_goodies.debug_out ( Cil.dummy_exp e);
	Format.fprintf  Ast_goodies.debug_out " \n %!";
        Format.fprintf Ast_goodies.debug_out "%s \n %!" (Ast_goodies.pprint_cil_exp ( Cil.dummy_exp e) );
	assert false
      end

and get_array_access_info_from_expr e =
  get_array_access_info_from_expr_node e.enode

and get_accessed_array_pointer_info e =
  match e with
    | Lval(Var(v),off) ->
      begin
	match v.vtype with
	    (*TArray(t,e,_,_)*) 
	      TPtr(TArray(t,e,_,_) as stype,_)
	    ->
	      begin
		let c_array_dim = pointed_array_dim v.vtype [] in 
		let base_type = get_base_type_of_array stype  in
		let c_array= LiTab(Some(v.vname),c_array_dim, base_type) in
		let index_access =  get_array_index off [] in
		LiBaseAddrOfArray(index_access,c_array)
	      end
	  | _ ->
	    begin
	      Format.fprintf Ast_goodies.debug_out "Stuck on this expression \n";
	      Cil.d_exp Ast_goodies.debug_out ( Cil.dummy_exp e);
	      Format.fprintf  Ast_goodies.debug_out " \n %!";
              Format.fprintf Ast_goodies.debug_out "%s \n %!" (Ast_goodies.pprint_cil_exp ( Cil.dummy_exp e) );
	      assert false
	    end
      end
    | Lval(Mem(e),off) ->
      begin
	
	let ptr = 
	get_accessed_array_pointer_info_from_expr e
	in 
	ptr
	(*in
	LiStarOfPtr(prt,t) *)
      end

    | BinOp (IndexPI,array_info,index,typ)
    | BinOp (PlusPI,array_info,index,typ)
      ->
      begin
	let ptr = get_accessed_array_pointer_info_from_expr array_info in
	let index_info = cil_expr_2_scalar index in
	let ret_val =
	  ( match ptr with
	      LiBaseAddrOfArray(index_access,c_array) ->
		(LiBaseAddrOfArray(index_info::index_access,c_array))
	  )
	in
	ret_val
      end

    | _ ->
      begin
	Format.fprintf Ast_goodies.debug_out "Stuck on this expression \n";
	Cil.d_exp Ast_goodies.debug_out ( Cil.dummy_exp e);
	Format.fprintf  Ast_goodies.debug_out " \n %!";
        Format.fprintf Ast_goodies.debug_out "%s \n %!" (Ast_goodies.pprint_cil_exp ( Cil.dummy_exp e) );
	assert false
      end

and get_accessed_array_pointer_info_from_expr e =
  get_accessed_array_pointer_info e.enode

and get_li_intvar_from_exp_node (expn : Cil_types.exp_node ) =
  match expn with
      Lval ( Var( p ) , off ) ->
	begin
	  Format.fprintf  debug_out "get_pvar_from_exp_node : lval is a Var(p) \n";
	  Cil.d_lval debug_out  ( Var(p), off);
	  Format.fprintf  debug_out "\n";
	  let type_of_lval = Cil.typeOfLval ( Var( p ) , off ) in
	   Format.fprintf  debug_out "get_ivar_from_exp_node : lval has type : \n %!";
	  Cil.d_type debug_out type_of_lval;
	   
	 
	 
	  let itypeval_of_expn = Composite_types.is_integer_type type_of_lval 
	  in
	  match itypeval_of_expn, off with
	      (Some(_), NoOffset) -> (*We have an integer or alias integer here*)
		LiVar(Unprimed,LiIntVar(p.vname))
		  
	    | (Some(_), Field(finfo,offs)) ->
	      begin
		let vname = get_subfield_name "" finfo offs in
		let vname = p.vname^"."^vname in
		LiVar(Unprimed,LiIntVar(vname))
	      end

	    | (Some(_),Index(_,_)) ->
		  (* In this case, an access to an element of an array
		     is performed.
		  *)
	      begin	
		match p.vtype with 
		    TArray(tinfo,_,_,_)->
		      begin
			let index_access = get_array_index off [] in
			let dim_of_tabs = array_dim p.vtype [] in
			let c_array =  LiTab(Some(p.vname),dim_of_tabs,tinfo) in
			LiElemOfCTab(index_access,c_array)
		      end
		  | _ -> 
		    raise (Debug_info ("In get_ivar_from_exp : The expression is not an Array, as it was expected, crash"))
	      end
		      
	    |  (None,_)-> 
	      raise (Debug_info (" In get_ivar_from_exp : The expression has not an integer type, whereas I was expecting some integer value \n"))
	end

    | Lval(Mem(e), off ) ->
      begin
	Format.printf "get_pvar_from_exp_node : lval is a Mem(e) \n";
	let type_of_expn = Cil.typeOfLval (Mem(e),off) in
	let itypeval_of_expn = Composite_types.is_integer_type type_of_expn 
	in
	match itypeval_of_expn, off with
	    (Some(_),NoOffset) ->
	      Format.fprintf Ast_goodies.debug_out 
		"\n Star of a pvar which value is an int \n%!" ;
	      Cil.d_lval Ast_goodies.debug_out (Mem(e),off);
	      
	      let pointer_var = Ast_goodies.get_pvar_from_exp e in
	      
	      let vtype_e = Cil.typeOf e in
	      let param_c_pvar =  lipvar_of_pvar pointer_var vtype_e in
	      LiIntStarOfPtr(param_c_pvar,type_of_expn)
		
	 | (Some(_), Field(finfo,offs)) -> 
	   let pointer_pvar =  Ast_goodies.get_pvar_from_exp_node (Lval(Mem(e), off )) in
	   let varname = Ssl.get_name_of_ptvar pointer_pvar in
	   Format.printf "%s \n" varname;
	   LiVar(Unprimed,LiIntVar(varname))
	     
	     
	 | (_,Index(_,_)) -> Format.printf "Some index \n"; 
	   raise (Debug_info ("In get_pvar_from_exp_node : I don't handle
  array indexes here and there is no reason why I should do it here.\n"))
	 | (_,_) -> raise (Debug_info ("Lost in get_ivar_from_exp_node \n"))
      end
	 

    

    | CastE(t,e) ->
      begin
	let itypeval_of_expn = Composite_types.is_integer_type t
	in 
	match  itypeval_of_expn with
            Some(_) ->  get_ivar_from_exp e
	  | None -> raise (Debug_info ("Casting subexpression into a non integer type"))
      end

    
    | _ ->  raise Contains_no_ivar

    (* get_li_fvar_from_exp_node *)
and get_li_floatvar_from_exp_node (expn : Cil_types.exp_node ) =
  match expn with
      Lval ( Var( p ) , off ) ->
	begin
	  Format.fprintf  debug_out "get_floatvar_from_exp_node : lval is a Var(p) \n";
	  Cil.d_lval debug_out  ( Var(p), off);
	  Format.fprintf  debug_out "\n";
	  let type_of_lval = Cil.typeOfLval ( Var( p ) , off ) in
	   Format.fprintf  debug_out "get_floatvar_from_exp_node : lval has type : \n %!";
	  Cil.d_type debug_out type_of_lval;
	   
	 
	 
	  let itypeval_of_expn = Composite_types.is_float_type type_of_lval 
	  in
	  match itypeval_of_expn, off with
	      (Some(_), NoOffset) -> (*We have a float or an alias for type float  here*)
		LiFVar(Unprimed,LiFloatVar(p.vname))
		  
	    | (Some(_), Field(finfo,offs)) ->
	      begin
		let vname = get_subfield_name "" finfo offs in
		let vname = p.vname^"."^vname in
		LiFVar(Unprimed,LiFloatVar(vname))
	      end

	    | (Some(_),Index(_,_)) ->
		  (* In this case, an access to an element of an array
		     is performed.
		  *)
	      begin	
		match p.vtype with 
		    TArray(tinfo,_,_,_)->
		      begin
			let index_access = get_array_index off [] in
			let dim_of_tabs = array_dim p.vtype [] in
			let c_array =  LiTab(Some(p.vname),dim_of_tabs,tinfo) in
			LiElemOfCTab(index_access,c_array)
		      end
		  | _ -> 
		    raise (Debug_info ("In get_fvar_from_exp : The expression is not an Array, as it was expected, crash"))
	      end
		      
	    |  (None,_)-> 
	      raise (Debug_info (" In get_fvar_from_exp : The expression has not an integer type, whereas I was expecting some integer value \n"))
	end

    | Lval(Mem(e), off ) ->
      begin
	Format.printf "get_floatvar_from_exp_node : lval is a Mem(e) \n";
	let type_of_expn = Cil.typeOfLval (Mem(e),off) in
	let ftypeval_of_expn = Composite_types.is_float_type type_of_expn 
	in
	match ftypeval_of_expn, off with
	    (Some(_),NoOffset) ->
	      Format.fprintf Ast_goodies.debug_out 
		"\n Star of a pvar which value is an int \n%!" ;
	      Cil.d_lval Ast_goodies.debug_out (Mem(e),off);
	      
	      let pointer_var = Ast_goodies.get_pvar_from_exp e in
	      
	      let vtype_e = Cil.typeOf e in
	      let param_c_pvar =  lipvar_of_pvar pointer_var vtype_e in
	      LiIntStarOfPtr(param_c_pvar,type_of_expn) (* Not necessary
							restricted to 
							integer values.*)
		
	 | (Some(_), Field(finfo,offs)) -> 
	   let pointer_pvar =  Ast_goodies.get_pvar_from_exp_node (Lval(Mem(e), off )) in
	   let varname = Ssl.get_name_of_ptvar pointer_pvar in
	   Format.printf "%s \n" varname;
	   LiFVar(Unprimed,LiFloatVar(varname))
	     
	     
	 | (_,Index(_,_)) -> Format.printf "Some index \n"; 
	   raise (Debug_info ("In get_pvar_from_exp_node : I don't handle
  array indexes here and there is no reason why I should do it here.\n"))
	 | (_,_) -> raise (Debug_info ("Lost in get_ivar_from_exp_node \n"))
      end
	 

    

    | CastE(t,e) ->
      begin
	let ftypeval_of_expn = Composite_types.is_float_type t
	in 
	match  ftypeval_of_expn with
            Some(_) ->  get_fvar_from_exp e
	  | None -> raise (Debug_info ("Casting subexpression into a non integer type"))
      end

    
    | _ ->  raise Contains_no_fvar
	  
and  get_ivar_from_exp (expr : Cil_types.exp ) =
  get_li_intvar_from_exp_node expr.enode
	       
and get_fvar_from_exp (expr : Cil_types.exp ) =
  get_li_floatvar_from_exp_node expr.enode


	
	
     


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
    | LiBScal( exp ) -> LiBEq ( exp, LiConst(LiIConst(My_bigint.zero) ))
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
    | LiConst(LiIConst(i)) -> (My_bigint.to_string i )
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

    | LiElemOfCTab(index,carray) -> 
	(c_tab_to_string carray)^"["^(pprint_accessed_elem "" index)^"]"

and ptrexp_to_str ( cptr : c_ptrexp ) =
  match cptr with 
    
      LiPVar ( Primed , LiIntPtr ( vname), _ ) ->
	vname^"'"

	  
    | LiBaseAddrOfArray	(index_list,cptr)-> 
      Format.sprintf "Address  of index %s of %s" 
	(pprint_accessed_elem "" index_list)
	( c_tab_to_string cptr)

    
    | LiPVar ( Unprimed , LiIntPtr ( vname ), _) ->
      vname

    |  LiAddrOfScal (e , _) -> "(Addr of TINT)"^(scal_to_string e)

    | LiPlusPI ( ptr_in , offset, _ ) ->
      ( ptrexp_to_str  ptr_in )^"["^(scal_to_string offset)^"]"
    
    | LiIndexPI ( ptr_in , offset , _ ) ->
       ( ptrexp_to_str  ptr_in )^"["^(scal_to_string offset)^"]"
    
    | LiMinusPI (ptr_in , offset , _ ) ->
      ( ptrexp_to_str  ptr_in )^"["^(scal_to_string offset)^"]"
  
(* Used to print the dimention of arrays as well as the accessed
element, or based addresses, use the two function defined below.*)
and pprint_size_tab (prefix : string) (l : (c_scal option) list) =
  let pprint_folder strarg elem =
    match elem with 
	Some(size)-> strarg^(Format.sprintf "[%s]" (scal_to_string size ))
      | None -> strarg^"[]"
  in 
  List.fold_left pprint_folder  prefix  l
 
and pprint_accessed_elem (prefix : string)(l : c_scal  list ) =
  let pprint_folder strarg elem =
    strarg^(Format.sprintf "[%s]" (scal_to_string elem ))
  in 
  List.fold_left pprint_folder  prefix  l

and c_tab_to_string tab =
  let pprint_tabname name =
    match name with
	Some(ptitnom)-> ptitnom
      | None -> "Anonymous"
(*  in
  List.fold_left pprint_folder sizelem prefix *)
  in
  match tab with 
      LiTab(name,size_list,typ) ->
	let prefix = pprint_tabname name in
	let acced_index =  pprint_size_tab prefix size_list in
	acced_index

	  

  
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

