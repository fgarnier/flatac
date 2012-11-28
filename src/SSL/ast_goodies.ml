(****************************************************************

 This  files contains pretty printers and basic funtion that 
aims at extracting useful datas from Cil structures, Ast and 
datatypes. Its purpose consists in offering a debugging support.

More suitable functions might already be defined and provided
with the Frama-C distribution.

Only use those functions after checking that no corresponding
function have been provided with you current frama-c issue.

 *** This file might be removed from the major Flata-C plugin
distribution. ***

For questions and comment, write to florent dot garnier a/t 
verimad \dot\fr. 

*****************************************************************)


open My_bigint
open Cil_types
open Ssl_types
open Ssl_types.SSL_lex
open Lexing


exception Debug_info of string
exception Not_an_array_offset
exception  Not_Ablock
exception No_2_successors_for_if_successor of int
exception Less_than_two_elem_in_this_list
exception Bothparameter_are_None_option







  
let rec pprint_fist_elem_of_block out b =
  match b.bstmts with
      hd::_ -> pprint_first_elem_of_stmtkind  out hd.skind
    | [] -> ()
and pprint_first_elem_of_stmtkind out s =
  match s with 
      Instr(ins) -> Format.fprintf out "";Cil.d_instr out ins
    | Goto(stmt,_) -> Format.fprintf out "goto";pprint_statement_head out !stmt;Format.fprintf out "\n"
    | If(expr,_,_,_) -> Format.fprintf out "if (";(Cil.d_exp out expr); Format.fprintf out " )  \n" 
    | Loop(_,bin,_,_,_)-> Format.fprintf out "";pprint_fist_elem_of_block out bin;Format.fprintf out ""
    | Switch(exp,_,_,_)-> Format.fprintf out "switch(";Cil.d_exp out exp ; Format.fprintf out ")\n"
    | Return(exp,_) -> 
      begin
	match exp with
	    None -> Format.fprintf out "return;"
	  | Some(exp) ->
	    begin
	      Format.fprintf out "return "; 
	      Cil.d_exp out exp;
	      Format.fprintf out "\n"; 
	    end
      end
    | Break(_) -> Format.fprintf out "break();"
    | Block(b) -> Format.fprintf out ""; pprint_fist_elem_of_block out b; Format.fprintf out ""
    | Continue (_) -> Format.fprintf out "continue();"
    |UnspecifiedSequence(_) -> Format.fprintf out "Unspecified sequence \n"
    | TryFinally(_,_,_) | TryExcept(_,_,_,_) ->  Format.fprintf out "MsVC try finally or try except constructionns  \n"
and pprint_statement_head out s =
 pprint_first_elem_of_stmtkind  out s.skind



type ast_li_ptr_field = AstGLiIntStarOfPtrField of string * string (*s->ival*)
			| AstGLiPtrStarOfPtrField of string * string (*s->ptr*)
			| AstGLiPtrOfField of string * string (*s.ptr*)
			| AstGliIntOfField of string * string (*s.ival*)
			| AstGLiPVar of string  (*ptr*)
			| AstGLiIntVar of string (* ival*)
			| AstGLiStarOfPVar of string (* *ptr *)
			

(*Does the lvalue given as paramater points to the value of
a structure field or a structure given at some address ? *)

let is_lval_of_mem_access ((lv,off) : Cil_types.lval) =
  match (lv,off) with
      (Mem(e),_) ->
	true
    | _ -> false





let is_exp_array ( e : Cil_types.exp ) =
  let tt = Cil.typeOf e in
  match tt with
      TArray(_,_,_,_) -> true
    | _ -> false


let rec base_type_of_muldim_array_type ( t : Cil_types.typ ) =
  match t with
      TArray(tinfo,_,_,_)->
	base_type_of_muldim_array_type tinfo 
    | t -> t


let is_expnode_an_array (enode : Cil_types.exp_node ) =
  let e = Cil.dummy_exp enode in
  is_exp_array  e 
  
(*Returns the name of a SSL pointer variable*)
let string_of_ptvar (pvar : ptvar) =
  match pvar with
      PVar(vname) -> vname

let name_of_non_assigned_ret_val () =
  "__nts__naffected_ret_val_"

let debug_out =
  Format.formatter_of_out_channel stdout
    
let pprint_lexing_infos (loc : Cil_types.location) =
  match loc with
      (posg , posd ) ->
	let filename = posg.pos_fname in
	let begin_line_numb =  posg.pos_lnum in
	let begin_zone_raw_number = posg.pos_cnum - posg.pos_bol in
	let end_line_numb =  posd.pos_lnum in
	let end_zone_raw_number = posd.pos_cnum - posd.pos_bol in
	(Format.sprintf "In file : %s, from line %d col %d to line %d col %d. /n" filename begin_line_numb begin_zone_raw_number end_line_numb end_zone_raw_number)
	

(** Get the fists statement of block. May raise an Empty exception. *)
let first_stmt_of_block (b : Cil_types.block) =
  List.hd b.bstmts
 




let get_two_first_elem_of_list l =
    match l with
	e::l' ->
	  begin
	    match l' with
		e'::r -> (e,e')
	      | [] -> raise Less_than_two_elem_in_this_list
	  end
      | [] -> raise Less_than_two_elem_in_this_list


let get_if_then_first_block_stmts (if_stmt : Cil_types.stmt)
    (b_yes :  Cil_types.block ) 
    (b_no : Cil_types.block ) =
  match b_yes.bstmts , b_no.bstmts with
      ([],[]) -> (None,None)
    | (a::_,[]) -> 
      begin
	(*let a = List.hd if_stmt.succs in*)
	(Some(a),None)
      end
    | (a::_,b::_) ->
      begin
	(*let a=List.hd if_stmt.succs in
	let b =List.nth  if_stmt.succs 1 in*)
      (Some(a),Some(b))
      end
    | ([],b::_) ->
      (*let b = List.nth if_stmt.succs 1 in*)
      (None,Some(b))
      

(* You need to ensure that at least one of the tow parameter
  'a option matches with Some('a)
*)
let get_some_from_option_pair a b =
  match a with
      Some(a') -> a'
    | None -> 
      begin
	match b with 
	    Some(b') -> b'
	  | None -> raise Bothparameter_are_None_option
      end


(*
let get_if_else_successor (if_statement : Cil_types.stmt ) =
  match  if_statement.skind with
      If(_,_,_,_) ->
	begin
	  let num_succs = List.length if_statement.succs in
	  if num_succs != 2
	  then
	    raise No_2_successors_for_if_successor (num_succs)
	  else
	    
	    
*)
	  


let loc_of_instr (inst:Cil_types.instr) =
  match inst with 
      Set ( _ , _ , loc ) -> loc
    | Call ( _ , _ , _ , loc ) -> loc 
    | Asm ( _ , _ , _ , _ , _ , loc ) -> loc
    | Skip ( loc ) -> loc
    | Code_annot ( _ , loc ) -> loc
      
      
(* This function is only here to provide some function that takes as
input a statement and that returns the location. This function 
was provided in the Ast.info module, but vanished during the migration
from Boron to Carbon ... *)
 
let loc_stmt_opt ( stmt : Cil_types.stmt ) =  
  match stmt.skind with 
      Instr( inst ) -> Some ( loc_of_instr inst )
    | Return( _ , loc ) -> Some ( loc )
    | Break ( loc ) -> Some ( loc )
    | If ( _ , _ , _ , loc ) -> Some (loc)
    | Switch ( _ , _, _, loc ) -> Some ( loc )
    | Loop ( _ , _ ,loc, _ , _ ) -> Some ( loc )
    | TryFinally (_ , _ , loc ) -> Some ( loc )
    | TryExcept ( _ , _ , _, loc ) -> Some ( loc )
    | _ -> None (* Default case *)
    (*	| Block ( _ ) -> None
	| UnspecifiedSequence ( _ )
    *)

let pprint_skind_basic_infos ( skind : Cil_types.stmtkind ) =
  match skind with
    Instr( inst ) -> "Some instruction \n"
    | Return( _ , loc ) -> "Return to"^( pprint_lexing_infos loc)
    | Break ( loc ) -> "Break to"^ ( pprint_lexing_infos loc )
    | If ( _ , _ , _ , loc ) -> "If" ^(pprint_lexing_infos loc)
    | Switch ( _ , _, _, loc ) -> "Switch to " ^(pprint_lexing_infos loc )
    | Loop ( _ , _ ,loc, _ , _ ) -> "Loop "^ ( pprint_lexing_infos loc )
    | TryFinally (_ , _ , loc ) -> "TryFinally" ^(pprint_lexing_infos  loc )
    | TryExcept ( _ , _ , _, loc ) -> "TryExcept "^(pprint_lexing_infos loc )
    | Block(_) -> "Block \n"
    | UnspecifiedSequence (_) -> "Unspecified sequence \n"
    | _ -> "pprint_skind_basic_infos : Don't know \n" (* Default case *)

 

let pprint_unop_op ( u : Cil_types.unop ) =
  match u with 
      Neg -> "-"
    | BNot -> "~"
    | LNot -> "!"


let pprint_binop_op (b : Cil_types.binop) =
  match b with
    | 	PlusA  -> "PlusA"	(*	arithmetic +	*)
    | 	PlusPI -> "PlusPI"	(*	pointer + integer	*)
    | 	IndexPI -> "IndexPI" 	(*	pointer + integer but only when it arises from an expression e[i] when e is a pointer and not an array. This is semantically the same as PlusPI but CCured uses this as a hint that the integer is probably positive.	*)
    | 	MinusA -> 	"MinusA" 	(*	arithmetic -	*)
    | 	MinusPI -> "MinusPI"	(*	pointer - integer	*)
    | 	MinusPP -> "MinusPP" 	(*	pointer - pointer	*)
    | 	Mult -> "Mult"
    | 	Div  -> "Div"	(*	/	*)
    | 	Mod  ->	"Mod" 	(*	%	*)
    | 	Shiftlt -> 	"Shiftlt"	(*	shift left	*)
    | 	Shiftrt -> "Shiftrt" 	(*	shift right	*)
    | 	Lt  -> "Lt"	(*	< (arithmetic comparison)	*)
    | 	Gt -> "Gt" 	(*	> (arithmetic comparison)	*)
    | 	Le -> "Le" 	(*	<= (arithmetic comparison)	*)
    | 	Ge -> "Ge"	(*	>= (arithmetic comparison)	*)
    | 	Eq -> "Eq"	(*	== (arithmetic comparison)	*)
    | 	Ne -> "Ne"	(*	!= (arithmetic comparison)	*)
    | 	BAnd -> "BAnd"	(*	bitwise and	*)
    | 	BXor -> "BXor"	(*	exclusive-or	*)
    | 	BOr -> "BOr"	(*	inclusive-or	*)
    | 	LAnd -> "LAnd" 	(*	logical and. Unlike other expressions this one does not always evaluate both operands. If you want to use these, you must set Cil.useLogicalOperators.	*)
    | 	LOr -> "LOr"
      
	  
	  


    
      
let pprint_comp_infos ( cinfo : Cil_types.compinfo ) =
  if cinfo.cstruct then
    "struct "^cinfo.cname
  else
    "union "^cinfo.cname

let rec pprint_enum_item (e : Cil_types.enumitem ) =
  "Enumitem : "^e.einame^" value : "^(pprint_cil_exp e.eival)
    

and pprint_field_info (finfo : Cil_types.fieldinfo) =
  "fieldname : "^finfo.fname^":"^(pprint_ciltypes finfo.ftype)
      
and pprint_offset (off : Cil_types.offset ) =
  match off with
      NoOffset -> "no_offset"
    | Field( finfo , foff ) ->
      begin
	finfo.forig_name^"."^finfo.fname^"Offset_info:["^(pprint_offset foff)^"]"
      end
    | Index (e , ioff) ->
      begin
	"Index of ["^(pprint_cil_exp e)^","^(pprint_offset ioff)^"]"
      end

	    
and pprint_type_infos ( tinfo : Cil_types.typeinfo) =
  "Typedef name :"^tinfo.torig_name^" Underlying type :"^(pprint_ciltypes tinfo.ttype)

and pprint_enum_infos ( enum : Cil_types.enuminfo) =
  "Enumeration : "^enum.eorig_name^" Ename : "^enum.ename
	
and  pprint_ciltypes (ciltype : Cil_types.typ ) =
  match ciltype with
      TInt(IBool,_) -> "bool"
    | TInt(IChar,_) ->  "char"
    | TInt(ISChar,_) -> "signed_char"
    | TInt(IUChar,_) -> "unsigned_char"	
    | TInt(IInt,_) -> "int"
    | TInt(IUInt,_) -> "unsigned_int"
    | TInt(IShort,_) -> "short"
    | TInt(IUShort,_) -> "unsigned_short"
    | TInt(ILong,_) -> "long"
    | TInt(IULong,_) -> "unsigned_long"
    | TInt(ILongLong,_) -> "long_long"
    | TInt(IULongLong,_) -> "unsigned_long_long"	
    | TFloat(FFloat,_) -> "float"
    | TFloat(FDouble,_) -> "double"
    | TFloat(FLongDouble,_) -> "long_double"
    | TPtr(t,_) -> "("^(pprint_ciltypes t)^")*" 
    | TVoid(_) -> "Void"
    | TArray ( t , _,_,_) ->
      "[Array : " ^(pprint_ciltypes t)^"]" 
	    
    | TComp(cinfo,_,_)-> pprint_comp_infos cinfo
    | TNamed(type_info,_) -> "TNamed type "^(pprint_type_infos type_info)
    | TEnum (enum,_) -> pprint_enum_infos enum
    | TFun( t,_,_,_) -> "Function of return type : "^(pprint_ciltypes t)
    | _ ->  "Non numerical type"
	  
and pprint_cil_constant (c : Cil_types.constant ) =
  match c with 
      CInt64(i,_,_) -> 
	let s = My_bigint.M.to_string i in 
	Format.sprintf "%s" s

    | CStr(s) -> s
    | CEnum(e) -> pprint_enum_item e
    | _ -> "Some non integer and non string constant"
	  
	  
and pprint_cil_exp ( e : Cil_types.exp ) =
  match e.enode with 
      Lval( Var(v) , NoOffset ) -> Format.sprintf "Var %s : %s" v.vname (pprint_ciltypes v.vtype)
    | Lval( Var(v) , ( Index(_,_) as idx))  -> Format.sprintf "Var %s %s :  %s" v.vname (pprint_offset idx) (pprint_ciltypes v.vtype)

    | Lval( Var(v) , _)  -> Format.sprintf "Var %s  :  %s" v.vname (pprint_ciltypes v.vtype)

    |  Lval ( Mem (e' ) , _) -> Format.sprintf "Mem [ %s ]" (pprint_cil_exp e')
    | SizeOfStr (str) -> "sizeof("^str^")"
    | SizeOfE ( e') -> "sizeof("^(pprint_cil_exp e')^")"
    | SizeOf ( t )-> "sizeof("^(pprint_ciltypes t)^")"
    | CastE( t , expr ) -> "( CAST "^(pprint_ciltypes t)^","^(pprint_cil_exp expr)^")"
    | BinOp( bop , eg , ed, t) ->  (pprint_binop_op bop)^"("^ (pprint_cil_exp eg ) ^ ","^(pprint_cil_exp ed )^") : "^(pprint_ciltypes t)
	    
    | UnOp(u , expr , t  ) -> (pprint_unop_op u)^( pprint_cil_exp expr)^" : "^(pprint_ciltypes t)
    | Const(c) -> pprint_cil_constant c 
    | AddrOf(l) ->
      begin
	match l with 
	  | (Var(vv),off_v) -> "AddressOf["^(vv.vname)^"@offset :"^(pprint_offset off_v)^"]"
	    
	  | (Mem(e), offv) -> "AddressOf[Mem["^(pprint_cil_exp e)^"@offset"^(pprint_offset offv)^"]"
      end
    
    | StartOf(l) -> Format.sprintf "StartOf[%s]" (pprint_cil_exp (Cil.dummy_exp( Lval(l)))) 
	

    | _ -> "Some not yet parsed expression"
	  

let rec pprint_attr_list_l_fold ( elem_left : int ref)( attr : Cil_types.attrparam )
    (s : string) =
  if (!elem_left <= 1 ) then
    begin
      
      s^( pprint_attrparam attr)
    end
  else
    begin
      elem_left := !elem_left - 1;
      s^( pprint_attrparam attr)^","
    end
 
and pprint_attrparam ( attr : Cil_types.attrparam ) =
  match attr with 
      AInt ( i ) -> Format.sprintf "%d" i
    | AStr ( s ) -> s
    | ACons (s , plist ) ->
	begin
	  let nbelem = ref (List.length plist) in 
	  s^"("^(List.fold_right (pprint_attr_list_l_fold nbelem) plist "" )^")"
	end
    | ASizeOf (t) -> "Sizeof("^(pprint_ciltypes t)^")"
    | AUnOp ( op , atp ) -> (pprint_unop_op op )^(pprint_attrparam atp)
    | ABinOp ( bop, ag , ad ) -> (pprint_binop_op bop )^"("^(pprint_attrparam ag)^","^(pprint_attrparam ad)^")"
    | AStar( atp) -> "*"^(pprint_attrparam atp)
    | _ -> "Some attributes params"



let pprint_attribute (atr : Cil_types.attribute ) =
  match atr with
      Attr ( s , atpl ) ->
	begin
	  let len = ref (List.length atpl) in
	    s^"("^( List.fold_right (pprint_attr_list_l_fold len) atpl "" )^")"
	end
    |  AttrAnnot (s ) -> "Annotation ["^s^"]"
  

let pprint_attributes ( attr :  Cil_types.attributes) =
  List.fold_right ( fun  a s -> (s^(pprint_attribute a)^";")) attr ""

let pprint_slocal_var (slocal : Cil_types.varinfo ) =
  "SLOCAL [ "^(slocal.vname)^":"^(pprint_ciltypes slocal.vtype)^" ] = "^(pprint_attributes slocal.vattr)
  
let pprint_slocal_vars ( slocals :  Cil_types.varinfo list ) =
  List.fold_right (fun vinf str -> str^(pprint_slocal_var vinf)^"\n" ) slocals ""



let rec get_subfield_name (prefix : string ) (finfo : Cil_types.fieldinfo)
    (off : Cil_types.offset) =
  let prefix =
    match prefix with
	"" -> ""
      | _ -> prefix^"."
  in
  match off with
      NoOffset -> prefix^(finfo.forig_name)
    | Field (subfieldinfo , suboffset ) ->
	begin
	  let current_path_name = prefix^(finfo.forig_name) in
	    get_subfield_name current_path_name subfieldinfo suboffset
	end
    
    | _ -> raise (Debug_info (" In get_subfield_name : I don't know how to deal with array indexes \n"))
    


(* This function is of use to get :

 (Lval(Var(v),off)) in Mem(e) where e=(int * )(b->off) *)  


let rec get_lval_under_cast (expr : Cil_types.exp ) =
  match expr.enode with 
      CastE(_,e) -> get_lval_under_cast e 
    | _ -> expr 


(** This function checks whether the argument is a pointer 
variable or a casted pointer variable. It returns a Ssl_type.PVar("vname")
if so, and raise an exception if the other case.*)
let rec get_pvar_from_exp_node (expn : Cil_types.exp_node ) =
  match expn with
      Lval ( Var( p ) , off ) ->
	begin
	  Format.fprintf  debug_out "get_pvar_from_exp_node : lval is a Var(p) \n";
	  Cil.d_lval debug_out  ( Var(p), off);
	  Format.fprintf  debug_out "\n";
	  let type_of_lval = Cil.typeOfLval ( Var( p ) , off ) in
	   Format.fprintf  debug_out "get_pvar_from_exp_node : lval has type : \n";
	  Cil.d_type debug_out type_of_lval;
	  Format.printf "\n";

	  match type_of_lval with 
	      TPtr(_,_) -> 
		begin
		  match off with (* If lval is a subfield of a structure*)
		      Field (finfo, suboffset) ->
			let pvar_name = get_subfield_name 
			  (p.vname) finfo suboffset in
			
			Format.printf "[get_pvar_from_exp_node]Pvar name is : %s \n" pvar_name;
			(PVar(pvar_name))
			  
		    | NoOffset -> 
		      Format.printf "No offset for pvar \n";
		      (PVar(p.vname))
		    |  _ -> raise (Debug_info (" In get_pvar_from_exp : I don't know how to deal with array indexes \n"))
		end
		
	    | _ -> 
	      raise Contains_no_pvar
	end

    | Lval(Mem(e), off ) ->
      begin
	Format.printf "get_pvar_from_exp_node : lval is a Mem(e) \n";
	get_pvar_from_mem_access expn
      end


    | CastE (TPtr (_,_), e ) ->
      begin
	Format.printf "Casting subtree type \n";
	Cil.d_exp debug_out e;
	Format.fprintf debug_out "\n %!";
	get_pvar_from_exp e
      end

    | Const ( CInt64 (i ,_,_)) -> 
      if (My_bigint.is_zero i)  then 
	raise Loc_is_nil
      else raise (Loc_is_a_constant(My_bigint.to_int64 i))
    
    | BinOp (PlusPI,e1,_,_)
    | BinOp (MinusPI,e1,_,_)
    | BinOp (IndexPI,e1,_,_)
	->
	get_pvar_from_exp e1

    | BinOp (b,_,_,_) ->
	let b = pprint_binop_op b in
	let msg = "[get_pvar_from_exp :] Don't know what
to do with Binop operator "^b in
	  raise (Debug_info(msg))

    | Info (_,_) ->raise (Debug_info("[get_pvar_from_exp :] Don't know what
to do with Info"))

    | AddrOf (_) ->  raise (Debug_info("[get_pvar_from_exp :] Don't know what to do with AddrOf"))

    | StartOf(Var(v),_) ->
      begin
	match v.vtype with 
	    TArray(_,_,_,_) -> PVar(v.vname)
	  | _ ->  raise (Debug_info ("Getting a Starting address of non array type"))
      end
    | StartOf(lv) ->
      begin
	let dummyexp = Cil.dummy_exp expn in
	Format.printf "Failed to get PVar in expression StartOf(_,_) \n :";
	Cil.d_exp debug_out dummyexp;
	
	Format.fprintf debug_out " lval inside is \n %!";
	Cil.d_lval debug_out lv;
	Format.fprintf debug_out "\n%!";
	get_pvar_from_mem_access (Lval(lv))
	(*raise Contains_no_pvar*)
      end
    | _ -> 
      begin 
	let dummyexp = Cil.dummy_exp expn in
	Format.printf "Failed to get PVar in expression  \n :";
	Cil.d_exp debug_out dummyexp;
	Format.printf " Raising exception ... \n %!";
	raise Contains_no_pvar
      end
	  
and  get_pvar_from_exp (expr : Cil_types.exp ) =
  Format.fprintf debug_out "Current expression is :\n";
  Cil.d_exp debug_out expr;
  Format.fprintf debug_out "\n%!";
  get_pvar_from_exp_node expr.enode



and get_pvar_from_array_element_access (exp : Cil_types.exp ) =
 (* if ( not (is_exp_array exp) )
  then
    begin
      let tt = Cil.typeOf exp in
      Format.printf "I was expecting an array type, I have that instead \n"; 
      Cil.d_type debug_out tt;
      Format.fprintf  debug_out " \n%!"; 
      assert false
    end
 
    else *)
    begin
      match exp.enode with
	  StartOf((Var(v),_)) -> 
	    begin
	      if ( not (is_exp_array exp) )
	      then
		begin
		  let tt = Cil.typeOf exp in
		  Format.printf "I was expecting an array type, I have that instead \n"; 
		  Cil.d_type debug_out tt;
		  Format.fprintf  debug_out " \n%!"; 
		  assert false
		end
	      else
		PVar(v.vname)
	    end
   
	| StartOf(_) ->    
	  assert false

	| Lval(Mem(e),_) ->
	  begin
	    match e.enode with
		BinOp(PlusPI,p,_,_) | BinOp(MinusPI,p,_,_) | BinOp(IndexPI,p,_,_)
		  ->
		    begin
		      get_pvar_from_array_element_access p
		    end
	      | _ -> assert false
	  end
	| Lval(Var(v),off) ->
	  if not ( match v.vtype with 
	      TArray (_,_,_,_) -> true 
	    | TPtr(TArray (_,_,_,_),_)-> true
	    | _ -> false
	  )
	  then
	    begin
	    
	      Format.printf "Var of lval has type\n"; 
	      Cil.d_type debug_out v.vtype;
	      Format.fprintf  debug_out " \n%! Lval is : ";
	      Cil.d_lval debug_out (Var(v),off);
	      Format.fprintf  debug_out " \n%!";
	      assert false
	    end
	      else
		PVar(v.vname)

	    
	| BinOp(_,_,_,_) ->
	  assert false

	| Info (_,_) -> 
	  assert false

	| Const(_) -> 
	  assert false

	| CastE(TPtr(_,_),e)
	    -> get_pvar_from_array_element_access e


	| AddrOf(_) ->
	  assert false

	
	| _ ->
	  Format.fprintf debug_out "Got that and I'm stuck \n";
	  Cil.d_exp debug_out exp;
	  Format.fprintf debug_out "Type of this expression :";
	  
	  let tt = Cil.typeOf exp in
	  Cil.d_type debug_out tt; 
	  
	  Format.fprintf debug_out " \n%!";
	 
	  assert false
    end
	
	    
	      
      
and get_pvar_from_mem_access ( expn : Cil_types.exp_node) =
  match expn with 
      Lval(Mem(e),off) ->
	begin
	  match e.enode , off with
	      (Lval(Var(v'),_),NoOffset) ->
		Format.printf "Mem(e) :*%s- \n" v'.vname ;
		PVar(v'.vname)
		  
	    | (Lval(Var(v'),_), Field(finfo,offs)) -> 
	      
	      let pointer_name = get_subfield_name "" finfo offs in
	      let pointer_name = Format.sprintf "%s->%s" v'.vname pointer_name
	      in
	      Format.printf "%s \n" pointer_name;
	      PVar(pointer_name)
		
		
	    | (_,Index(_,_)) -> Format.printf "Some index \n"; 
	      raise (Debug_info ("In get_pvar_from_exp_node : I don't handle
  array indexes here and there is no reason why I should do it here.\n"))
		
	    | (CastE(TPtr(_,_),e), NoOffset) ->
	      let exprprime = get_lval_under_cast e in
	      get_pvar_from_mem_access exprprime.enode

	    | (Lval(Mem(b),off),_) ->
	      begin
		assert false
	      end


	    | (StartOf(Var(v),_),_) ->
	      PVar(v.vname)
		
	    | (StartOf(Mem(_),_),_) ->
	      assert false

	    | (BinOp(PlusPI,p,_,_),_) | (BinOp(MinusPI,p,_,_),_) |
	      (BinOp(IndexPI,p,_,_),_)-> 
	      (* Access to an array element which type is an array*)
	      (*get_pvar_from_array_element_access p*)
	      get_pvar_from_exp p
	      (*assert false*)

		

	    |  (AddrOf( Var(v), offset ),_) ->
	      begin
		match offset with
		  (* Field(finf,off) -> 
		     begin
		     let sfield_name = get_subfield_name 
		     v.vname finf off in
		     LiDerefCVar(sfield_name,v.vtype)
		     end *)
		    
		  | Index(exp,off) ->
		    begin
		      (*let indexes = get_array_index offset [] in
		      let dim = array_dim v.vtype [] in
		      let parsed_tab = LiTab( Some(v.vname) , dim, v.vtype ) in
			LiBaseAddrOfArray(indexes,parsed_tab)*)
		      PVar(v.vname)
		    end
		      
		  | NoOffset ->
		    begin
		      match v.vtype with
			  TArray(_,_,_,_) -> 
			    begin
			      (* let dim = array_dim v.vtype [] in
				 let parsed_tab = LiTab( Some(v.vname) , dim, v.vtype ) 
				 in
				 LiBaseAddrOfArray([],parsed_tab)*)
			      PVar(v.vname)
			    end
			      
			| _->
			  begin
			    PVar(v.vname)
			  end
		    end
		      
	      end

		
	    
	    | (_,_) ->
	      Format.fprintf debug_out "I don't know what to do with :";
	      Cil.d_lval debug_out (Mem(e),off);
	      let expnode = Cil.dummy_exp expn in
	      Format.fprintf debug_out "Expression has type : \n";
	      let expnodetype = Cil.typeOf expnode in
	      Cil.d_type debug_out expnodetype;
	      Format.fprintf debug_out "\n%!";
	      raise (Debug_info ("Lost in get_pvar_from_exp_node \n"))
	end
    |_ -> raise (Debug_info ("This function is only for analysing Mem(e) lhosts in lval types"))

     
	    

let rec get_first_ptvar_from_lparam ( lparam : Cil_types.exp list ) =
  Format.printf " I am in get_first_ptvar_from lparam\n"; 
 match lparam with 
     [] -> raise No_pvar_in_param_list
   | h::l' ->
	 begin
	   match h.enode with
	       (Lval(Var(varinfo),off)) ->
		 begin
		   match varinfo.vtype with
		       TPtr(_,_) -> 
			 get_pvar_from_exp h
			 
		     | _ -> get_first_ptvar_from_lparam l'
		 end
		    | CastE(_,expr) -> 
		      begin 
			Format.printf "J'ai vu un cast dans la liste
des parametres \n" ;
			try
			  get_pvar_from_exp expr
			with
			    Contains_no_pvar -> get_first_ptvar_from_lparam l'
			      
		      end
	
       
		    | _  -> get_first_ptvar_from_lparam l' 
	 end











let max_args_numbers_of_callees (stmtkl : Cil_types.stmt list) =
  let folder max_num stmt =
    match stmt.skind with 
	Instr(Call(_,_,l,_)) -> 
	  begin
	    let curr_len = List.length l in
	    if curr_len > max_num then
	      curr_len
	    else 
	      max_num
	  end
      |_ -> max_num
  in
  List.fold_left folder 0 stmtkl



(* Does a stmt has a Default constructors inside its list of
associated labels ? *)

let is_default_label ( l : Cil_types.label ) =
  match l with
      Default(_) -> true
    | _ -> false
   


let stmt_has_default_label ( s : Cil_types.stmt ) =
  List.exists is_default_label s.labels 

(* This function returns true iff one statment
of the list contains a Default type label.

*)  



let has_default_label (stmt_succs : Cil_types.stmt list ) =
 
  let stmt_has_default_label ( s : Cil_types.stmt ) =
    List.exists is_default_label s.labels
  in
  List.exists stmt_has_default_label stmt_succs
  

