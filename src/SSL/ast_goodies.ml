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


    
let pprint_lexing_infos (loc : Cil_types.location) =
  match loc with
      (posg , posd ) ->
	let filename = posg.pos_fname in
	let begin_line_numb =  posg.pos_lnum in
	let begin_zone_raw_number = posg.pos_cnum - posg.pos_bol in
	let end_line_numb =  posd.pos_lnum in
	let end_zone_raw_number = posd.pos_cnum - posd.pos_bol in
	(Format.sprintf "In file : %s, from line %d col %d to line %d col %d. /n" filename begin_line_numb begin_zone_raw_number end_line_numb end_zone_raw_number)
	


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
	let i32 = My_bigint.M.to_int i in 
	Format.sprintf "%d"  i32

    | CStr(s) -> s
    | CEnum(e) -> pprint_enum_item e
    | _ -> "Some non integer and non string constant"
	  
	  
and pprint_cil_exp ( e : Cil_types.exp ) =
  match e.enode with 
      Lval( Var(v) , _ ) -> Format.sprintf "Var %s : %s" v.vname (pprint_ciltypes v.vtype)
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
  match off with
      NoOffset -> prefix^(finfo.forig_name)
    | Field (subfieldinfo , suboffset ) ->
	begin
	  let current_path_name = prefix^(subfieldinfo.forig_name) in
	    get_subfield_name current_path_name subfieldinfo suboffset
	end
    | _ -> raise (Debug_info (" In get_subfield_name : I don't know how to deal with array indexes \n"))
    

(** This function checks whether the argument is a pointer 
variable or a casted pointer variable. It returns a Ssl_type.PVar("vname")
if so, and raise an exception is not.*)
let rec get_pvar_from_exp_node (expn : Cil_types.exp_node ) =
  match expn with
      Lval ( Var( p ) , off ) ->
	begin
	  Format.printf "get_pvar_from_exp_node : lval is a Var(p) \n";
	  match p.vtype with 
	      TPtr(_,_) -> 
		begin
		  match off with (* If lval is a subfield of a structure*)
		      Field (finfo, suboffset) ->
			let pvar_name = get_subfield_name 
			  (p.vname) finfo suboffset in
			Format.printf "Pvar name is : %s \n" pvar_name;
			  (PVar(pvar_name))

		    | NoOffset -> 
		      Format.printf "No offset for pvar \n";
		      (PVar(p.vname))
		    |  _ -> raise (Debug_info (" In get_pvar_from_exp : I don't know how to deal with array indexes \n"))
		end
		
	    | _ -> raise Contains_no_pvar
	end

    | Lval(Mem(e), off ) ->
      Format.printf "get_pvar_from_exp_node : lval is a Mem(e) \n";
      begin
	match e.enode , off with
	    (Lval(Var(v'),_),NoOffset) ->
	      Format.printf "Mem(e) :*%s- \n" v'.vname ;
	      PVar(v'.vname)

	  | (Lval(Var(v'),_), Field(finfo,offs)) -> 
	    let pointer_name = Format.sprintf "%s->" v'.vname in
	    let pointer_name = get_subfield_name pointer_name finfo offs in
	    Format.printf "%s \n" pointer_name;
	    PVar(pointer_name)
	    
	  
	  | (_,Index(_,_)) -> Format.printf "Some index \n"; 
	    raise (Debug_info ("In get_pvar_from_exp_node : I don't handle
  array indexes here and there is no reason why I should do it here.\n"))
	  | (_,_) -> raise (Debug_info ("Lost in get_pvar_from_exp_node \n"))
      end
	 

    | CastE (TPtr (_,_), e ) ->
	get_pvar_from_exp e

    | Const ( CInt64 (i ,_,_)) -> 
      if (My_bigint.is_zero i)  then 
	raise Loc_is_nil
      else raise (Loc_is_a_constant(My_bigint.to_int64 i))
    
    | BinOp (PlusPI,e1,_,_) 
    | BinOp (MinusPI,e1,_,_)
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

    | _ ->  raise Contains_no_pvar
	  
and  get_pvar_from_exp (expr : Cil_types.exp ) =
  get_pvar_from_exp_node expr.enode

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
		(* begin
		 match param.enode with
		     Lval(Var(varinf),_) -> (PVar(varinf.vname))
		   | _ -> get_first_ptvar_from_lparam l'
		 end *)
       
		    | _  -> get_first_ptvar_from_lparam l' 
	 end
	   
(*
let get_cil_type_of_global (g : cil_types.global ) =
  match g with
      GType (tinfo , _ ) ->
	TNamed(tinfo, [])
    | GCompTag ( cinfo , _ ) ->
      TComp( cinfo, _ , _ )
    
    |  GCompTagDecl ( cinfo , _ ) ->
      TCompt(cinfo, _ , _ )
    
    | GEnumTag ( einfo , _ ) ->
      TEnum( einfo, _ )
    
    | GEnumTagDecl ( einfo , _ ) ->
      TEnum (einfo , _ )	
*)
