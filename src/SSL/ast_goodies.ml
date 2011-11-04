open Cil_types
(*open Option *)

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
      tinfo.torig_name

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
	  CInt64(i,_,_) -> Format.sprintf "%d" (Int64.to_int i)
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
