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



let rec pprint_ciltypes (ciltype : Cil_types.typ ) =
  match ciltype with
      TInt(IBool,_) -> "bool"
    | TInt(IChar,_) ->  "char"
    | TInt(ISChar,_) -> "sizeof_signed_char"
    | TInt(IUChar,_) -> "unsigned_char"	
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
    | TPtr(t,_) -> (pprint_ciltypes t)^"*" 
    | _ ->  "Non numerical type"



let pprint_cil_constant (c : Cil_types.constant ) =
  match c with 
      CInt64(i,_,_) -> Format.sprintf "%d" (Int64.to_int i)
    | CStr(s) -> s
    | _ -> "Some non integer and non string constant"


let rec pprint_cil_exp ( e : Cil_types.exp ) =
  match e.enode with 
      Lval( Var(v) , _ ) -> Format.sprintf "Var %s : %s" v.vname (pprint_ciltypes v.vtype)
    |  Lval ( Mem (e' ) , _) -> Format.sprintf "Mem [ %s ]" (pprint_cil_exp e')
    | SizeOfStr (str) -> "sizeof("^str^")"
    | SizeOfE ( e') -> "sizeof("^(pprint_cil_exp e')^")"
    | SizeOf ( t )-> "sizeof("^(pprint_ciltypes t)^")"
    | CastE( t , expr ) -> "("^(pprint_ciltypes t)^")"^(pprint_cil_exp expr)
    | BinOp( bop , eg , ed, t) ->  (pprint_binop_op bop)^"("^ (pprint_cil_exp eg ) ^ ","^(pprint_cil_exp ed )^") : "^(pprint_ciltypes t)
      
    | UnOp(u , expr , t  ) -> (pprint_unop_op u)^( pprint_cil_exp expr)^" : "^(pprint_ciltypes t)
    | Const(c) -> pprint_cil_constant c 

    | _ -> "Some expr"
	  
