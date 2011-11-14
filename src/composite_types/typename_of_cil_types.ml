
open Cil_types
open Cil
open Composite_type_types

	  
let typename_of_comp_infos ( cinfo : Cil_types.compinfo ) =
  if cinfo.cstruct then
    cinfo.cname
  else
    cinfo.cname



let rec string_typename_of_ciltype (ciltype : Cil_types.typ ) =
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
	| TPtr(t,_) -> "("^(string_typename_of_ciltype t)^")*" 
	| TVoid(_) -> "Void"
	| TArray ( t , _,_,_) ->
	  "[Array : " ^(string_typename_of_ciltype t)^"]" 
	    
	| TComp(cinfo,_,_)-> typename_of_comp_infos cinfo
	| TNamed(type_info,_) -> type_info.torig_name
	| TEnum (enum,_) ->  raise  Cant_compute_ciltype_name
	| TFun( t,_,_,_) -> "Function of return type : "^(string_typename_of_ciltype t)
	| _ ->  raise  Cant_compute_ciltype_name
	  
 
  
and string_typename_of_cilconstant (c : Cil_types.constant ) =
  match c with 
      
      CInt64(i,_,_) -> "int"
    | CStr(s) -> "string"
    | CEnum(e) -> "Enumeration"
    | _ -> raise  Cant_compute_ciltype_name
     
	  
let typename_of_ciltype ( t : Cil_types.typ ) =
  let name = string_typename_of_ciltype t in
  CTypeName(name)
  
