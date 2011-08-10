(**********************************************************************)
(* 
This file contains the functions that translates the arithmetics
expression from CIL into counter automata based one. The focus is maid
on pointer arithmetic, especialy for int * pointer arithmetics

Questions and/or remarks : mail to florent dot garnier At imag dot fr
*) 

open Cil_types
open Intermediate_language
open Ssl_types
open Ssl
open SSL_lex
open Global_mem
open Validity
open Cil_types
open Nts_types


(** Needs to be duely completed.*)
let interpret_ciltypes_size (ciltype : Cil_types.typ ) =
  match ciltype with
      _ -> CntSymCst ("It's a sizeof")



(** This function aims at computing the name of the couter var name
associated to the offset of a pointer variable*)
let offset_cnt_name ( ptvar : c_ptrexp ) =  
  match ptvar with
      LiPVar(_,LiIntPtr(vname),_) -> CntVar ( "offset("^vname^")" )
    | _ -> raise Not_LiPVar
  
let int_var_cnt_name ( cexpr : c_scal) =
   match cexpr with
      LiVar(_,LiIntVar(vname)) -> CntVar( "intvar("^vname^")" )
    | _ -> raise Not_LiVar

let rec interpret_c_scal_to_cnt  ( sslf : ssl_formula )( scalexp : c_scal ) =
  match scalexp with 
      LiVar(_) -> int_var_cnt_name scalexp
    | LiConst(LiIConst(i)) ->  CntCst(i)
    | LiProd ( l , r ) ->
	begin
	  let lg = interpret_c_scal_to_cnt sslf l in
	  let ld = interpret_c_scal_to_cnt sslf r in
	    CntProd ( lg , ld )
	end
    |  LiSum  ( l , r ) ->
	 begin
	   let lg = interpret_c_scal_to_cnt sslf l in
	   let ld = interpret_c_scal_to_cnt sslf r in
	     CntProd ( lg , ld )
	 end
    | LiMinus ( l , r ) ->
	begin
	  let lg = interpret_c_scal_to_cnt sslf l in
	  let ld = interpret_c_scal_to_cnt sslf r in
	    CntMinus ( lg , ld )
	end
	  
    | LiMod ( l , r ) ->
	begin
	  let lg = interpret_c_scal_to_cnt sslf l in
	  let ld = interpret_c_scal_to_cnt sslf r in
	    CntMod ( lg , ld )
	end
	  
    | LiUnMin( t ) -> 
	let tin = interpret_c_scal_to_cnt sslf t in
	  CntUnMin( tin)
	    
    | LiMinusPP ( l , r , optype) ->
	let basel = base_ptrexp sslf l in
	let baser = base_ptrexp sslf r in
	  if basel = baser then
	    begin
	      let lg = interpret_c_ptrexp_to_cnt sslf l in
	      let ld = interpret_c_ptrexp_to_cnt sslf r in
		CntMinus ( lg , ld )
	    end
	  else CntInvalidExp
    
    | LiSymConst ( LiSymIConst(const_name)) -> CntSymCst(const_name)
	    
and interpret_c_ptrexp_to_cnt (sslf : ssl_formula )( ptrexp : c_ptrexp ) =
  match ptrexp with 
      LiPVar( _ , LiIntPtr(vname), _) -> CntVar(vname)
    | LiPlusPI ( cptrexp , scalv, optype ) -> 
	begin
	  let ll = interpret_c_ptrexp_to_cnt sslf cptrexp in
	  let lr = interpret_c_scal_to_cnt sslf scalv in
	  let sizeof_ptr_type = interpret_ciltypes_size optype in
	  let lr = CntProd( lr , sizeof_ptr_type ) in
	    CntSum(ll, lr)
	end
    | LiMinusPI ( cptrexp , scalv , optype ) ->
	begin
	  let ll = interpret_c_ptrexp_to_cnt sslf cptrexp in
	  let lr = interpret_c_scal_to_cnt sslf scalv in
	  let sizeof_ptr_type = interpret_ciltypes_size optype in
	    CntDiv(CntMinus(ll,lr),sizeof_ptr_type)
	end
    | LiIndexPI ( cptrexp , scalv, optype ) ->
	begin
	  let ll = interpret_c_ptrexp_to_cnt sslf cptrexp in
	  let lr = interpret_c_scal_to_cnt sslf scalv in
	  let  sizeof_ptr_type = interpret_ciltypes_size optype in
	  let lr = CntProd ( lr , sizeof_ptr_type ) in
	    CntSum(ll,lr) 
	end


(** Returns the type of the pointer expression, that is
basically the type of the varname. Returns the type of the
innermost pointer variable the expression tree.

One need to check that this procedure is sufficient, but
it may not be.
*)
let rec type_of_ptrexp ptrexp =
   match ptrexp with 
      LiPVar( _ , LiIntPtr(vname), vtype) -> vtype
    | LiPlusPI ( cptrexp , _ ,_) -> 
	type_of_ptrexp cptrexp
    | LiMinusPI ( cptrexp , scalv,_ ) ->
	type_of_ptrexp cptrexp
    | LiIndexPI ( cptrexp , scalv,_ ) ->
	type_of_ptrexp cptrexp


let rec c_bool_to_cnt_bool (sslf : ssl_formula)(cbool : c_bool ) = 
  match cbool with 
      LiBNot (b) -> 
	let cnt_arg = c_bool_to_cnt_bool sslf b in 
	  CntNot ( cnt_arg )
    | LiBAnd ( bg , bd ) ->
	begin
	  let bgarg = c_bool_to_cnt_bool sslf bg in
	  let bdarg = c_bool_to_cnt_bool sslf bd in
	    CntBAnd ( bgarg , bdarg ) 
	end
    
    | LiBOr ( bg , bd ) ->
	begin
	  let bgarg = c_bool_to_cnt_bool sslf bg in
	  let bdarg = c_bool_to_cnt_bool sslf bd in
	    CntBOr ( bgarg , bdarg ) 
	end

    | LiBTrue -> CntBTrue
    | LiBFalse -> CntBFalse
	
    | LiBEq ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	     CntBool ( CntEq , argg , argd )
	end
  

    | LiBNeq ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	     CntBool ( CntNeq , argg , argd )
	end

    | LiBLt ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	     CntBool ( CntLt , argg , argd )
	end
	  
    | LiBLeq ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	     CntBool ( CntLeq , argg , argd )
	end

    | LiBGt ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	     CntBool ( CntGt , argg , argd )
	end 
    
    | LiBGeq ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	     CntBool ( CntGeq , argg , argd )
	end
    
    |LiBScal (cscal) ->
       begin
	 let arg = interpret_c_scal_to_cnt sslf cscal in
	   CntBool (CntEq , arg , (CntCst(0)))
       end

    (* Pointer comparisons match cases start here*)
    | LiBPtrEq ( cptrg ,  cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	     CntBool (CntEq , argg , argd )
	end
	     

    | LiBPtrNeq ( cptrg , cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	     CntBool (CntNeq , argg , argd )
	end

    
    | LiBPtrGeq ( cptrg , cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	     CntBool ( CntGeq , argg , argd )
	end
	
	
    | LiBPtrGt ( cptrg , cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	     CntBool ( CntGt , argg , argd )
	end

    | LiBPtrLt ( cptrg , cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	    CntBool ( CntLt , argg , argd )
	end	  
	  
    | LiBPtrLeq ( cptrg , cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	    CntBool (CntLeq , argg , argd )
	end	  
	  
    
