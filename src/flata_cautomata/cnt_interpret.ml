(**********************************************************************)
(* 
This file contains the functions that translates the arithmetics
expression from the intermediate language into counter automata arithmetic
expressions. The focus is maid
on pointer arithmetic.

Questions and/or remarks : mail to florent dot garnier At imag dot fr

The value that are translated into counter automata arithmetics are
expression that have type :

_ int
and Type *, where Type ranges on char, int, long, float, double etc ...
*) 

open Cil_types
open Intermediate_language
open Ssl_types
open Ssl
open SSL_lex
open Global_mem
open Validity_types
open Validity
open Cil_types
open Nts_types

exception Unhandled_valuetype_in_interpretciltypesize
exception CilTypeHasNoEquivalentNtsType of Cil_types.typ



let dereferenced_name_of_varname (v : string ) =
  "addr_off_"^v




let ciltype_2_ntstype (t : Cil_types.typ) =
  match t with 
      TInt(_,_) ->  NtsIntType
    | TFloat(_,_) ->  NtsRealType
    | _ -> raise (CilTypeHasNoEquivalentNtsType(t))

(** Creates the sizeof variable from the name stored
in tinfo.tname.*)
let sizeof_cil_tinfo ( tinfo : Cil_types.typeinfo ) =
  let typename = "sizeof_"^tinfo.tname in
    CntSymCst( typename )

(** Needs to be duely completed.
Given a type t, this function return the sizeof of t,
or for type t*, it returns the sizeof of t.
*)
let rec interpret_ciltypes_size (ciltype : Cil_types.typ ) =
  match ciltype with
      TInt(IBool,_) -> CntSymCst ("sizeof_bool")
    | TInt(IChar,_) -> CntSymCst ("1")
    | TInt(ISChar,_) -> CntSymCst ("1")
    | TInt(IUChar,_) -> CntSymCst ("1")	
    | TInt(IInt,_) -> CntSymCst ("4")
    | TInt(IUInt,_) -> CntSymCst ("4")
    | TInt(IShort,_) -> CntSymCst ("2")
    | TInt(IUShort,_) -> CntSymCst ("2")
    | TInt(ILong,_) -> CntSymCst ("4")
    | TInt(IULong,_) -> CntSymCst ("4")
    | TInt(ILongLong,_) -> CntSymCst ("8")
    | TInt(IULongLong,_) -> CntSymCst ("8")	
    | TFloat(FFloat,_) -> CntSymCst ("4")
    | TFloat(FDouble,_) -> CntSymCst ("8")
    | TFloat(FLongDouble,_) -> CntSymCst ("")
    | TVoid([]) -> CntSymCst ("1") 
    | TNamed(tinfo, _ ) -> sizeof_cil_tinfo tinfo 
    | TPtr(TVoid([]), _) ->  CntSymCst ("4")
    | TPtr(t,_) -> interpret_ciltypes_size t (* Won't work
					     for t** ...*)

    | _ -> raise  Unhandled_valuetype_in_interpretciltypesize

let offset_cnt_of_pvar (pvar : ptvar) =
  match pvar with 
      PVar(vname) -> CntVar ( NtsIVar("offset__"^vname^"_") )


(** Same as above, but more conveninant to express CntAffectation,
in the sense it spare the programmer to add another cumbersome
match structure to get the embeded NtsIVar(name).
*)
let offset_ntsivar_of_pvar (pvar : ptvar ) =
   match pvar with 
      PVar(vname) -> NtsIVar("offset__"^vname^"_") 

   
(** This function aims at computing the name of the counter var name
associated to the offset of a pointer variable*)
let offset_cnt_name ( ptvar : c_ptrexp ) =  
  match ptvar with
      LiPVar(_,LiIntPtr(vname),_) -> CntVar ( NtsIVar("offset__"^vname^"_") )
    | _ -> raise Not_LiPVar
  

let int_var_cnt_name ( cexpr : c_scal) =
   match cexpr with
      LiVar(_,LiIntVar(vname)) -> CntVar(NtsIVar( vname ))
    | _ -> raise Not_LiVar


(** This function returns a counter associated to base address of a location
variable. This function is used to model a memory allocation. See thechnical
report and the section dedicated to malloc. *)
let lvarbase_cnt_name ( lvar : locvar ) =
  match lvar with
    | LVar (vname) -> CntVar(NtsIVar(vname^"_base_addr"))

(** Same as above, but for the size of a segment allocation.*)
let lvarsize_cnt_name ( lvar : locvar ) =
  match lvar with 
    | LVar (vname ) -> CntVar(NtsIVar(vname^"_size_of_segment"))



(** Interprets a scalar expression into the nts formalism, w.r.t. the
current context expressed as a ssl formula.*)
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
	     CntSum ( lg , ld )
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
    
    | LiSymConst ( cnt ) ->
	begin
	  match cnt with 
	      LiSymIConst( const_name ) -> CntSymCst(const_name)
	    | LiTypeSizeof ( t )  ->
		 interpret_ciltypes_size t (* Returns the constant
					   name associated to the type t.*)
	end

    | LiScalOfAddr( ptrexp , optype ) -> 
      begin
       	(*let ll = interpret_c_ptrexp_to_cnt sslf ptrexp in
	  let  sizeof_ptr_type = interpret_ciltypes_size optype in
	  CntProd(ll,sizeof_ptr_type *)
	CntNdet
      end

    | LiElemOfCTab(_,_) ->
      CntNdet
      
    | LiScalOfLiBool(_)->
      CntNdet (* !! That can be statically decided in various different
	      cases, must be refined at some point !!*)
	
    
	    
and interpret_c_ptrexp_to_cnt (sslf : ssl_formula )( ptrexp : c_ptrexp ) =
  match ptrexp with 
      LiPVar(_,_,_) ->  offset_cnt_name ptrexp
     
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

    | LiAddrOfScal ( scalval , optype ) ->
      begin
	let ll = interpret_c_scal_to_cnt sslf scalval in
	let  sizeof_ptr_type = interpret_ciltypes_size optype in
	CntProd(ll,sizeof_ptr_type)
      end

    | LiBaseAddrOfArray (position,LiTab(Some(name),index_list,typeofelem)) ->
      begin
	CntNdet (* Offset of an array set to zero*)
      end

    | LiBaseAddrOfArray (position,LiTab(None,index_list,typeofelem)) ->
      begin
	CntNdet (* Offset of an array set to zero*)
      end
    | LiDerefCVar(vname, _) ->
      begin
	let vname = dereferenced_name_of_varname vname in  
	CntVar(NtsIVar(vname))
      end

    | LiStarOfPtr(cptr,t) ->
      begin
	match cptr with
	    LiDerefCVar(vname,_) ->
	      CntVar(NtsIVar(vname))
	  | LiDerefCPtr(vptr,_) ->
	    let cnt_vptr = interpret_c_ptrexp_to_cnt sslf vptr in
	    cnt_vptr
	      
	  | _ -> CntNdet
      end

    (*| Li
      begin
	match cptr with
	    LiDerefCVar(vname,_) ->
	      CntVar(NtsIVar(vname))
	  | LiDerefCPtr(vptr,_) ->
	    let cnt_vptr = interpret_c_ptrexp_to_cnt sslf vptr in
	    cnt_vptr
	      
	  | _ -> CntNdet
      end *)

    |  LiDerefCPtr ( cptr , t ) ->
      CntNdet

    | LiDerefCTab(LiTab(Some(vname),_,_))-> 
      let vname = dereferenced_name_of_varname vname in  
      CntVar(NtsIVar(vname))
	
    | LiDerefCTab(LiTab(None,_,_))-> 	
      CntNdet

and c_bool_to_cnt_bool (sslf : ssl_formula)(cbool : c_bool ) = 
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
    
    | LiBScal (cscal) ->
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
	
(** Returns the type of the pointer expression, that is
basically the type of the varname. Returns the type of the
innermost pointer variable the expression tree.

One need to check that this procedure is sufficient, but
it may not be.
*)
let rec type_of_ptrexp ptrexp =
  match ptrexp with 
      LiPVar( _ , LiIntPtr(vname), vtype) -> vtype
    | LiBaseAddrOfArray(_,LiTab(_,_,t))-> t
    
    | LiPlusPI ( cptrexp , _ ,_) -> 
	type_of_ptrexp cptrexp
    | LiMinusPI ( cptrexp , scalv,_ ) ->
	type_of_ptrexp cptrexp
    | LiIndexPI ( cptrexp , scalv,_ ) ->
	type_of_ptrexp cptrexp
    | LiAddrOfScal(_, optype ) -> optype



	  

(** Translates expressions of the valid_counter type into 
booleans of the nts arithmetics.*)
let rec valid_expr_2_cnt_bool ( vexpr : valid_counter ) =
  match vexpr with 
      TrueValid -> CntBTrue
    | FalseValid -> CntBFalse
    (*
      | PtValid ( s ) -> CntBool(CntEq,CntVar(NtsIVar(("validity__"^s^"_"))),CntCst(1))
    | IntValid ( s ) -> CntBool(CntEq,CntVar(NtsIVar(("validity__"^s^"_"))),CntCst(1))
    *)
    | PtValid ( s ) -> CntBool(CntEq,CntVar(NtsIVar(s)),CntCst(1))
    | IntValid ( s ) -> CntBool(CntEq,CntVar(NtsIVar(s)),CntCst(1))
    | AndValid ( l , r ) -> 
	begin
	  match l , r with 
	      (FalseValid , _ ) -> CntBFalse
	    | (_,FalseValid ) -> CntBFalse
	    | (_,_) ->
		let ll = valid_expr_2_cnt_bool l in
		let rr =  valid_expr_2_cnt_bool r in
		  CntBAnd ( ll , rr )
	end
	  
    | OrValid ( l , r ) -> 
	begin
	 match l , r with 
	      (TrueValid , _ ) -> CntBTrue
	    | (_, TrueValid ) -> CntBTrue
	    | (_,_) ->
		let ll = valid_expr_2_cnt_bool l in
		let rr =  valid_expr_2_cnt_bool r in
		  CntBOr ( ll , rr )  
	end

    
