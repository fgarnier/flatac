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
open Intermediate_language_types
open Intermediate_language
open Ssl_types
open Ssl
open SSL_lex
open Global_mem
open Validity_types
open Validity
open Cil_types
open Nts_types
open Big_int

(**
   Those two modules allow to handle non determinism 
   on arithemtical expressions.
*)

open Flatac_ndet_nts_support_types
open Flatac_ndet_nts_support

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
    CntSymCst( typename, NtsIntType )

(** Needs to be duely completed.
Given a type t, this function return the sizeof of t,
or for type t*, it returns the sizeof of t.
*)
let rec interpret_ciltypes_size (ciltype : Cil_types.typ ) =
  match ciltype with
      TInt(IBool,_) -> 
	CntGenSymCst (CntSymCst("sizeof_bool",NtsIntType),NtsIntType)
    | TInt(IChar,_) -> CntGenCst (CntGenICst(Big_int.big_int_of_int 1),NtsIntType)
    | TInt(ISChar,_) -> CntGenCst (CntGenICst(big_int_of_int 1),NtsIntType)
    | TInt(IUChar,_) -> CntGenCst (CntGenICst(big_int_of_int 1),NtsIntType)
    | TInt(IInt,_) -> CntGenCst (CntGenICst(big_int_of_int 4),NtsIntType)
    | TInt(IUInt,_) -> CntGenCst (CntGenICst(big_int_of_int 4), NtsIntType)
    | TInt(IShort,_) -> CntGenCst (CntGenICst(big_int_of_int 2), NtsIntType)
    | TInt(IUShort,_) -> CntGenCst (CntGenICst(big_int_of_int 2), NtsIntType)
    | TInt(ILong,_) -> CntGenCst (CntGenICst(big_int_of_int 4),NtsIntType)
    | TInt(IULong,_) -> CntGenCst (CntGenICst(big_int_of_int 4),NtsIntType)
    | TInt(ILongLong,_) -> CntGenCst (CntGenICst(big_int_of_int 8),NtsIntType)
    | TInt(IULongLong,_) -> CntGenCst (CntGenICst(big_int_of_int 8), NtsIntType)	
    | TFloat(FFloat,_) -> CntGenCst (CntGenICst(big_int_of_int 8),NtsIntType)
    | TFloat(FDouble,_) -> CntGenCst (CntGenICst(big_int_of_int 8),NtsIntType)
    | TFloat(FLongDouble,_) -> CntGenCst (CntGenICst(big_int_of_int 8),NtsIntType)
    | TVoid([]) -> CntGenCst (CntGenICst(big_int_of_int 1),NtsIntType) 
    | TNamed(tinfo, _ ) -> let symsize = sizeof_cil_tinfo tinfo in
			   CntGenSymCst(symsize,NtsIntType)

    | TPtr(TVoid([]), _) ->  CntGenCst (CntGenICst(big_int_of_int 4),NtsIntType)
    | TPtr(t,_) -> interpret_ciltypes_size t (* Won't work
					     for t** ...*)
    | (TArray (tin, None,_,_ )) ->   CntGenSymCst(CntSymCst("sizeof_array_reference",NtsIntType),NtsIntType)
      
    | _ -> raise  Unhandled_valuetype_in_interpretciltypesize

let offset_cnt_of_pvar (pvar : ptvar) =
  match pvar with 
      PVar(vname) -> NtsVar ( ("offset__"^vname^"_"),NtsIntType )


(** Same as above, but more conveninant to express CntAffectation,
in the sense it spare the programmer to add another cumbersome
match structure to get the embeded NtsIVar(name).
*)
let offset_ntsivar_of_pvar (pvar : ptvar ) =
   match pvar with 
      PVar(vname) -> NtsVar("offset__"^vname^"_",NtsIntType) 

   
(** This function aims at computing the name of the counter var name
associated to the offset of a pointer variable*)
let offset_cnt_name ( ptvar : c_ptrexp ) =  
  match ptvar with
      LiPVar(_,LiIntPtr(vname),_) -> NtsGenVar ( NtsVar("offset__"^vname^"_",NtsIntType),NtsUnPrimed )
    | _ -> raise Not_LiPVar
  

let int_var_cnt_name ( cexpr : c_scal) =
   match cexpr with
      LiVar(_,LiIntVar(vname)) -> NtsGenVar(NtsVar( vname, NtsIntType),NtsUnPrimed)
    | _ -> raise Not_LiVar


let float_var_cnt_name ( cexpr : c_scal) =
   match cexpr with
      LiFVar(_,LiFloatVar(vname)) -> NtsGenVar(NtsVar( vname ,NtsRealType),NtsUnPrimed)
    | _ -> raise Not_LiVar

(** This function returns a counter associated to base address of a location
variable. This function is used to model a memory allocation. See thechnical
report and the section dedicated to malloc. *)
let lvarbase_cnt_name ( lvar : locvar ) =
  match lvar with
    | LVar (vname) -> NtsGenVar(NtsVar(vname^"_base_addr",NtsIntType),NtsUnPrimed)

(** Same as above, but for the size of a segment allocation.*)
let lvarsize_cnt_name ( lvar : locvar ) =
  match lvar with 
    | LVar (vname ) -> NtsGenVar(NtsVar(vname^"_size_of_segment",NtsIntType),NtsUnPrimed)



exception TypeMisMatchInBop of c_scal * c_scal

(** Interprets a scalar expression into the nts formalism, w.r.t. the
current context expressed as a ssl formula.*)
let rec interpret_c_scal_to_cnt  ( sslf : ssl_formula )( scalexp : c_scal ) =
  match scalexp with 
      LiVar(_) -> 
	 DetAVal(CntGenVar(int_var_cnt_name scalexp))
    | LiFVar(_) ->
      DetAVal(CntGenVar(float_var_cnt_name scalexp))
	
    | LiFConst(LiFloatConst(f)) -> 
      DetAVal(CntGenCst(CntRealConst(f),NtsRealType))
 
    | LiConst(LiIConst(i)) ->  DetAVal(CntICst(i))
    | LiProd ( l , r ) ->
	begin
	  let lg = interpret_c_scal_to_cnt sslf l in
	  let ld = interpret_c_scal_to_cnt sslf r in
	  (*let type_lg = Nts_generic.type_of_gen_arithmetic_expr lg in
	  let type_ld = Nts_generic.type_of_gen_arithmetic_expr ld in
	  if (type_lg=type_ld) then*)  
	  aterm_binop_ndet_supp_cnt_val CntGenProd lg ld
	  (*DetAVal(CntGenArithmBOp(CntGenProd, lg , ld, type_lg))*)
	  (*else
	    raise (TypeMisMatchInBop(l,r))*)
	end
    |  LiSum  ( l , r ) ->
	 begin
	   let lg = interpret_c_scal_to_cnt sslf l in
	   let ld = interpret_c_scal_to_cnt sslf r in
	   (*let type_lg = Nts_generic.type_of_gen_arithmetic_expr lg in
	   let type_ld = Nts_generic.type_of_gen_arithmetic_expr ld in
	   if (type_lg=type_ld) then  
	   DetAVal(CntGenArithmBOp(CntGenSum, lg , ld, type_lg))*)
	   aterm_binop_ndet_supp_cnt_val CntGenSum lg ld
	   (*else
	     raise (TypeMisMatchInBop(l,r))
	   *) 
	 end
	   
    | LiMinus ( l , r ) ->
      begin
	let lg = interpret_c_scal_to_cnt sslf l in
	let ld = interpret_c_scal_to_cnt sslf r in
	
	aterm_binop_ndet_supp_cnt_val CntGenMinus lg ld
      end
	  
    | LiMod ( l , r ) ->
	begin
	  let lg = interpret_c_scal_to_cnt sslf l in
	  let ld = interpret_c_scal_to_cnt sslf r in
	  aterm_binop_ndet_supp_cnt_val CntGenMinus lg ld
	end


    | LiDiv( l ,r ) ->
      begin
	let lg = interpret_c_scal_to_cnt sslf l in
	let ld = interpret_c_scal_to_cnt sslf r in
	aterm_binop_ndet_supp_cnt_val CntGenDiv lg ld
      (*let type_lg = Nts_generic.type_of_gen_arithmetic_expr lg in
	let type_ld = Nts_generic.type_of_gen_arithmetic_expr ld in
	if (type_lg=type_ld) then  
	  DetAVal(CntGenArithmBOp(CntGenDiv, lg , ld, type_lg))
	  else
	    raise (TypeMisMatchInBop(l,r)) 
	*)
      end
	  
    | LiUnMin( t ) -> 
	let tin = interpret_c_scal_to_cnt sslf t in
	aterm_uop_ndet_supp_cnt_val CntGenUMinus tin
	(*let top = Nts_generic.type_of_gen_arithmetic_expr t in
	  DetAVal(CntGenArithmUOp( CntGenUMinus,tin,top))
	*)
  
    | LiMinusPP ( l , r , optype) ->
	let basel = base_ptrexp sslf l in
	let baser = base_ptrexp sslf r in
	  if basel = baser then
	    begin
	      let lg = interpret_c_ptrexp_to_cnt sslf l in
	      let ld = interpret_c_ptrexp_to_cnt sslf r in
	      aterm_binop_ndet_supp_cnt_val CntGenMinus lg ld
	      (*DetAVal(CntGenArithBOp ( CntGenMinus , lg , ld , NtsIntType))*)
	    end
	  else CntINdet (** Non deterministic value, which has type int.*)
    
    | LiSymConst ( cnt ) ->
	begin
	  match cnt with 
	      LiSymIConst( const_name ) -> 
		DetAVal(CntSymCst(const_name))
	    | LiTypeSizeof ( t )  ->
		 DetAVal(interpret_ciltypes_size t)
	(* Returns the constant
	   name associated to the type t.*)
	end

    | LiScalOfAddr( _ , optyp ) -> 
      begin
       	(*let ll = interpret_c_ptrexp_to_cnt sslf ptrexp in
	  let  sizeof_ptr_type = interpret_ciltypes_size optype in
	  CntProd(ll,sizeof_ptr_type *)
       
	match ( Composite_types.is_integer_type optyp)
	with
	    Some(_) -> CntINdet
	  | None -> 
	    begin
	      match ( Composite_types.is_float_type optyp)
	      with
		  Some(_) ->  CntRNdet
		| None -> CntNdet
	    end
	  
      end

    | LiElemOfCTab(acc_list,LiTab(_,dim_infos,ciltyp)) ->
      begin
	if (List.length dim_infos) != ( List.length acc_list)
	then assert false
	else
	  begin
	    match ( Composite_types.is_integer_type ciltyp)
	    with
		Some(_) -> CntINdet
	      | None -> 
		begin
		  match ( Composite_types.is_float_type ciltyp)
		  with
		      Some(_) -> CntRNdet
		    | None -> assert false
		end
	  end
      end
      
    | LiScalOfLiBool(_)->
      CntINdet (* !! That can be statically decided in various different
	      cases, must be refined at some point !!*)
	
    
	    
and interpret_c_ptrexp_to_cnt (sslf : ssl_formula )( ptrexp : c_ptrexp ) =
  match ptrexp with 
      LiPVar(_,_,_) ->  DetAVal(offset_cnt_name ptrexp)
     
    | LiPlusPI ( cptrexp , scalv, optype ) -> 
	begin
	  let ll = interpret_c_ptrexp_to_cnt sslf cptrexp in
	  let lr = interpret_c_scal_to_cnt sslf scalv in
	  let sizeof_ptr_type = interpret_ciltypes_size optype in
	  let lr = aterm_binop_ndet_supp_cnt_val CntGenProd lr sizeof_ptr_type 
	  in
	  aterm_binop_ndet_supp_cnt_val CntGenSum ll lr
	    
	(*CntProd( lr , sizeof_ptr_type ) in
	  CntSum(ll, lr) *)
	end
    | LiMinusPI ( cptrexp , scalv , optype ) ->
	begin
	  let ll = interpret_c_ptrexp_to_cnt sslf cptrexp in
	  let lr = interpret_c_scal_to_cnt sslf scalv in
	  let sizeof_ptr_type = interpret_ciltypes_size optype in
	  let to_div = aterm_binop_ndet_supp_cnt_val CntGenMinus ll lr in
	  aterm_binop_ndet_supp_cnt_val CntGenMinus to_div sizeof_ptr_type
	    (* CntDiv(CntMinus(ll,lr),sizeof_ptr_type) *)
	end
    | LiIndexPI ( cptrexp , scalv, optype ) ->
	begin
	  let ll = interpret_c_ptrexp_to_cnt sslf cptrexp in
	  let lr = interpret_c_scal_to_cnt sslf scalv in
	  let  sizeof_ptr_type = interpret_ciltypes_size optype in
	  let lr = aterm_binop_ndet_supp_cnt_val CntGenProd lr sizeof_ptr_type 
	  in 
	  aterm_binop_ndet_supp_cnt_vak CntGenSum lr sizeof_ptr_size
	    (*CntProd ( lr , sizeof_ptr_type ) in
	    CntSum(ll,lr) *) 
	end

    | LiAddrOfScal ( scalval , optype ) ->
      begin
	let ll = interpret_c_scal_to_cnt sslf scalval in
	let  sizeof_ptr_type = interpret_ciltypes_size optype in
	aterm_binop_ndet_supp_cnt_val CntGenProd ll sizeof_ptr_type
	  (*CntProd(ll,sizeof_ptr_type)*)
      end

    | LiBaseAddrOfArray (position,LiTab(Some(name),index_list,typeofelem)) ->
      begin
	CntINdet (* Offset of an array set to zero*)
      end

    | LiBaseAddrOfArray (position,LiTab(None,index_list,typeofelem)) ->
      begin
	CntINdet (* Offset of an array set to zero*)
      end
    | LiDerefCVar(vname, _) ->
      begin
	let vname = dereferenced_name_of_varname vname in  
	CntVar(NtsIVar(vname))
      end

    | LiStarOfPtr(cptr,t) ->
      begin
	(*Cil.d_type Ast_goodies.debug_out t;
	Format.fprintf  Ast_goodies.debug_out "%!";
	  assert false;
	*)
	match cptr with
	    LiDerefCVar(vname,_) ->
	      CntVar(NtsIVar(vname))
	  
	  | LiDerefCPtr(vptr,_) ->
	    let cnt_vptr = interpret_c_ptrexp_to_cnt sslf vptr in
	    cnt_vptr
	      
	  | _ -> 
	    begin
	      match (Composite_types.is_integer_type t) with
		  Some(_) -> CntNdet
		| None -> 
		  begin
		    match (Composite_types.is_float_type t) with
			Some(_) -> CntRValNdet
		       | _ -> CntNdet 
		  end
	    end
      end

   

    |  LiDerefCPtr ( cptr , t ) ->
      CntNdet

    | LiDerefCTab(LiTab(Some(vname),_,_))-> 
      let vname = dereferenced_name_of_varname vname in  
      CntVar(NtsIVar(vname))
	
    | LiDerefCTab(LiTab(None,_,_))-> 	
      CntNdet

and c_bool_to_cnt_bool (sslf : ssl_formula)(cbool : c_bool) = 
  match cbool with 
      LiBNot (b) -> 
	let cnt_arg = c_bool_to_cnt_bool sslf b in 
	  Flatac_ndet_nts_support.neg_bterm cnt_arg 
    | LiBAnd ( bg , bd ) ->
	begin
	  let bgarg = c_bool_to_cnt_bool sslf bg in
	  let bdarg = c_bool_to_cnt_bool sslf bd in
	  bterm_logic_binop CntGenBAnd bgarg bdarg
	   (* CntBAnd ( bgarg , bdarg ) *) 
	end
    
    | LiBOr ( bg , bd ) ->
	begin
	  let bgarg = c_bool_to_cnt_bool sslf bg in
	  let bdarg = c_bool_to_cnt_bool sslf bd in
	  bterm_logic_binop CntGenBOr bgarg bdarg
	   (* CntBOr ( bgarg , bdarg ) *)
	end

    | LiBTrue -> ND_CntGenTrue
    | LiBFalse -> ND_CntGenFalse
	
    | LiBEq ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	  bterm_genrel_comp CntGenEq argg argd
	  (*CntBool ( CntEq , argg , argd )*)
	end

    | LiBNeq ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	  bterm_gemrel_comp CntNeq argg argd
	(*CntBool ( CntNeq , argg , argd )*)
	end

    | LiBLt ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	  bterm_genrel_comp CntGenLt argg argd   
	   (* CntBool ( CntLt , argg , argd ) *)
	end
	  
    | LiBLeq ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	  bterm_genrel_comp CntGenLeq argg argd   
	    (*CntBool ( CntLeq , argg , argd ) *)
	end

    | LiBGt ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	  bterm_genrel_comp CntGenGt argg argd   
	   (* CntBool ( CntGt , argg , argd ) *)
	end 
    
    | LiBGeq ( cscalg , cscald ) -> 
	begin
	  let argg =  interpret_c_scal_to_cnt sslf cscalg in
	  let argd =  interpret_c_scal_to_cnt sslf cscald in
	  bterm_genrel_comp CntGenGeq argg argd
	  (*CntBool ( CntGeq , argg , argd )*)
	end
    
    | LiBScal (cscal) ->
       begin
	 let arg = interpret_c_scal_to_cnt sslf cscal in
	 bterm_genrel_comp CntGenEq arg (DetAVal(CntGenICst(My_bigint.zero)))
	   (*CntBool (CntEq , arg , (CntCst(My_bigint.zero)))*)
       end

    (* Pointer comparisons match cases start here*)
    | LiBPtrEq ( cptrg ,  cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	  bterm_genrel_comp CntGenEq argg argd
	    (*CntBool (CntEq , argg , argd ) *)
	end
	     

    | LiBPtrNeq ( cptrg , cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	  bterm_genrel_comp CntNeq argg argd
	  (*CntBool (CntNeq , argg , argd ) *)
	end

    
    | LiBPtrGeq ( cptrg , cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	  bterm_genrel_comp CntGeq argg argd
	(*CntBool ( CntGeq , argg , argd ) *)
	end
	
	
    | LiBPtrGt ( cptrg , cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	  bterm_genrel_comp CntGt argg argd
	(*CntBool ( CntGt , argg , argd ) *)
	end

    | LiBPtrLt ( cptrg , cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	   bterm_genrel_comp CntLt argg argd
	  (*CntBool ( CntLt , argg , argd )*)
	end	  
	  
    | LiBPtrLeq ( cptrg , cptrd ) ->
	begin
	  let argg =  interpret_c_ptrexp_to_cnt sslf cptrg in
	  let argd =  interpret_c_ptrexp_to_cnt sslf cptrd in
	  bterm_genrel_comp CntLea argg argd 
	    (*CntBool (CntLeq , argg , argd )*)
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
    | PtValid ( s ) -> CntBool(CntEq,CntVar(NtsIVar(s)),CntCst(My_bigint.one))
    | IntValid ( s ) -> CntBool(CntEq,CntVar(NtsIVar(s)),CntCst(My_bigint.one))
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

    
