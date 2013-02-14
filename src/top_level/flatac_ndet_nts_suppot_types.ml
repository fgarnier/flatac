(*

type ndet_supp_cnt_val =
  DetAVal of Nts_types.nts_genrel_arithm_exp
| NDetAVal of Nts_types.nts_genrel_arithm_exp 
| DetNdetBOp of nts_gen_arithm_binop * ndet_supp_cnt_val *ndet_supp_cnt_val
  * nts_base_type
| DetNdetUOp of nts_gen_arithm_up * ndet_supp_cnt_val * nts_base_types 
| CntINdet
| CntRNdet
| CntBNdet
| CntNdet 

*)


exception Type_mismatch_in_arithm_expression of Nts_types.nts_genrel_arithm_exp * Nts_types.nts_genrel_arithm_exp
exception Type_mismatch_in_detndetarithmetic_operation of ndet_supp_cnt_val * ndet_supp_cnt_val




let type_if_type_eq vg vd =
  let opt = Nts_generic.arithm_exp_same_type vg vd in
  match opt with
    Some(t) -> t
  | None -> raise (Type_mismatch_in_arithm_expression(vg,vd))

(** Get type as specified in the topmost contructor or in its
immediate subtree, i.e. non type checking.*)

let type_of_ndet_supp_cnt_val_shallow v =
  | CntINdet -> NtsIntType
  | CntRNdet -> NtsRealType
  | DetAVal(aop) -> Nts_generic.type_of_gen_arithmetic_expr aop
  | NdetAVal(aop) -> Nts_generic.type_of_gen_arithmetic_expr aop
  | DetNdetBOp(_,_,_,t) -> t
  | DetNderUOp(_,_,t) -> t



let type_if_eq_det_ndet_aexp l r =
  let tl = type_of_ndet_supp_cnt_val_shallow l in
  let tr = type_of_ndet_supp_cnt_val_shallow r in
  if tl=tr then
    tl
  else
    raise (Type_mismatch_in_detndetarithmetic_operation(l,r) )


(** This function creates a new term *)
let aterm_binop_ndet_supp_cnt_val ( operator : nts_genrel_arithm_exp ) vg vd =
  match vg,vd with 
    DetAVal(l),DetAVal(r) -> 
      begin 
	let aexptype = type_if_type_eq l r in
	DetAVal(CntGenArithmBOp(operator,l,r,aexptype))
      end
  | _,_  ->
    begin
      let aexptype = type_if_eq_det_ndet_aexp l r in
      DetNdetBOp(operator,vg,vd,aexptype)
    end

let aterm_uop_ndet_supp_cnt_val (operator : nts_genrel_arithm_exp ) t =
  match t with
    DetVal((op,_,typ) as v) ->
      DetVal(CntGenUOp(operator,v,typ))
  
  | NdetAVal(aop) -> 
    let typofaop = type_of_ndet_supp_cnt_val_shallow aop in
    DetNdetUOp(operator,aop,typofaop)
    
    
