open Nts_types
open Flatac_ndet_nts_support_types


(** 
Those types and functions here are used to add to nts 
expressions the fact that some arithmetical expression don't have 
a deterministic evaluation.

This is 
*)

let type_if_type_eq vg vd =
  let opt = Nts_generic.arithm_exp_same_type vg vd in
  match opt with
    Some(t) -> t
  | None -> raise (Nts_generic.Type_mismatch_in_arithm_expression(vg,vd))

(** Get type as specified in the topmost contructor or in its
immediate subtree, i.e. non type checking.*)

let type_of_ndet_supp_cnt_val_shallow v =
	match v with	
  | CntINdet -> NtsIntType
  | CntRNdet -> NtsRealType
  | CntBNdet -> NtsBoolType
  | CntNdet -> assert false
  | DetAVal(aop) -> Nts_generic.type_of_gen_arithmetic_expr aop
  | NDetAVal(aop) -> Nts_generic.type_of_gen_arithmetic_expr aop
  | DetNdetBOp(_,_,_,t) -> t
  | DetNdetUOp(_,_,t) -> t



let type_if_eq_det_ndet_aexp l r =
  let tl = type_of_ndet_supp_cnt_val_shallow l in
  let tr = type_of_ndet_supp_cnt_val_shallow r in
  if tl=tr then
    tl
  else
    raise (Type_mismatch_in_detndetarithmetic_operation(l,r) )


(** This function creates a new term *)
let aterm_binop_ndet_supp_cnt_val ( operator : nts_gen_arithm_binop ) vg vd =
  match vg,vd with 
    DetAVal(l),DetAVal(r) -> 
      begin 
	let aexptype = type_if_type_eq l r in
	DetAVal(CntGenArithmBOp(operator,l,r,aexptype))
      end
  | _,_  ->
    begin
      let aexptype = type_if_eq_det_ndet_aexp vg vd in
      DetNdetBOp(operator,vg,vd,aexptype)
    end

let aterm_uop_ndet_supp_cnt_val (operator : nts_gen_arithm_unop ) t =
  match t with
    DetAVal( v) ->
      let typ = Nts_generic.type_of_gen_arithmetic_expr v in
      DetAVal(CntGenArithmUOp(operator,v,typ))
  
  | NDetAVal(aop) -> 
    let typofaop = type_of_ndet_supp_cnt_val_shallow t in
    DetNdetUOp(operator,t,typofaop)
    
  | _ -> 
    let typeofaop = type_of_ndet_supp_cnt_val_shallow t in
    DetNdetUOp(operator,t,typeofaop)

let bterm_genrel_comp rel_binop nd_vall nd_valr =
  ND_CntGenRelComp(rel_binop,nd_vall,nd_valr)

let bterm_logic_binop binop boolg boold =
  ND_CntGenRel(binop,boolg,boold)

let neg_bterm bt =
  match bt with
    ND_CntGenNot(t) -> t
  | _ -> ND_CntGenNot(bt)



let arithm_value_of_ndsupport_or_fails v =
  match v with
    DetAVal(v) -> v
  | _ -> assert false

(** Checks whether a subtree reprensents a deterministic value*)
let is_val_det v =
  match v with
    DetAVal(_) -> true
  | _ -> false


(*

val nts_pprint_bool_binop :
  string -> Nts_types.nts_gen_bool_binop -> string -> string

val nts_pprint_aritm_binop : Nts_types.nts_gen_arithm_binop -> string
*)


let rec pprint_val value = 
  match value with 
    DetAVal(v) -> Nts_generic.nts_pprint_genrel_arithm_exp v
  | NDetAVal(v) -> Format.sprintf "Non_det(%s)" (Nts_generic.nts_pprint_genrel_arithm_exp v)
  | DetNdetBOp(bop,fg,fd,_) ->
    begin
      let op_printout = Nts_generic.nts_pprint_aritm_binop bop in
      let fg_printout = pprint_val fg in
      let fd_printout = pprint_val fd in
      Format.sprintf "(%s) %s (%s)" fg_printout op_printout fd_printout 
    end
  | DetNdetUOp(uop,f,_) ->
    begin
      let op_printout = Nts_generic.nts_pprint_arithm_unop uop in
      let f_printout = pprint_val f in
      Format.sprintf "%s (%s)" op_printout f_printout 
    end
  | _ -> "Ndet_var"
