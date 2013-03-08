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


module Vars_acc = Nts_generic.Vars_acc


let rec primed_vars_of_ndet_cnt_val term folder_set =
  match term with
    NDetAVal(aexp) | DetAVal (aexp) ->
      let expv = Nts_generic.primed_vars_of_genrel_aexpr aexp
      in
      Vars_acc.union folder_set expv
  | DetNdetBOp (_,ndexpg,ndexpd,_) -> 
    begin
      let var_g = primed_vars_of_ndet_cnt_val ndexpg folder_set in
      primed_vars_of_ndet_cnt_val ndexpd var_g 
    end

  | DetNdetUOp (_,v,_)
    -> primed_vars_of_ndet_cnt_val v folder_set 

  | CntINdet
  | CntRNdet
  | CntBNdet
  | CntNdet -> folder_set



let rec get_set_of_modified_vars_ndet_cnt_bool term folder_set =
  match term with 
    ND_CntGenTrue
  | ND_CntGenFalse
  | ND_CntGenDK -> folder_set
    
  | ND_CntGenNot ( e) -> 
    get_set_of_modified_vars_ndet_cnt_bool e folder_set
 
  | ND_CntGenRelComp(_,g,d)-> 
    let vg =  primed_vars_of_ndet_cnt_val g folder_set in
    primed_vars_of_ndet_cnt_val d vg

  | ND_CntGenRel (_, ndg, ndd) -> 
    let vg = get_set_of_modified_vars_ndet_cnt_bool ndg folder_set 
    in
    get_set_of_modified_vars_ndet_cnt_bool ndd vg


(*
let get_list_of_modified_varsnd_translabel nd_genrel =
  let accu = Vars_acc.empty in
  let rec collect_vars t acc =
    match t with
      ND_CntGenGuard () ->

    | ND_CntGenGuardIf ndet_supp_cnt_genrel
    | ND_CntGenGuardElse of ndet_supp_cnt_genrel
	
    | ND_CntGenCall ( _, Some(ret_lhs_vars) , _ ) 
      -> 
      begin
	List.iter (fun s -> Vars_acc.add s acc) ret_lhs_vars
      end
				     				     
    | ND_CntGenHavoc _ -> acc 
*)		     



(* this method is used to compute the set of counter variables who are
   assigned a new value*)
(*

let havocise (trans_label_list : ndet_supp_nts_trans_label list) =
  let not_havoc label =
    match label with
      ND_CntGenHavoc(_) -> false
    (*| CntAffect(_,CntNdet)-> false*)
    (*| CntGuard( CntBool(_,CntNdetVar("__if_ndet_cond__"),_)) -> false*)
    | _ -> true
  in
  let modified_vars (var_list : Nts_types.nts_var list)
      (trans_label : cnt_trans_label) =
    match trans_label with
      
      
    (* Elimination
       de toutes les variables
       de test à valeurs non déterministes.
       Cas le plus général,
       ?? plus grand point fixe ??
       Pourrait certainement se
       raffiner.
    *)
      
(*| CntGuard( CntBool(_,CntNdetVar("__if_ndet_cond__"),_))
-> (NtsIVar("__if_ndet_cond__"))::var_list*)
      
    | CntAffect(nvar,_) -> nvar::var_list
    | ND_CntGenCall(_,Some(nvar_list),_) -> nvar_list@var_list
    | CntHavoc (nvlist) -> nvlist@var_list
    | CntGuard(CntBool(_,CntNdetVar("__ndet_cond__"),_))
    | CntGuard(CntNot(CntBool(_,CntNdetVar("__ndet_cond__"),_)))
      ->
      begin
	if (not (List.exists
		   (fun s->
		     match s with
		     | NtsIVar("__ndet_cond__") -> true
		     | _ -> false
		   )
		   var_list) )
	then
	  NtsIVar("__ndet_cond__")::var_list
	else
	  var_list
      end
    | CntGuardIf(CntBool(_,CntNdetVar("__if_ndet_cond__"),_))
    | CntGuardElse((CntBool(_,CntNdetVar("__if_ndet_cond__"),_)))
    | CntGuardElse(CntNot((CntBool(_,CntNdetVar("__if_ndet_cond__"),_))))
      ->
      begin
	if (not (List.exists
		   (fun s->
		     match s with
		     | NtsIVar("__if_ndet_cond__") -> true
		     | _ -> false
		   )
		   var_list) )
	then
	  NtsIVar("__if_ndet_cond__")::var_list
	else
	  var_list
      end


    | _ -> var_list
  in
  let vars_in_havoc = List.fold_left modified_vars [] trans_label_list in
  let ret_list = List.filter not_havoc trans_label_list in
  (ret_list@(CntHavoc(vars_in_havoc)::[]))
*)


(*


(* this method is used to compute the set of counter variables who are
   assigned a new value*)
let havocise (trans_label_list : cnt_trans_label list) =
  let not_havoc label =
    match label with
      CntHavoc(_) -> false
    (*| CntAffect(_,CntNdet)-> false*)
    (*| CntGuard( CntBool(_,CntNdetVar("__if_ndet_cond__"),_)) -> false*)
    | _ -> true
  in
  let modified_vars (var_list : Nts_types.nts_var list)
      (trans_label : cnt_trans_label) =
    match trans_label with
      
      
    (* Elimination
       de toutes les variables
       de test à valeurs non déterministes.
       Cas le plus général,
       ?? plus grand point fixe ??
       Pourrait certainement se
       raffiner.
    *)
      
(*| CntGuard( CntBool(_,CntNdetVar("__if_ndet_cond__"),_))
-> (NtsIVar("__if_ndet_cond__"))::var_list*)
      
    | CntAffect(nvar,_) -> nvar::var_list
    | CntFunCall(_,Some(nvar_list),_) -> nvar_list@var_list
    | CntHavoc (nvlist) -> nvlist@var_list
    | CntGuard(CntBool(_,CntNdetVar("__ndet_cond__"),_))
    | CntGuard(CntNot(CntBool(_,CntNdetVar("__ndet_cond__"),_)))
      ->
      begin
	if (not (List.exists
		   (fun s->
		     match s with
		     | NtsIVar("__ndet_cond__") -> true
		     | _ -> false
		   )
		   var_list) )
	then
	  NtsIVar("__ndet_cond__")::var_list
	else
	  var_list
      end
    | CntGuardIf(CntBool(_,CntNdetVar("__if_ndet_cond__"),_))
    | CntGuardElse((CntBool(_,CntNdetVar("__if_ndet_cond__"),_)))
    | CntGuardElse(CntNot((CntBool(_,CntNdetVar("__if_ndet_cond__"),_))))
      ->
      begin
	if (not (List.exists
		   (fun s->
		     match s with
		     | NtsIVar("__if_ndet_cond__") -> true
		     | _ -> false
		   )
		   var_list) )
	then
	  NtsIVar("__if_ndet_cond__")::var_list
	else
	  var_list
      end


    | _ -> var_list
  in
  let vars_in_havoc = List.fold_left modified_vars [] trans_label_list in
  let ret_list = List.filter not_havoc trans_label_list in
  (ret_list@(CntHavoc(vars_in_havoc)::[]))
    
*)
