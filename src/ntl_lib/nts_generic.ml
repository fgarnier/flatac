open Nts_types
open Nts

exception Found_a_primed_var

let nts_pprint_genvar var =
  match var with
      NtsGenVar(v,NtsPrimed) -> Format.sprintf "%s'" (nts_pprint_nts_var v)
    | NtsGenVar(v,NtsUnPrimed) -> (nts_pprint_nts_var v)
  

let pprint_ntsgen_var_list l =
  let rec pprint_nts_var_list_fold str l =
    match str, l with 
	(_,[]) -> str
      | ("",(h::l')) -> pprint_nts_var_list_fold (nts_pprint_genvar h) l'
      | (_,(h::l')) -> pprint_nts_var_list_fold (str^","^(nts_pprint_genvar h)) l' 
  in
  (pprint_nts_var_list_fold "" l)

let pprint_nts_quantifier  q =
  match q with 
      NtsExists -> "exists"
    | NtsForall -> "forall"

let rec nts_pprint_nts_typeinfo_genvar ( x : nts_genrel_var) =
  match x with 
      NtsGenVar(NtsIVar( vname ),_) -> vname^" :int "
    | NtsGenVar(NtsRVar ( vname ),_) ->vname^" :real "
    | NtsGenVar(NtsMiscType ( vname ),_) ->vname^" : No defined type"
    | NtsGenVar(NtsINdetVar vname, _) ->vname^" :nondet int" 
      

let is_int_var v =
  match v with 
      NtsGenVar(NtsIVar( vname ),_)-> true
    |_ -> false
      
let is_real_var v =
  match v with
    NtsGenVar(NtsRVar( vname ),_)-> true
    |_ -> false
      

 
      
let concat_if_first_arg_nonzero s1 s2 =
  if String.length s1 != 0
  then s1^s2
  else ""
	
let concat_comma_both_arg_non_empty s1 s2 =
  if String.length s1 != 0 then
    begin
      if  String.length s2 != 0 then
	s1^","^s2
      else
	s1
    end
  else
    s2
      
(*let pprint_typeinfo_int_nts_var_list l =
  let int_var_list = List.filter ( is_int_var) l in
  pprint_ntsgen_var_list int_var_list
*)
  
let pprint_typeinfo_nts_genvar_list l =
  let int_var_list = List.filter ( is_int_var) l in
  let real_var_list =  List.filter ( is_real_var) l in
  let pp_of_list_of_int = 
    concat_if_first_arg_nonzero (pprint_ntsgen_var_list int_var_list) " : int" in
  let pp_of_list_of_real = 
    concat_if_first_arg_nonzero (pprint_ntsgen_var_list real_var_list) " : real" in
  concat_comma_both_arg_non_empty pp_of_list_of_int pp_of_list_of_real
    
    
  



let rec size_genrel_arithm_deeper_than 
    (barithm : nts_genrel_arithm_exp ) (depth : int ) =

  if depth <= 0 then true
  else 
    let depth' = depth - 1 in
    match barithm with 
	CntGenCst(_)
      | CntGenNdet
      | CntGenSymCst (_ )
      | CntGenVar (_)
      | CntGenNdetVar(_)
      | CntGenInvalidExp -> false
      | CntGenArithmUOp( _, exp' ) ->  
	size_genrel_arithm_deeper_than exp' depth'
      | CntGenArithmBOp (_ , eg ,  ed ) ->
	(size_genrel_arithm_deeper_than eg depth' ) || (size_genrel_arithm_deeper_than ed depth' )
      
  

(* This function is used to parse subtrees in a more human
readable fashion. *)

let rec size_genrel_deeper_than  (bexp : nts_gen_relation ) (depth : int ) =
  if depth <= 0 then
    true
  else
    let deep' = depth - 1 in
    match bexp with
	CntGenTrue -> false
      | CntGenFalse -> false
      | CntGenNot ( exp' ) -> size_genrel_deeper_than exp' deep'
      | CntGenRelComp(_ , eg ,  ed ) ->
	(size_genrel_deeper_than eg deep' ) || (size_genrel_deeper_than ed deep' )
      | CntGenRel ( _ , _ , _ ) -> false   
      |  CntQVarsGenRel(_,_,exp') -> size_genrel_deeper_than exp' deep'


let rec nts_pprint_genrel_arithm_exp ( exp : nts_genrel_arithm_exp ) =
  match exp with
      CntGenCst(i) -> Big_int.string_of_big_int i
    | CntGenNdet -> "NDET"
    | CntGenSymCst(str) -> str
    | CntGenVar ( ntsgenvar ) -> nts_pprint_genvar ntsgenvar
    | CntGenNdetVar(varname) -> varname
    
    
    | CntGenArithmUOp(CntGenUMinus, e ) ->
      begin
	match e with
	    CntGenArithmUOp(CntGenUMinus, subtree ) -> nts_pprint_genrel_arithm_exp subtree
	  | _  -> "-"^(nts_pprint_genrel_arithm_exp e)
      end

    | CntGenArithmBOp(CntGenSum , eg , ed ) ->
      (nts_pprint_genrel_arithm_exp eg ) ^"+" ^(nts_pprint_genrel_arithm_exp ed)

    | CntGenArithmBOp( CntGenMinus , eg , ed )
      -> 
      begin
	if size_genrel_arithm_deeper_than ed 2 then
	  let pprint_output = 
	    (nts_pprint_genrel_arithm_exp eg)^"-("^(nts_pprint_genrel_arithm_exp ed)^")"
	  in
	  pprint_output
	else
	  (nts_pprint_genrel_arithm_exp eg)^"-"^(nts_pprint_genrel_arithm_exp ed)
      end
	
    | CntGenArithmBOp(  CntGenDiv,  eg , ed )
      -> 
     begin
       let pprint_outputd = ref ""
       in
       let pprint_outputg = ref "" 
       in
       begin
	 if size_genrel_arithm_deeper_than ed 2 then
	   begin
	     pprint_outputd := "("^(nts_pprint_genrel_arithm_exp ed)^")";
	   end
	 else
	   pprint_outputd := nts_pprint_genrel_arithm_exp ed;
       end;
       begin
	 if size_genrel_arithm_deeper_than eg 2 then
	   begin
	     pprint_outputg := "("^(nts_pprint_genrel_arithm_exp eg)^")";
	   end
	 else
	   pprint_outputg := nts_pprint_genrel_arithm_exp eg;
       end;
       (!pprint_outputg)^"-"^(!pprint_outputd)
      end
       
   | CntGenArithmBOp(  CntGenProd , eg , ed )
	-> 
     begin
       let pprint_outputd = ref ""
       in
       let pprint_outputg = ref "" 
       in
       begin
	 if size_genrel_arithm_deeper_than ed 1 then
	   begin
	     pprint_outputd := "("^(nts_pprint_genrel_arithm_exp ed)^")";
	   end
	 else
	   pprint_outputd :=nts_pprint_genrel_arithm_exp ed;
       end;
       begin
	 if size_genrel_arithm_deeper_than eg 1 then
	   begin
	     pprint_outputg := "("^(nts_pprint_genrel_arithm_exp eg)^")";
	   end
	 else
	   pprint_outputg := nts_pprint_genrel_arithm_exp eg;
       end;
       (!pprint_outputg)^"*"^(!pprint_outputd)
     end
       
    
   | CntGenArithmBOp( CntGenMod ,  eg , ed )
	-> 
     begin
       let pprint_outputd = ref ""
       in
       let pprint_outputg = ref "" 
       in
       begin
	 if size_genrel_arithm_deeper_than ed 2 then
	   begin
	     pprint_outputd := "("^(nts_pprint_genrel_arithm_exp ed)^")";
	   end
	 else
	   pprint_outputd := nts_pprint_genrel_arithm_exp ed;
       end;
       begin
	 if size_genrel_arithm_deeper_than eg 2 then
	   begin
	     pprint_outputg := "("^(nts_pprint_genrel_arithm_exp eg)^")";
	   end
	 else
	   pprint_outputg := nts_pprint_genrel_arithm_exp eg;
       end;
       (!pprint_outputg)^"%"^(!pprint_outputd)
     end 

   | CntGenInvalidExp -> raise Invalid_nts_expression
     




let pprint_gen_rel_arithm_list larithm =
  let rec pprint_nts_var_list_fold str l =
    match str, l with 
	(_,[]) -> str
      | ("",(h::l')) -> pprint_nts_var_list_fold (nts_pprint_genrel_arithm_exp h) l'
      | (_,(h::l')) -> pprint_nts_var_list_fold (str^","^(nts_pprint_genrel_arithm_exp h)) l' 
  in
  (pprint_nts_var_list_fold "" larithm)
  
  




(* Print the conjunction or the disjunction of lhs/rhs *)
let nts_pprint_bool_binop ( lhs : string) bop (rhs : string) =
  match bop with
      CntGenBAnd -> lhs ^ " and " ^ rhs
    | CntGenBOr -> lhs ^ " or " ^ rhs


let nts_pprint_aritm_binop  bop  =
  match bop with
      CntGenSum -> "+"
    | CntGenMinus -> "-"
    | CntGenProd -> "*"
    | CntGenDiv -> "/"
    | CntGenMod -> "%"

(* reduce code size using cnt_gen_bool_binop type*)
let rec nts_pprint_genrel (bexp : nts_gen_relation ) =
  match bexp with 
      CntGenTrue -> "true"
    | CntGenFalse-> "false"
    | CntGenNot ( exp ) ->
      if size_genrel_deeper_than exp 0 then
	"not ("^(nts_pprint_genrel exp)^")"
      else 
	"not "^(nts_pprint_genrel exp)
	  
    |  CntGenRelComp(CntGenBAnd, eg , ed )
      -> 
      begin
	let pprint_outputd = ref ""
	    in
	let pprint_outputg = ref "" 
	in
	begin
	  if size_genrel_deeper_than ed 1 then
	    begin
	      pprint_outputd := "("^(nts_pprint_genrel ed)^")";
	    end
	  else
	    pprint_outputd := nts_pprint_genrel ed;
	end;
	begin
	  if size_genrel_deeper_than eg 1 then
	    begin
	      pprint_outputg := "("^(nts_pprint_genrel eg)^")";
	    end
	  else
	    pprint_outputg := nts_pprint_genrel eg;
	end;
	(!pprint_outputg)^" and "^(!pprint_outputd)
      end

    |  CntGenRelComp ( CntGenBOr , eg , ed )
      -> 
      begin
	let pprint_outputd = nts_pprint_genrel ed 
	in
	let pprint_outputg = nts_pprint_genrel eg
	in
	(pprint_outputg)^" or "^(pprint_outputd)
      end
	
    | CntGenRel ( bop , expg , expd ) ->
      begin
	let expg = nts_pprint_genrel_arithm_exp expg 
	in
	let expd =  nts_pprint_genrel_arithm_exp expd 
	in
	match bop with
	    CntEq ->  expg^" = "^expd
	  | CntNeq ->  expg^" != "^expd
	  | CntLeq -> expg^" <=  "^expd
	  | CntLt -> expg^" < "^expd
	  | CntGt -> expg^" > "^expd
	  | CntGeq -> expg^" >= "^expd
      end
    | CntQVarsGenRel (vlist,quantifier, subformula) ->
      begin
	let qvars_list_print =  pprint_typeinfo_nts_genvar_list vlist in
	let q_print = pprint_nts_quantifier quantifier in
	let pprint_sformula = nts_pprint_genrel subformula in
	Format.sprintf "%s %s . %s" q_print qvars_list_print pprint_sformula
      end
	


(* This function answers yes whenever r is a boolean relation,
i.e. when all variables are unprimed*)

let boolean_relation r =
  let unprimed_var_checker x =
    match x with
        NtsGenVar(_,NtsPrimed) -> raise Found_a_primed_var
      | NtsGenVar(_,NtsUnPrimed) -> ()
  in
  let rec primeless_arithm_express p =
    match p with
	CntGenVar(n) ->  unprimed_var_checker n
      | CntGenNdet | CntGenNdetVar(_) 
      | CntGenSymCst(_) -> ()
      | CntGenArithmBOp(_,a,b) -> 
	( primeless_arithm_express a);
	( primeless_arithm_express b) 
      | CntGenArithmUOp(_,a) ->  primeless_arithm_express a 
      | CntGenInvalidExp -> raise  Invalid_nts_expression
  in
  try
    primeless_arithm_express r; true
  with
      Found_a_primed_var -> false
    
    
(* Syntactic simplification of boolean expressions *)
  let simplify_genrel_bottom_top (e : nts_gen_relation ) = 
    match e with
      | CntGenRelComp(CntGenBAnd,CntGenFalse,_) -> CntGenFalse
      | CntGenRelComp(CntGenBAnd,_,CntGenFalse) -> CntGenFalse
      | CntGenRelComp(CntGenBAnd,CntGenTrue,a) ->  a
      | CntGenRelComp(CntGenBAnd,a,CntGenTrue) ->  a
      | CntGenNot(CntGenTrue) -> CntGenFalse
      | CntGenNot(CntGenFalse) -> CntGenTrue
      | CntGenNot(CntGenNot(a)) -> a
      | CntGenRelComp(CntGenBOr,_,CntGenTrue) -> CntGenTrue
      | CntGenRelComp(CntGenBOr,CntGenTrue,_) -> CntGenTrue
      | CntGenNot(CntGenRel(CntEq,a,b)) -> (CntGenRel(CntNeq,a,b))
      | CntGenNot(CntGenRel(CntNeq,a,b)) -> (CntGenRel(CntEq,a,b))
      | CntGenNot(CntGenRel(CntLt,a,b)) -> (CntGenRel(CntGeq,a,b))
      | CntGenNot(CntGenRel(CntGt,a,b)) -> (CntGenRel(CntLeq,a,b))
      | CntGenNot(CntGenRel(CntLeq,a,b)) -> (CntGenRel(CntGt,a,b))
      | CntGenNot(CntGenRel(CntGeq,a,b)) -> (CntGenRel(CntLt,a,b))	
      | _ -> e

	
  let rec simplify_gen_rel ( e : nts_gen_relation ) =
    match e with
      | CntGenRelComp(CntGenBAnd,CntGenFalse,_) -> CntGenFalse
      | CntGenRelComp(CntGenBAnd,_,CntGenFalse) -> CntGenFalse
      
      | CntGenRelComp(CntGenBOr,CntGenTrue,_) -> CntGenTrue
      | CntGenRelComp(CntGenBOr,_,CntGenTrue) -> CntGenTrue	
      
      
      | CntGenNot(CntGenNot(a)) -> 
	simplify_gen_rel a

      | CntGenRelComp(CntGenBAnd,a,b) -> 
	let fg = simplify_gen_rel a in
	let fd = simplify_gen_rel b in
	simplify_genrel_bottom_top (CntGenRelComp(CntGenBAnd,fg,fd))
	
	  
      | CntGenRelComp(CntGenBOr,a,b) -> 
	let fg = simplify_gen_rel a in
	let fd = simplify_gen_rel b in
	simplify_genrel_bottom_top (CntGenRelComp(CntGenBOr,fg,fd))		  
      | CntGenNot(a) -> 
	let a = simplify_gen_rel a in
	simplify_genrel_bottom_top (CntGenNot(a))
			 
      | CntGenTrue -> CntGenTrue
      | CntGenFalse -> CntGenFalse
	
      | _ -> e



(**********************)
(* Those mutually recursive functions answers yes whenever the bool, res. the 
arithmetic val, only depends on the variable evaluation --and not of 
non-determinitic variables.*)

let rec is_gen_bool_det ( b : nts_gen_relation ) =
  match b with
    | CntGenTrue -> true
    | CntGenFalse -> true
    
    | CntGenRelComp (_,a,b) ->  
      let ndet_fg = is_gen_bool_det a in
      let ndet_fd = is_gen_bool_det b in
      ndet_fg && ndet_fd
	
 
    | CntGenNot (a) -> is_gen_bool_det a
      
    | CntGenRel(_,a,b) -> 
      let det_fg = is_gen_arithm_exp_a_function a in
      let det_fd = is_gen_arithm_exp_a_function b in
      det_fg && det_fd

(* Does the arithmetic expression have a deterministic evalution, i.e.
contains no CntNdet constructor *)	
and is_gen_arithm_exp_a_function (e : nts_genrel_arithm_exp ) =
  match e with
   | CntGenNdet -> false
   | CntGenNdetVar(_) -> false
   | CntGenArithmBOp(_,fg,fd) ->   
     let det_fg = is_gen_arithm_exp_a_function fg in
     let det_fd = is_gen_arithm_exp_a_function fd in
     det_fg && det_fd
   | CntGenArithmUOp (_,a) -> is_gen_arithm_exp_a_function a
   | CntGenInvalidExp -> raise CntInvalidExpression
   | _-> true

(**********************)


 (* Answers true if the expression can be sytacticaly evalutated to false.
 An answers to false means that e shall be evaluated at runtime, and might
be equal CntBFalse.
*)

let static_check_if_gen_relation_false ( e : nts_gen_relation  ) =
  let es =  simplify_gen_rel e in
  match es with
      CntGenFalse -> true
    | _ -> false


let static_check_if_gen_translist_unsat ( l : nts_trans_label list) =
  let decide_folder unsat_previous current_label =
    if unsat_previous then true
    else
      match current_label with 
	  CntGenGuard(cond) ->
	    (static_check_if_gen_relation_false cond)
	| _ -> false (* Non guard transition are not unsat*)
  in
  List.fold_left decide_folder false l 
    

(*       *)
	    
let nts_pprint_gen_trans_label ( tlabel : nts_trans_label ) =
  match tlabel with
      CntGenGuard ( nts_rel ) ->
	begin
	  nts_pprint_genrel nts_rel
	end
	  
    | CntGenHavoc(ntslist ) ->
      begin
	let strl = pprint_ntsgen_var_list ntslist in
	Format.sprintf "havoc(%s)" strl
      end
	
    | CntGenCall(nts_sys_name,lvals_opt_varlist,param_list) ->
      begin
	match lvals_opt_varlist with
	    None ->
	      Format.sprintf "%s(%s)" nts_sys_name (pprint_gen_rel_arithm_list param_list) 
	  | Some(lhslist) -> 
	    begin
	      let str_ret_var_list = 
		pprint_ntsgen_var_list lhslist in
	      let str_param = pprint_gen_rel_arithm_list 
		param_list in
	      Format.sprintf "(%s)=%s(%s)" str_ret_var_list 
		nts_sys_name str_param
	    end
      end
	

(*Is the guard a single true ? *)
let is_label_true t =
  match t with
      CntGenGuard(CntGenTrue) -> true
    | _ -> false
      


let nts_pprint_gen_trans_label_list ( tlabellist : nts_trans_label list ) =
  let folder_of_the_day s tlabel =
    match s with
	"" ->
	  begin
	    if  not ( is_label_true tlabel )
	     then
	      nts_pprint_gen_trans_label tlabel
	    else
	      ""
	  end
      | _ ->
	begin
	  if  not ( is_label_true tlabel ) 
	  then
	    s^" and "^( nts_pprint_gen_trans_label tlabel)
	  else 
	    s
	end
  in
  List.fold_left folder_of_the_day "" tlabellist




