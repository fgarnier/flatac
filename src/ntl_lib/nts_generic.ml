open Nts_types
open Nts

exception Found_a_primed_var

let nts_pprint_genvar =
  (v,NtsPrimed) -> Format.sprintf "%s'" (nts_pprint_nts_var v)
  | (v,NtsUnPrimed) -> (nts_pprint_nts_var v)
  

let rec size_genrel_arithm_deeper_than 
    (barihtm : nts_genrel_arithm ) (depth : int ) =

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
    | CntGenUnMin ( exp' ) ->   size_genrel_arithm_deeper_than exp' depth'
    | CntGenMinus ( eg ,  ed ) ->
      (size_genrel_arithm_deeper_than eg depth' ) || (size_genrel_arithm_deeper_than ed depth' )
    | CntGenSum ( eg ,  ed ) ->
     (size_genrel_arithm_deeper_than eg depth' ) || (size_genrel_arithm_deeper_than ed depth' )
    | CntGenProd ( eg ,  ed ) -> 
      (size_genrel_arithm_deeper_than eg depth' ) || (size_genrel_arithm_deeper_than ed depth' )
    | CntGenMod ( eg ,  ed ) -> 
      (size_genrel_arithm_deeper_than eg depth' ) || (size_genrel_arithm_deeper_than ed depth' ) 
    | CntDiv ( eg ,  ed ) -> 
      (size_genrel_arithm_deeper_than eg depth' ) || (size_genrel_arithm_deeper_than ed deepness' )

  

(* This function is used to parse subtrees in a more human
readable fashion. *)

let rec size_genrel_deeper_than  (bexp : nts_gen_relation ) (depth : int ) =
  if deepth <= 0 then
    true
  else
    let deep' = depth - 1 in
    match bexp with
	CntGenTrue -> false
      | CntGenFalse -> false
      | CntGenNot ( exp' ) -> size_boolexp_deeper_than exp' deep'
      | CntGenAnd ( eg ,  ed ) ->
	(size_boolexp_deeper_than eg deep' ) || (size_boolexp_deeper_than ed deep' )
      | CntGenOr ( eg ,  ed ) ->
	(size_boolexp_deeper_than eg deep' ) || (size_boolexp_deeper_than ed deep' )
      | CntGenRel ( _, _ , _ ) -> false   



let rec nts_pprint_genrel_arithm_exp ( exp : cnt_genrel_arithm_exp ) =
  match exp with
      CntGenCst(i) -> Big_int.string_of_big_int i
    | CntGenNdet -> "NDET"
    | CntGenSymCst(str) -> str
    | CntGenVar ( ntsgenvar ) -> nts_pprint_genvar ntsgenvar
    | CntGenNdetVar(varname) -> varname
    | CntGenSum ( eg , ed ) ->
        (nts_pprint_genrel_arithm_exp eg)^"+"^(nts_pprint_genrel_arithm_exp ed)
    
    | CntGenUnMin (e ) ->
      begin
	match e with
	    CntGenUnMin( subtree ) -> nts_pprint_genrel_arithm_exp subtree
	  | _  -> "-"^(nts_pprint_genrel_arithm_exp e)
      end
    
    | CntGenMinus ( eg , ed )
	-> 
      begin
	if size ed 2 then
	  let pprint_output = 
	    (nts_pprint_genrel_arithm_exp eg)^"-("^(nts_pprint_genrel_arithm_exp ed)^")"
	  in
	  pprint_output
	else
	  (nts_pprint_genrel_arithm_exp eg)^"-"^(nts_pprint_genrel_arithm_exp ed)
      end
	
   | CntDiv ( eg , ed )
	-> 
     begin
       let pprint_outputd = ref ""
       in
       let pprint_outputg = ref "" 
       in
       begin
	 if size_genrel_deeper_than ed 2 then
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
  
   | CntProd ( eg , ed )
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

    
   | CntMod ( eg , ed )
	-> 
     begin
       let pprint_outputd = ref ""
       in
       let pprint_outputg = ref "" 
       in
       begin
	 if size_genrel_arithm_deeper_than ed 2 then
	   begin
	     pprint_outputd := "("^(nts_genrel_pprint_arithm_exp ed)^")";
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




(* Print the conjunction or the disjunction of lhs/rhs *)
let nts_pprint_bool_binop ( lhs : string) bop (rhs : string) =
  match bop with
      CntGenBAnd -> lhs^" and "^rhs
    | CntGenBOr -> lhs^" or "^rhs


let nts_pprint_aritm_binop  bop  =
  match bop with
      CntGenSum -> "+"
    | CntGenMinus -> "-"
    | CntGenProd -> "*"
    | CntGenDiv -> "/"
    | CntGenMod -> "%"

(* reduce code size using cnt_gen_bool_binop type*)
let rec nts_pprint_genrel (gexp : nts_gen_relation ) =
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
	let expg = cnt_pprint_arithm_exp expg 
	in
	let expd =  cnt_pprint_arithm_exp expd 
	in
	match bop with
	    CntEq ->  expg^" = "^expd
	  | CntNeq ->  expg^" != "^expd
	  | CntLeq -> expg^" <=  "^expd
	  | CntLt -> expg^" < "^expd
	  | CntGt -> expg^" > "^expd
	  | CntGeq -> expg^" >= "^expd
      end
	


(* This function answers yes whenever r is a boolean relation,
i.e. when all variables are unprimed*)

let boolean_relation r =
  let unprimed_var_checker (_,p) =
    match p with
        NtsPrimed -> raise Found_a_primed_var
      | NtsUnPrimed -> ()
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
      | CntInvalidExp -> raise  Invalid_nts_expression
  in
  try
    primeless_arithm_express r; true
  with
      Found_a_primed_var -> false
    
    
(* Syntactic simplification of boolean expressions *)
  let simplify_gen_bottom_top (e : cnt_bool ) = 
    match e with
      | CntGenRelComp(CntGenBAnd,CntGenFalse,_) -> CntGenFalse
      | CntGenRelComp(CntGenBAnd,_,CntGenBFalse,_) -> CntGenBFalse
      | CntGenRelComp(CntGenBAnd,CntGenTrue,a) ->  a
      | CntGenRelComp(CntGenAnd,a,CntBTrue) ->  a
      | CntGenNot(CntGenTrue) -> CntGenFalse
      | CntGenNot(CntGenFalse) -> CntGenTrue
      | CntGenNot(CntGenNot(a)) -> a
      | CntGenComp(CntGenBOr,_,CntBTrue) -> CntGenTrue
      | CntGenComp(CntGenBOr,CntGenTrue,_) -> CntGenTrue
      | CntGenComp(CntGenBAnd,CntGenFalse,CntGenFalse) -> CntGenFalse
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
      | CntNot(a) -> 
	let a = simplify_gen_rel a in
	simplify_genrel_bottom_top (CntGenNot(a))
			 
      | CntGenTrue -> CntGenTrue
      | CntGenFalse -> CntGenFalse
	
      | _ -> e

(*
 let nts_simplify_bottom_top (e : cnt_bool ) = 
    match e with
      | CntBAnd(CntBFalse,_) -> CntBFalse
      | CntBAnd(_,CntBFalse) -> CntBFalse
      | CntBAnd(CntBTrue,a) ->  a
      | CntBAnd(a,CntBTrue) ->  a
      | CntNot(CntBTrue) -> CntBFalse
      | CntNot(CntBFalse) -> CntBTrue
      | CntNot(CntNot(a)) -> a
      | CntBOr(_,CntBTrue) -> CntBTrue
      | CntBOr(CntBTrue,_) -> CntBTrue
      | CntBOr(CntBFalse,CntBFalse) -> CntBFalse
      | CntNot(CntBool(CntEq,a,b)) -> (CntBool(CntNeq,a,b))
      | CntNot(CntBool(CntNeq,a,b)) -> (CntBool(CntEq,a,b))
      | CntNot(CntBool(CntLt,a,b)) -> (CntBool(CntGeq,a,b))
      | CntNot(CntBool(CntGt,a,b)) -> (CntBool(CntLeq,a,b))
      | CntNot(CntBool(CntLeq,a,b)) -> (CntBool(CntGt,a,b))
      | CntNot(CntBool(CntGeq,a,b)) -> (CntBool(CntLt,a,b))	
      | _ -> e

	
  let rec simplify_cnt_boolexp ( e : cnt_bool ) =
    match e with
      | CntBAnd(CntBFalse,_) -> CntBFalse
      | CntBAnd(_,CntBFalse) -> CntBFalse
	
      | CntBOr(CntBTrue,_) -> CntBTrue
      | CntBOr(_,CntBTrue) -> CntBTrue

      | CntNot(CntNot(a)) -> 
	simplify_cnt_boolexp a

      | CntBAnd(a,b) -> 
	let fg = simplify_cnt_boolexp a in
	let fd = simplify_cnt_boolexp b in
	simplify_bottom_top (CntBAnd(fg,fd))
			  
      | CntBOr(a,b) ->
	let fg = simplify_cnt_boolexp a in
	let fd = simplify_cnt_boolexp b in
	simplify_bottom_top (CntBOr(fg,fd))
      
      | CntNot(a) -> 
	let a = simplify_cnt_boolexp a in
	simplify_bottom_top (CntNot(a))
			 
      | CntBTrue -> CntBTrue
      | CntBFalse -> CntBFalse
	
      | _ -> e
     
	*)

      (*| CntBAnd(CntBTrue,a) -> simplify_cnt_boolexp a
      | CntBAnd(a,CntBTrue) -> simplify_cnt_boilexp a
      | CntBAnd(CntBFalse,a) -> CntBFalse
      | CntBAnd(a,CntBFalse) -> CntBFalse
      | CntBOr(a,CntBTrue) -> CntBTrue
      | CntBOr(CntBTrue,a) -> CntBTrue *)		


(**********************)


 (* Answers true if the expression can be sytacticaly evalutated to false.
 An answers to false means that e shall be evaluated at runtime, and might
be equal CntBFalse.
*)
  let static_check_if_false ( e : cnt_bool  ) =
    let es =  simplify_cnt_boolexp e in
    match es with
	CntBFalse -> true
      | _ -> false

(*       *)
	    
let nts_pprint_gen_trans_label (l : cnt_trans_label ) =
  match l with
      CntCall
  
