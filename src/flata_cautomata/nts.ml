open Nts_types
open Queue

exception CntInvalidExpression

(*  
This files contains the functions used to deal with Numerical Transition
Systems, a.k.a. counter automata.

_ Translating intermediate language into nts language.

If you have any questions or suggestions, please
type the following address starting from the
rightmost caracter until you reach the leftmost
one in your favorite mail editor : rf.gami-at-reinrag.tnerolf,
and then send me your message.
*)



let is_ntisvar_det v =
  match v with
      NtsINdetVar(_) -> false
    | _ -> true


(* This part defines the function used to export the nts trees into
a NTS compliant syntax -- as well as being human readable.*)




let negate_cntbool_shallow ( b : cnt_bool) =
  match b with
      CntBTrue -> CntBFalse
    | CntBFalse -> CntBTrue
    | CntNot(a)-> a
    | _ -> CntNot(b)


let nts_pprint_nts_var (x : nts_var ) = 
  match x with 
      NtsIVar( vname ) -> vname
    | NtsRVar ( vname ) -> vname
    | NtsMiscType ( vname ) -> vname
  (*  | NtsArray (vname,size,_) -> Format.printf "%s[%d]" vname size*)


let rec nts_pprint_nts_typeinfo_var ( x :nts_var) =
  match x with 
      NtsIVar( vname ) -> vname^" :int "
    (*| NtsBVar( vname ) ->  vname^":  bool"*)
    | NtsRVar ( vname ) ->vname^" :real "
    | NtsMiscType ( vname ) ->vname^" :non scalar"
   (* | NtsArray ( name, size , base_type) -> "Tab : "^vname^"["^(Format.printf "%d" size)^"] "^(nts_pprint_nts_typeinfo_var base_type )*)


let pprint_typeinfo_nts_var_list l =
  (*let elem_left = ref (List.length l) in*)
  let pprint_lfold res var =
    (*if !elem_left > 1 then 
      begin 
	elem_left := (!elem_left - 1);*)
	res^(nts_pprint_nts_typeinfo_var var)^";"
     (* end
    else
      res^(nts_pprint_nts_typeinfo_var var)*)
  in
  List.fold_left pprint_lfold  "" l


(* Pretty prints the list of the names of a Nts variable list.*)
let pprint_nts_var_list l =
  let rec pprint_nts_var_list_fold str l =
    match str, l with 
	(_,[]) -> str
      | ("",(h::l')) -> pprint_nts_var_list_fold (nts_pprint_nts_var h) l'
      | (_,(h::l')) -> pprint_nts_var_list_fold (str^","^(nts_pprint_nts_var h)) l' 
  in
  (pprint_nts_var_list_fold "" l)

let pprint_nts_and_prime_var_list l =
  let rec pprint_nts_var_list_fold str l =
    match str, l with 
	(_,[]) -> str
      | ("",(h::l')) -> pprint_nts_var_list_fold ((nts_pprint_nts_var h)^"'") l'
      | (_,(h::l')) -> pprint_nts_var_list_fold (str^","^((nts_pprint_nts_var h)^"'")) l' 
  in
  (pprint_nts_var_list_fold "" l)
 


let valid_name_of_var (vname : string ) =
  "validity__"^vname^"_"  

let offset_name_of_var (vname : string ) =
  "offset__"^vname^"_"

let make_ntsvars_of_ptrvar (vname : string ) = 
  let val_name = valid_name_of_var vname in
  let offset_name =  offset_name_of_var vname in
  (NtsIVar(val_name))::(NtsIVar(offset_name)::[])



let make_ntsvars_of_intvars (vname : string) =
  let val_name = valid_name_of_var vname in 
  (NtsIVar(vname))::(NtsIVar(val_name)::[])




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

let pprint_typeinfo_int_nts_var_list l =
  let is_int_var  = function
  NtsIVar( vname ) -> true
    | _ ->false
  in
  let int_var_list = List.filter ( is_int_var) l in
  pprint_nts_var_list int_var_list

let pprint_typeinfo_nts_var_list l =
  let is_int_var  = function
  NtsIVar( vname ) -> true
    | _ ->false
  in
  let is_real_var  = function
  NtsRVar( vname ) -> true
    | _ ->false
  in
  let int_var_list = List.filter ( is_int_var) l in
  let real_var_list =  List.filter ( is_real_var) l in
  let pp_of_list_of_int =  
    concat_if_first_arg_nonzero (pprint_nts_var_list int_var_list) " : int" in
  let pp_of_list_of_real = 
    concat_if_first_arg_nonzero (pprint_nts_var_list real_var_list) " : real" in
  concat_comma_both_arg_non_empty pp_of_list_of_int pp_of_list_of_real
 
    
  
let rec size_arithm_exp ( exp : cnt_arithm_exp ) =
  match exp with 
       CntCst(_) -> 1
    | CntNdet(_) -> 1
    | CntNdetVar(_) -> 1
    | CntSymCst ( _ ) -> 1
    | CntVar (_) -> 1
    | CntInvalidExp -> 1
    | CntUnMin ( exp' ) -> 1 +    size_arithm_exp exp'
    | CntMinus ( eg ,  ed ) ->
      1 + max (size_arithm_exp eg ) (size_arithm_exp eg )
    | CntSum ( eg ,  ed ) ->
      1 + max (size_arithm_exp eg ) (size_arithm_exp eg )
    | CntProd ( eg ,  ed ) -> 
      1 + max (size_arithm_exp eg ) (size_arithm_exp eg )    
    | CntMod ( eg ,  ed ) -> 
      1 + max (size_arithm_exp eg ) (size_arithm_exp eg )       
    | CntDiv ( eg ,  ed ) -> 
      1 + max (size_arithm_exp eg ) (size_arithm_exp eg )       




(* This function answers true if there exists a subtree of exp which size
is greater or equal that deepness. We use this function to decide wheter
some expression shall be parenthesed or not. *)
let rec size_arithmexp_deeper_than  (exp : cnt_arithm_exp ) (deepness : int ) =
  if deepness <= 0 then true
  else 
    let deepness' = deepness - 1 in
    match exp with 
       CntCst(_)
      | CntNdet
    | CntSymCst (_ )
    | CntVar (_)
    | CntNdetVar(_)
    | CntInvalidExp -> false
    | CntUnMin ( exp' ) ->   size_arithmexp_deeper_than exp' deepness'
    | CntMinus ( eg ,  ed ) ->
      (size_arithmexp_deeper_than eg deepness' ) || (size_arithmexp_deeper_than ed deepness' )
    | CntSum ( eg ,  ed ) ->
     (size_arithmexp_deeper_than eg deepness' ) || (size_arithmexp_deeper_than ed deepness' )
    | CntProd ( eg ,  ed ) -> 
      (size_arithmexp_deeper_than eg deepness' ) || (size_arithmexp_deeper_than ed deepness' )
    | CntMod ( eg ,  ed ) -> 
      (size_arithmexp_deeper_than eg deepness' ) || (size_arithmexp_deeper_than ed deepness' ) 
    | CntDiv ( eg ,  ed ) -> 
      (size_arithmexp_deeper_than eg deepness' ) || (size_arithmexp_deeper_than ed deepness' )

let rec size_boolexp_deeper_than  (bexp : cnt_bool ) (deepness : int ) =
  if deepness <= 0 then
    true
  else
    let deep' = deepness - 1 in
    match bexp with
	CntBTrue -> false
      | CntBFalse -> false
      | CntNot ( exp' ) -> size_boolexp_deeper_than exp' deep'
      | CntBAnd ( eg ,  ed ) ->
	(size_boolexp_deeper_than eg deep' ) || (size_boolexp_deeper_than ed deep' )
      | CntBOr ( eg ,  ed ) ->
	(size_boolexp_deeper_than eg deep' ) || (size_boolexp_deeper_than ed deep' )
      | CntBool ( _, _ , _ ) -> false


let rec cnt_pprint_arithm_exp ( exp : cnt_arithm_exp ) =
  match exp with
      CntCst(i) -> My_bigint.to_string i
    | CntNdet -> "NDET"
    | CntSymCst(str) -> str
    | CntVar ( ntsvar ) -> nts_pprint_nts_var ntsvar
    | CntNdetVar(varname) -> varname
    | CntSum ( eg , ed ) ->
        (cnt_pprint_arithm_exp eg)^"+"^(cnt_pprint_arithm_exp ed)
    
    | CntUnMin (e ) ->
      begin
	match e with
	    CntUnMin( ploc ) -> cnt_pprint_arithm_exp ploc
	  | _  -> "-"^(cnt_pprint_arithm_exp e)
      end
    
    | CntMinus ( eg , ed )
	-> 
      begin
	if size_arithmexp_deeper_than ed 2 then
	  let pprint_output = 
	    (cnt_pprint_arithm_exp eg)^"-("^(cnt_pprint_arithm_exp ed)^")"
	  in
	  pprint_output
	else
	  (cnt_pprint_arithm_exp eg)^"-"^(cnt_pprint_arithm_exp ed)
      end
	
   | CntDiv ( eg , ed )
	-> 
     begin
       let pprint_outputd = ref ""
       in
       let pprint_outputg = ref "" 
       in
       begin
	 if size_arithmexp_deeper_than ed 2 then
	   begin
	     pprint_outputd := "("^(cnt_pprint_arithm_exp ed)^")";
	   end
	 else
	   pprint_outputd := cnt_pprint_arithm_exp ed;
       end;
        begin
	 if size_arithmexp_deeper_than eg 2 then
	   begin
	     pprint_outputg := "("^(cnt_pprint_arithm_exp eg)^")";
	   end
	 else
	   pprint_outputg := cnt_pprint_arithm_exp eg;
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
	 if size_arithmexp_deeper_than ed 1 then
	   begin
	     pprint_outputd := "("^(cnt_pprint_arithm_exp ed)^")";
	   end
	 else
	   pprint_outputd := cnt_pprint_arithm_exp ed;
       end;
        begin
	 if size_arithmexp_deeper_than eg 1 then
	   begin
	     pprint_outputg := "("^(cnt_pprint_arithm_exp eg)^")";
	   end
	 else
	   pprint_outputg := cnt_pprint_arithm_exp eg;
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
	 if size_arithmexp_deeper_than ed 2 then
	   begin
	     pprint_outputd := "("^(cnt_pprint_arithm_exp ed)^")";
	   end
	 else
	   pprint_outputd := cnt_pprint_arithm_exp ed;
       end;
        begin
	 if size_arithmexp_deeper_than eg 2 then
	   begin
	     pprint_outputg := "("^(cnt_pprint_arithm_exp eg)^")";
	   end
	 else
	   pprint_outputg := cnt_pprint_arithm_exp eg;
	end;
	(!pprint_outputg)^"%"^(!pprint_outputd)
     end 

   | CntInvalidExp -> raise Invalid_nts_expression






(*********************************************************************************************) 
(**** Simplification of CntBool expression : Elimination of tautologies, or false  bool expressions *)



  let simplify_bottom_top (e : cnt_bool ) = 
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



  let static_check_if_translist_unsat ( l : cnt_trans_label list) =
    let decide_folder unsat_previous current_label =
      if unsat_previous then true
      else
	match current_label with 
	    CntGuard(cond) 
	  | CntGuardIf(cond)
	  | CntGuardElse(cond) ->
	    (static_check_if_false cond)
	  | _ -> false
    in
    List.fold_left decide_folder false l 
  
      
      
  let rec cnt_pprint_boolexp (bexp :cnt_bool ) =
    match bexp with 
      	 CntBTrue -> "true"
	| CntBFalse-> "false"
	| CntNot ( exp ) ->
	  if size_boolexp_deeper_than exp 0 then
	    "not ("^(cnt_pprint_boolexp exp)^")"
	  else 
	    "not "^cnt_pprint_boolexp exp
	
	|  CntBAnd ( eg , ed )
	  -> 
	  begin
	    let pprint_outputd = ref ""
	    in
	    let pprint_outputg = ref "" 
	    in
	    begin
	      if size_boolexp_deeper_than ed 1 then
		begin
	     pprint_outputd := "("^(cnt_pprint_boolexp ed)^")";
		end
	      else
		pprint_outputd := cnt_pprint_boolexp ed;
	    end;
	    begin
	      if size_boolexp_deeper_than eg 1 then
		begin
		  pprint_outputg := "("^(cnt_pprint_boolexp eg)^")";
		end
	      else
		pprint_outputg := cnt_pprint_boolexp eg;
	    end;
	    (!pprint_outputg)^" and "^(!pprint_outputd)
	  end

       	|  CntBOr ( eg , ed )
	  -> 
	  begin
	    let pprint_outputd = cnt_pprint_boolexp ed 
	    in
	    let pprint_outputg = cnt_pprint_boolexp eg
	    in
	    (pprint_outputg)^" or "^(pprint_outputd)
	  end

	| CntBool ( bop , expg , expd ) ->
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


	    
  let cnt_simplify_and_pprint_boolexp ( bexp : cnt_bool) =
    (* Uncomment this to simplify *) 
    let bexp = simplify_cnt_boolexp bexp in
    cnt_pprint_boolexp bexp



(* This return true iff there is no constructor CntNDet in a CntBoolExpression*)
let rec is_cnt_bool_det ( b : cnt_bool ) =
  match b with
    | CntBTrue -> true
    | CntBFalse -> true
    
    | CntBAnd (a,b) ->  
      let ndet_fg = is_cnt_bool_det a in
      let ndet_fd = is_cnt_bool_det b in
      ndet_fg && ndet_fd
	
    | CntBOr (a,b) -> (* a and b need to be both *) 
      let ndet_fg = is_cnt_bool_det a in
      let ndet_fd = is_cnt_bool_det b in
      ndet_fg && ndet_fd	  
	
    | CntNot (a) -> is_cnt_bool_det a
      
    | CntBool(_,a,b) -> 
      let det_fg = is_cnt_arithm_exp_a_function a in
      let det_fd = is_cnt_arithm_exp_a_function b in
      det_fg && det_fd

(* Does the arithmetic expression have a deterministic evalution, i.e.
contains no CntNdet constructor *)	
and is_cnt_arithm_exp_a_function (e : cnt_arithm_exp ) =
  match e with
   | CntNdet -> false
   | CntNdetVar(_) -> false
   | CntMinus(a,b) | CntSum (a,b) 
   | CntProd(a,b) | CntMod (a,b) 
   | CntDiv (a,b) ->   
     let det_fg = is_cnt_arithm_exp_a_function a in
     let det_fd = is_cnt_arithm_exp_a_function b in
     det_fg && det_fd
   | CntUnMin (a) ->  	is_cnt_arithm_exp_a_function a
   | CntInvalidExp -> raise CntInvalidExpression
   | _-> true

	  
let pprint_il_args arg =
  match arg with 
      IlPtrArg(s) -> (cnt_pprint_arithm_exp s.offset_of_exp)^","^(cnt_pprint_arithm_exp s.validity_of_ptr_exp)
    | IlScalArg(s) -> (cnt_pprint_arithm_exp s.expr)^","^(cnt_pprint_arithm_exp s.validity_of_exp)


let arg_name_left_folder (str : string ) ( il_arg : il_fun_arguments) = 
  match str with
      "" -> pprint_il_args il_arg 
    | _ -> str^","^( pprint_il_args il_arg )
  
  

let cnt_pprint_translabel ( tlabel : cnt_trans_label ) =
  match tlabel with
      CntGuardIf(cbool) |
	  CntGuard ( cbool )| CntGuardElse(cbool)
	    -> cnt_simplify_and_pprint_boolexp cbool

   (* | CntGuardElse(cbool) -> cnt_simplify_and_pprint_boolexp cbool *)
      
	
    | CntAffect( ntvar ,  expr ) ->
       (nts_pprint_nts_var ntvar)^"'="^(cnt_pprint_arithm_exp expr)
    | CntFunCall ( funname, retval , largs ) ->
      begin
	match retval with
	    Some(list_varname) ->
	      begin
		"("^(pprint_nts_and_prime_var_list list_varname)^")="^funname^"("^(List.fold_left arg_name_left_folder "" largs )^")"
	      end
	  | None -> funname^"("^(List.fold_left arg_name_left_folder "" largs )^")"
      end
    | CntHavoc(ntslist ) -> let pp_list =  pprint_nts_var_list ntslist in
			    "havoc("^pp_list^")"
      
  
	    
 let need_split_transition label_list =
    let need_split_folder (has_guard,has_call) trans_label =
	match trans_label with
	    CntGuard(_) -> (true, has_call)
	  | CntFunCall(_,_,_) -> (has_guard, true)
	  | _ ->(has_guard,has_call)
    in
    let (a,b) = List.fold_left need_split_folder (false,false) label_list
    in
    a && b


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
	|  CntGuard(CntNot(CntBool(_,CntNdetVar("__ndet_cond__"),_)))
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
    
      (*Split a transition when both guards and fun call occurs*)
  let split_guard_call_transition (trans_label_list :  cnt_trans_label list ) =
    let is_a_call trans_label =
      match trans_label with
	 CntFunCall(_,_,_) -> true
	| _ -> false
    in 
    let tpre = List.filter (fun s -> (not( is_a_call s))) trans_label_list in
    let tpost = List.filter is_a_call trans_label_list in
    (tpre,tpost)
	  


 



  (* Replace every  argument of a function that is equal to NDet by
     an argument variable that is previous havocised.
  *)

      (*  number of variables added *)
     (* ( int *  il_fun_argument list ) *)
      
  let name_ndet_arg i =
    let name = Format.sprintf "_ndet_arg_%d" i in
    NtsIVar(name)

  let replace_ndet_args_by_ndet_counters ( ilfunlist :  il_fun_arguments list ) =
    let ndet_args = ref 0 in
    let replace_ndet_args_mapper (ilfunarg : il_fun_arguments ) =
      match ilfunarg with
	  
	  IlScalArg(iarg) ->
	    begin 
	      match iarg.expr with
		  CntNdet ->
		    begin
		      let ret_val =
			{
			  expr = CntVar((name_ndet_arg !ndet_args));
			  validity_of_exp = iarg.validity_of_exp;
			}
		      in
		      ndet_args := !ndet_args + 1;
		      IlScalArg(ret_val)
		    end
		      
		| _-> ilfunarg	  
	    end
	      
	| IlPtrArg(ptrarg) ->
	  begin 
	    match ptrarg.offset_of_exp with
		CntNdet ->
		  begin
		    let ret_val =
		      {
			base_of_exp = ptrarg.base_of_exp;
			offset_of_exp =  CntVar((name_ndet_arg !ndet_args));  
			validity_of_ptr_exp = ptrarg.validity_of_ptr_exp;
		      }
		    in
		    ndet_args := !ndet_args + 1;
		   IlPtrArg(ret_val)
		  end
	      | _ -> ilfunarg 	  
	  end
    in
    let modif_il_list = List.map replace_ndet_args_mapper  ilfunlist in
    (!ndet_args , modif_il_list)



  (* When guards deplends on non deteerministic values, transition :

   Guard(nd_cnd)              havoc(cnd_var)      Guard( cnd_var == 0)
  --------->       becomes  ( ----------------> . ---------------------->) 
*)


  let need_split_guard ( g : cnt_trans_label ) =
    match g with 
	CntGuard( condition ) ->
	  not (is_cnt_bool_det condition)

      | _ -> raise Not_Guard

	    
 (* let split_ndet_guard_transition (l : cnt_trans_label list) =
    
    let 
    let (pre,post)=([],l) in
 *) 
    
  

(* If c is not a deterministic condition, then it is replaced by a
   CntBool(CntEq,CntNderVar("__if_ndet_cond__"),CntCst(0)). Use
   for if then else label generation. The computed label is for
   the positive test of an if statment. Negate it to get the
   else test.
*)
  let format_cntcond_for_cfg_condition ( condition : cnt_bool ) =
    if is_cnt_bool_det condition 
    then condition
    else (*condition*)
      begin
	(*match condition with
	    CntNot(c) |  ->*)
	CntBool(CntEq,CntNdetVar("__ndet_cond__"),CntCst(My_bigint.zero))
	 (* | _ ->
	    CntBool(CntEq,CntNdetVar("__ndet_cond__"),CntCst(My_bigint.zero))
	 *)
      end
     

  let format_ifthen_cntcond_for_cfg ( guard : cnt_trans_label ) =
    match guard with
	CntGuardIf(cnd) ->
	  begin
	    if (is_cnt_bool_det cnd) 
	    then cnd
	    else
	      CntBool(CntEq,CntNdetVar("__if_ndet_cond__"),CntCst(My_bigint.zero))
	  end
      | CntGuardElse(cnd) ->
	begin
	  if is_cnt_bool_det cnd 
	  then cnd
	  else
	    CntNot(CntBool(CntEq,CntNdetVar("__if_ndet_cond__"),CntCst(My_bigint.zero)))
	end
      | _ -> raise Not_an_if_then_else_condition_guard 

  let build_argn_det_list (size : int ) =
    let rec rec_build_it index list =
      if index > 1 then
	rec_build_it (index -1) ((name_ndet_arg (index-1))::list) 
      else
	list 
    in
    rec_build_it size [] 
	
 

  let rewrite_ndet_assignation (l : cnt_trans_label list ) =
    let ndet_affect_folder ret_list transit =
      match transit with
	| CntFunCall(v,retval,arglist) ->
	  begin
	    let ( ndet_cnt , new_arg_list )= 
	      replace_ndet_args_by_ndet_counters  
		arglist 
	    in
	    if ndet_cnt == 0 then
	      transit::ret_list
	    else
	      begin
		let list_of_ndet_vars = build_argn_det_list ndet_cnt in
		(CntFunCall(v,retval,new_arg_list))::(CntHavoc(list_of_ndet_vars)::ret_list)
	      end
	  end
	| CntAffect(lhsvar, expr_val) ->
	  begin
	    
	    if (not  (is_cnt_arithm_exp_a_function expr_val) )
	    then
	      (* In this case, the lhs is assigne a value
	      that is abstracted as non deterministic. We shall
	      then not copy the lhs current value so that it is
	      considered as unknown in the sequel of this transition.*)
	      begin
		(CntHavoc((lhsvar::[]))::ret_list)
	      end
	    else 
	      transit::ret_list
	  end

	| CntGuard( condition) 
	  ->
	  begin
	    if is_cnt_bool_det condition
	    then
	      transit::ret_list
	    else
	      let condition = 
		format_cntcond_for_cfg_condition condition 
	      in 
	      (CntGuard(condition)::ret_list)
	    (* (CntGuard(condition)::((CntHavoc(NtsIVar("__if_ndet_cond__")::[]))::ret_list)) *)
	      
	  end

	| CntGuardIf(_) ->
	  CntGuardIf((format_ifthen_cntcond_for_cfg transit))::ret_list
	
	| CntGuardElse(_) ->
	  begin
	  CntGuardElse((format_ifthen_cntcond_for_cfg transit))::ret_list
	  end
	(*    transit::ret_list
	      end *)
	      
	| _ -> transit::ret_list
    in
    List.fold_left ndet_affect_folder [] l 
  (* Replace non deterministic
    guards by simpler non deterministic test, when possible.*)
    (*l*) (*Retuns the labels untouched*) 


  let havocise_label l =
    let l = rewrite_ndet_assignation l
    in		
      havocise l


  let split_guard_call_transition translabel =
     let translabel = 
       rewrite_ndet_assignation translabel in
       split_guard_call_transition translabel

     
(************** Implementation of the structural equality between nts
types. *********)


  let compare_nts_var (vg : nts_var ) (vd : nts_var ) =
    match vg,vd with 
	(NtsIVar(nameg),NtsIVar(named)) -> nameg=named
      | (NtsRVar(nameg),NtsRVar(named)) -> nameg=named
      |  _ -> false

	

  let rec compare_cnt_arithm_exp (eg : cnt_arithm_exp)(ed : cnt_arithm_exp) =
    match eg,ed with 
	(CntCst(cstg),CntCst(cstd)) -> My_bigint.equal cstg cstd
      | (CntNdet,CntNdet) -> true
      | (CntSymCst(sg),CntSymCst(sd)) -> (String.compare sg sd)=0
      | (CntVar(vg),CntVar(vd)) -> compare_nts_var vg vd
      | (CntNdetVar(sg),CntNdetVar(sd))-> (String.compare sg sd)=0

      | (CntMinus(egfg,egfd),CntMinus(edfg,edfd)) -> 
	(compare_cnt_arithm_exp egfg edfg)&&(compare_cnt_arithm_exp egfd edfd )
      | (CntSum(egfg,egfd),CntSum(edfg,edfd)) ->
	(compare_cnt_arithm_exp egfg edfg)&&(compare_cnt_arithm_exp egfd edfd )
      | (CntProd(egfg,egfd),CntProd(edfg,edfd)) ->
	(compare_cnt_arithm_exp egfg edfg)&&(compare_cnt_arithm_exp egfd edfd )
      | (CntMod(egfg,egfd),CntMod(edfg,edfd)) ->
	(compare_cnt_arithm_exp egfg edfg)&&(compare_cnt_arithm_exp egfd edfd )  
      | (CntDiv(egfg,egfd),CntDiv(edfg,edfd)) ->
	(compare_cnt_arithm_exp egfg edfg)&&(compare_cnt_arithm_exp egfd edfd )
      | (CntUnMin(fg),CntUnMin(fd)) ->
	compare_cnt_arithm_exp fg fd
      | (CntInvalidExp,CntInvalidExp) -> true
      
      | (_,_) -> false




  let rec compare_cnt_bool (bg : cnt_bool)(bd : cnt_bool ) =
    match bg,bd with
	(CntBTrue,CntBTrue) -> true
      | (CntBFalse,CntBFalse) -> true
     
      | ( CntBAnd(egfg, egfd),CntBAnd(edfg, edfd) )
	  -> (compare_cnt_bool egfg edfg)&&(compare_cnt_bool egfd edfd)
      
      | ( CntBOr(egfg, egfd),CntBOr(edfg, edfd) )
	-> (compare_cnt_bool egfg edfg)&&(compare_cnt_bool egfd edfd)	
	
      | (CntNot(a),CntNot(b)) ->
	(compare_cnt_bool a b)
	  
      | ( CntBool(bg,egfg, egfd),CntBool(fd,edfg, edfd) )
	-> 
	begin
	  if bg != fd then false
	  else
	    (compare_cnt_arithm_exp egfg edfg)&&
	      (compare_cnt_arithm_exp egfd edfd ) 
	end 
	  
      | _-> false



  let compare_il_int_fun_arg (arg1 : il_int_fun_arg) 
      (arg2 : il_int_fun_arg) =
    let eq_exp =  
      compare_cnt_arithm_exp arg1.expr arg2.expr in 
    let eq_val = compare_cnt_arithm_exp arg2.validity_of_exp 
      arg1.validity_of_exp 
    in
    eq_exp && eq_val


  let compare_il_ptr_fun_arg (arg1 : il_ptr_fun_arg) 
      (arg2 : il_ptr_fun_arg) =
    let eq_validity = compare_cnt_arithm_exp arg1.validity_of_ptr_exp 
      arg2.validity_of_ptr_exp in
    let eq_offset = compare_cnt_arithm_exp arg1.offset_of_exp
      arg2.offset_of_exp in
    eq_validity && eq_offset


  let rec compare_nts_var_list l1 l2 =
    match l1 , l2 with
	(a::_,[]) -> false
      | ([],a::_) -> false
      | ([],[]) -> true
      | (a::lg,b::ld) ->
	if compare_nts_var a b
	then compare_nts_var_list lg ld 
	else false
	  
	    
  let rec compare_il_fun_arguments (ilg : il_fun_arguments  list )
      (ild :il_fun_arguments list  ) =
    match  ilg , ild with
	(a::[],[]) -> false
      | ([],a::[]) -> false
      | ([],[]) -> true
      | (IlScalArg(a)::lg, IlScalArg(b)::ld) ->
	begin
	  if compare_il_int_fun_arg a b then
	    compare_il_fun_arguments lg ld
	  else false
	end
	  
      | (IlPtrArg(a)::lg, IlPtrArg(b)::ld) ->
	begin
	  if compare_il_ptr_fun_arg a b then
	    compare_il_fun_arguments lg ld
	  else false
	end  
      | (_,_) -> false
	
	    


  let compare_cnt_trans_label_guard 
      (gg : cnt_trans_label)( gd : cnt_trans_label ) =
    match gg,gd with
	(CntGuardIf(a),CntGuardIf(b)) 
      |   (CntGuardElse(a),CntGuardElse(b)) 
      |	(CntGuard(a),CntGuard(b)) -> 
		  compare_cnt_bool a b
	    
      | (CntAffect(varg,exprg),CntAffect(vard,exprd))
	-> 
	if not (compare_nts_var varg vard) 
	then false
	else
	  compare_cnt_arithm_exp exprg exprd
	

      
	
      | (CntFunCall(vg,Some(optg),argsg),CntFunCall(vd,Some(optd),argsd))->
	if ( String.compare vg vd ) != 0 then false
	else
	  begin
	    if not (compare_nts_var_list optg optd )
	    then
	      false 
	    else 
	      compare_il_fun_arguments argsg argsd
	  end
	    
      | (CntHavoc(vlistg),CntHavoc(vlisd))
	-> compare_nts_var_list vlistg vlisd
	
     
      | (CntNdetAssign(nvarg),CntNdetAssign(nvard))
	->
	compare_nts_var nvarg nvard

      |(_,_) -> false



  let rec compare_tranlabel_list l1 l2 =
     match  l1 , l2 with
	(a::_,[]) -> false
      | ([],a::_) -> false
      | ([],[]) -> true
      | (a::lg,b::ld) -> 
	begin
	  if not( compare_cnt_trans_label_guard a b )
	  then false
	  else compare_tranlabel_list lg ld
	end




(************** Hashing function for nts_label types and nts types in general **)

(* The rhs integer values are all primes numbers and shall be pairwise
distinct.*)


  let hash_nts_var 
      (t : nts_var ) =
    match t with 
	NtsIVar(s) ->  Hashtbl.hash s 
      | NtsRVar(s) ->  Hashtbl.hash s 
      | NtsMiscType(s) ->  Hashtbl.hash s 

  let hash_cnt_arithm_constructor (exp : cnt_arithm_exp ) =
    match exp with 
	CntSum (_,_) -> 59183 
      | CntMinus (_,_) -> 69341 
      | CntProd(_,_) -> 47797
      | CntMod (_,_) -> 104623
      | CntDiv(_,_) -> 503 
      | CntUnMin(_) -> 101527
      | CntNdet -> 7039
      | CntVar(_) -> 87797 
      | CntCst(_) -> 104659 
      | CntInvalidExp -> 70607	
      | CntSymCst(_) -> 613 
      | CntNdetVar(_) -> 21601
	  


  let hash_cnt_bool_constructor ( cb : cnt_bool) =
    match cb with
    | CntBTrue -> 5839  
    | CntBFalse ->  5827
    | CntBAnd (_,_) -> 90197 
    | CntBOr (_,_) -> 2293
    | CntBool ( opr , _ , _ ) ->
      begin
	match opr with 
	    CntEq -> 11833 
	  | CntNeq -> 10091 
	  | CntLeq -> 3691 
	  | CntLt -> 797    
	  | CntGt -> 73189 
	  | CntGeq -> 82561 
      end
    | CntNot(_) -> 4813	
    


  let hash_cnt_arithm_exp (visit_nodes_left : int)
      (t : cnt_arithm_exp ) =
    
    
    let breaths_hash_traversal (vnodes_left : int) 
	(next_generation : cnt_arithm_exp Queue.t) 
	(curr_expr : cnt_arithm_exp) =
     
      match curr_expr with
	  CntCst( cst ) -> (vnodes_left - 1, My_bigint.hash cst)

	| CntVar(v) ->  ( vnodes_left - 1 , hash_nts_var v)
	| CntNdet -> (vnodes_left , 7039)
	| CntInvalidExp -> ( vnodes_left , 78203) 
	| CntNdetVar(s) 
	
	| CntSymCst(s)
	  -> 
	  (vnodes_left - 1, Hashtbl.hash s)
	| CntMinus(fg,fd)  
	| CntProd(fg,fd)
	| CntSum(fg,fd) 
	| CntMod(fg,fd) 
	| CntDiv(fg,fd) ->	 
	  begin
	    Queue.add fg next_generation;
	    let local_hash = hash_cnt_arithm_constructor fg in
	    let vnodes_left = (vnodes_left - 2 ) in
	    if visit_nodes_left > 0 then
	      begin
		let local_hash =  (hash_cnt_arithm_constructor fd) + local_hash 
		in
		let local_hash = Hashtbl.hash local_hash 
		in 
		Queue.add fd next_generation;
		(vnodes_left, local_hash)
	      end 
	    else 
	      (vnodes_left, local_hash)
	  end
	| CntUnMin(f) ->
	    Queue.add f next_generation;
	    let local_hash = hash_cnt_arithm_constructor f
	    in
	      (vnodes_left-1, local_hash)
	      
	    
    in
    
    let genqueue = Queue.create() in
    Queue.add t genqueue;
    let global_hash = ref (hash_cnt_arithm_constructor  t) in
    let vnodes_left = ref visit_nodes_left in
    while ( !vnodes_left > 0 && not ( Queue.is_empty genqueue)) 
    do
      let exp_head = Queue.pop genqueue in
      let ( leftnodes , loc_hash ) =
	breaths_hash_traversal !vnodes_left genqueue exp_head in
      global_hash := Hashtbl.hash ( !global_hash + loc_hash );
      vnodes_left := leftnodes
    done;
    !global_hash
      




 (* The first parameter represents the maximum number of nodes to traverse
 in every syntactical arithmetical expression subtree*)

  let hash_cnt_bool_exp (visit_nodes_arithm : int) (visit_nodes_left : int)
      (t : cnt_bool ) =
    
    
    let breaths_hash_traversal (vnodes_left : int) 
	(next_generation : cnt_bool Queue.t) 
	(curr_expr : cnt_bool) =
      
      match curr_expr with
	  CntBTrue | CntBFalse -> 
	    (vnodes_left - 1, hash_cnt_bool_constructor curr_expr )
	    
	| CntNot(a) -> 
	  let local_hash = hash_cnt_bool_constructor a in
	  ( vnodes_left - 1 , local_hash)
    
    
	| CntBAnd(fg,fd)  | CntBOr(fg,fd) ->	 
	  begin
	    Queue.add fg next_generation;
	    let local_hash = hash_cnt_bool_constructor fg in
	    let vnodes_left = (vnodes_left - 2 ) in
	    if visit_nodes_left > 0 then
	      begin
		let local_hash =  (hash_cnt_bool_constructor fd) + local_hash 
		in
		let local_hash = Hashtbl.hash local_hash 
		in 
		Queue.add fd next_generation;
		(vnodes_left, local_hash)
	      end 
	    else 
	      (vnodes_left, local_hash)
	  end 
	
	| CntBool(rel,fg,fd) ->
	  begin
	    
	    let local_hash =  hash_cnt_arithm_exp visit_nodes_arithm fg in
	    let vnodes_left = (vnodes_left - 2 ) in
	    if visit_nodes_left > 0 then
	      begin
		let local_hash =  ( hash_cnt_arithm_exp visit_nodes_arithm fd) 
		  + local_hash 
		in
		let local_hash = Hashtbl.hash local_hash 
		in 
		(vnodes_left, local_hash)
	      end 
	    else 
	      (vnodes_left, local_hash)
	  end
    in
    
	  let genqueue = Queue.create() in
	  Queue.add t genqueue;
	  let global_hash = ref (hash_cnt_bool_constructor t) in
	  let vnodes_left = ref visit_nodes_left in
	  while ( !vnodes_left > 0 && not ( Queue.is_empty genqueue)) 
	  do
	    let cbool_head = Queue.pop genqueue in
	    let ( leftnodes , loc_hash ) =
	      breaths_hash_traversal !vnodes_left genqueue cbool_head in
	    global_hash := Hashtbl.hash ( !global_hash + loc_hash );
	    vnodes_left := leftnodes
	  done;
	  !global_hash



  let hash_il_fun_arg  m arg =
    match arg with
	IlScalArg(il_int) ->
	  begin
	    hash_cnt_arithm_exp m il_int.expr
	  end
      
      | IlPtrArg(il_ptr) ->  
	  begin
	    hash_cnt_arithm_exp m il_ptr.offset_of_exp
	  end


  let hash_cnt_trans_label (n : int) (m : int)
      (t : cnt_trans_label ) =
    match t with
	CntGuardIf(cbool) | CntGuardElse(cbool) |
	CntGuard(cbool) -> hash_cnt_bool_exp n m cbool
	 
      | CntFunCall(name,Some(list_ret),h::_) ->
	  begin
	    let hash_ret=Hashtbl.hash name in
	    let hash_ret = hash_nts_var (List.hd list_ret) in
	    let hash_ret=(hash_il_fun_arg m h)+hash_ret in
	      hash_ret
	  end
      | CntFunCall(name,Some(list_ret),[]) ->
	  begin
	    let hash_ret=Hashtbl.hash name in
	    let hash_ret = hash_nts_var (List.hd list_ret) in 
	      hash_ret
	  end
	    
      | CntFunCall(name,None,_) ->
	  begin
	    let hash_ret=Hashtbl.hash name 
	    in hash_ret
	  end
      |  CntAffect(x,e) -> (hash_nts_var x)+(hash_cnt_arithm_exp m e)
      |  CntNdetAssign(x) -> 31019+(hash_nts_var x )
      |  CntHavoc(a::_) -> 28109 +(hash_nts_var a )
      
      


  let hash_cnt_translabel_list (n : int )( m : int) 
      (tl : cnt_trans_label list) =

    let rec recursor elem_left lleft hash_prev =
      if elem_left <= 0 then hash_prev
      else
	match lleft with 
	    a::l ->  
	      let hash_loc = hash_cnt_trans_label n m a in
	      let hash_loc = Hashtbl.hash (hash_prev+hash_loc) in
		recursor (elem_left -1) l hash_loc
		  
	  | [] -> hash_prev
    in
      recursor n tl 0
    
