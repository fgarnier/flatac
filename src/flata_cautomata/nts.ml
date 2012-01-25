open Nts_types


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


(* This part defines the function used to export the nts trees into
a NTS compliant syntax -- as well as being human readable.*)

let rec nts_pprint_nts_var (x : nts_var ) = 
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
      CntCst(i) -> let s = Format.sprintf "%d" i in s
    | CntNdet -> "NDET"
    | CntSymCst(str) -> str
    | CntVar ( ntsvar ) -> nts_pprint_nts_var ntsvar

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




let rec cnt_pprint_boolexp (bexp :cnt_bool ) =
  match bexp with 
      	 CntBTrue -> "true"
	| CntBFalse-> "false"
	| CntNot ( exp ) ->
	  if size_boolexp_deeper_than exp 1 then
	    "not ("^(cnt_pprint_boolexp exp)^")"
	  else 
	    "not"^cnt_pprint_boolexp exp
	
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
      CntGuard ( cbool ) -> cnt_pprint_boolexp cbool
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
	| CntAffect(_,CntNdet)-> false
	| _ -> true
    in
    let modified_vars (var_list : Nts_types.nts_var list) 
	(trans_label : cnt_trans_label) =
      match trans_label with	
	| CntAffect(nvar,_) -> nvar::var_list
	| CntFunCall(_,Some(nvar_list),_) -> nvar_list@var_list
	| CntHavoc (nvlist) -> nvlist@var_list
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

	| _ -> transit::ret_list
    in
    List.fold_left ndet_affect_folder [] l
