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

let nts_pprint_nts_var (x : nts_var ) = 
  match x with 
      NtsIVar( vname ) -> vname
    | NtsRVar ( vname ) -> vname
    | NtsMiscType ( vname ) -> vname  


let nts_pprint_nts_typeinfo_var ( x :nts_var) =
  match x with 
      NtsIVar( vname ) -> vname^" :int "
    (*| NtsBVar( vname ) ->  vname^":  bool"*)
    | NtsRVar ( vname ) ->vname^" :real "
    | NtsMiscType ( vname ) ->vname^" :non scalar"


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

  
let rec size_arithm_exp ( exp : cnt_arithm_exp ) =
  match exp with 
       CntCst(_) -> 1
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
    | CntSymCst(str) -> str
    | CntVar ( ntsvar ) -> nts_pprint_nts_var ntsvar

    | CntSum ( eg , ed ) ->
        (cnt_pprint_arithm_exp eg)^"+"^(cnt_pprint_arithm_exp ed)
    
    | CntUnMin (e ) ->
      begin
	match e with
	    CntUnMin( ploc ) -> cnt_pprint_arithm_exp ploc
	  | _  -> cnt_pprint_arithm_exp e
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
	  if size_boolexp_deeper_than exp 2 then
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
	      if size_boolexp_deeper_than ed 2 then
		begin
	     pprint_outputd := "("^(cnt_pprint_boolexp ed)^")";
		end
	      else
		pprint_outputd := cnt_pprint_boolexp ed;
	    end;
	    begin
	      if size_boolexp_deeper_than eg 2 then
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





let arg_name_left_folder (str : string ) ( cnt_aexp : cnt_arithm_exp) = 
  match str with
      "" -> cnt_pprint_arithm_exp cnt_aexp 
    | _ -> str^","^( cnt_pprint_arithm_exp cnt_aexp )
  
  

let cnt_pprint_translabel ( tlabel : cnt_trans_label ) =
  match tlabel with
      CntGuard ( cbool ) -> cnt_pprint_boolexp cbool
    | CntAffect( ntvar ,  expr ) ->
       (nts_pprint_nts_var ntvar)^"'="^(cnt_pprint_arithm_exp expr)
    | CntFunCall ( funname, retval , largs ) ->
      begin
	match retval with
	    Some(NtsIVar(varname)) ->
	      begin
		varname^"'="^funname^"("^(List.fold_left arg_name_left_folder "" largs )^")"
	      end
	  | None -> funname^"("^(List.fold_left arg_name_left_folder "" largs )^")"
      end
      
  
	    
	  
