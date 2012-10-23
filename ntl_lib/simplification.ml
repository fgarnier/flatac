(*  
Ntl lib, ocaml implementation.
(c) Verimag 2012.

Questions : florent dot garnier at gmail dot com

*)

open Nts_types

(** This function cleans up unused variable, i.e. all local variables
  that don't appear in any transition are stripped from the local variables
  lists.*)


type vars_entry = UVars_diary of ( string ,  unit ) Hashtbl.t
type vars_entry_by_name = UNamedVarsDiary of ( string , nts_var ) Hashtbl.t

let create_empty_var_diary () =
  let tbl = Hashtbl.create 11 in
  UVars_diary tbl

let get_diary_table tble =
  match tble with 
      UVars_diary(table) -> table

let get_var_name nvar =
  match nvar with 
      NtsIVar(vname) | NtsINdetVar(vname) | NtsRVar(vname) 
    |  NtsBVar(vname)  |  NtsMiscType(vname) | NtsNVar(vname) ->
      vname

let add_nts_var_to_diary diary nvar =
  
  let table = get_diary_table diary in
  let vname = get_var_name nvar in
  if (Hashtbl.mem table vname) then () 
  else
    begin
      Hashtbl.add table  vname ()
    end






      
let pprint_diary diary =
  let pprint_table var unit =
    Format.printf "var : %s \n" (var)
  in
  let table = get_diary_table diary in
  Hashtbl.iter pprint_table table


let contains_var diary nvar =
  let table = get_diary_table diary in
  let vname = get_var_name nvar in
    Hashtbl.mem table vname 



let contains_nts_genrel_var diary gen_var =
  match gen_var with 
      NtsGenVar(nvar,_) ->  contains_var diary nvar


let rec add_vars_of_genrel_arithm_exp_to_diary diary ( expr : nts_genrel_arithm_exp ) =
  
  match expr with
      CntGenCst _ | CntGenNdetVar _ 
    | CntGenSymCst _  | CntGenInvalidExp | CntGenNdet -> ()
      
    | CntGenVar (NtsGenVar(v,_)) ->
      add_nts_var_to_diary diary v
		
    | CntGenArithmBOp (_, g , d ) ->
      begin
	add_vars_of_genrel_arithm_exp_to_diary diary g;
	add_vars_of_genrel_arithm_exp_to_diary diary d
      end

    | CntGenArithmUOp (_, p ) ->
      begin
	add_vars_of_genrel_arithm_exp_to_diary diary p
      end
       


let rec add_vars_of_genrel_to_diary diary ( expr : nts_gen_relation ) =
  match expr with
      CntGenRel(_ ,expg ,expd ) ->
	begin
	  add_vars_of_genrel_arithm_exp_to_diary diary expg;
	  add_vars_of_genrel_arithm_exp_to_diary diary expd
	end
    | CntGenNot(rel) -> 
       add_vars_of_genrel_to_diary diary rel
	  
    | CntGenRelComp(_ ,expg , expd ) ->
      begin
	add_vars_of_genrel_to_diary diary expg;
	add_vars_of_genrel_to_diary diary expd
      end
	
    | CntQVarsGenRel(_,_,p) ->
      add_vars_of_genrel_to_diary diary p
	
    | CntGenTrue | CntGenFalse -> ()



let add_vars_of_lvals_to_diary diary (l : nts_genrel_var list option) =
  match l with
      None -> ()
    | Some lvals ->  
      List.iter 
	(fun s -> 
	  match s with NtsGenVar(v,_) 
	      -> add_nts_var_to_diary diary v
	) lvals	  
     


let add_vars_of_fun_parameters_to_diary diary 
    (l : nts_genrel_arithm_exp list) =
  List.iter (fun s ->  add_vars_of_genrel_arithm_exp_to_diary diary s) l
  



let add_vars_of_cnt_trans_label_to_diary diary ( lbl : nts_trans_label ) =
  match lbl with 
      CntGenGuard (ntgenrel) ->  
	add_vars_of_genrel_to_diary diary ntgenrel
    
    | CntGenCall (_,lvals,expr_list) ->
      begin
	add_vars_of_lvals_to_diary diary lvals;
	add_vars_of_fun_parameters_to_diary diary expr_list
      end
	
    (* Havoc list here for dealine, need not to count variables
    in havoc as used if they don't appear in relations.*)

    | CntGenHavoc (hlist) -> add_vars_of_lvals_to_diary diary (Some(hlist))



let add_vars_of_trans_label_list_to_diary diary (tlabel : nts_trans_label list ) =
  List.iter (fun s -> add_vars_of_cnt_trans_label_to_diary diary s ) tlabel


