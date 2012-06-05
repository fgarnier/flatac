open Nts_types
open Hashtbl




exception Var_name_already_used

let pprint_trans_list_foldleft (s : string ) ( trans : cnt_trans_label ) =
  match (s,trans) with 
    | ("",CntGuard(guard))-> 
      let s_guard = Nts.simplify_cnt_boolexp guard in
      (*let s_guard = guard in*)
      begin
	match s_guard with 
	    CntBTrue -> ""
	  | _ -> Nts.cnt_pprint_boolexp s_guard
      end
    | ("",_) ->
      (Nts.cnt_pprint_translabel trans )
    | (_,CntGuard(guard)) -> 
      let s_guard = Nts.simplify_cnt_boolexp guard in
      begin
	match s_guard with 
	    CntBTrue -> s
	  | _ -> s^ " and "^(Nts.cnt_pprint_boolexp s_guard) 
      end
	
    | (_,_) -> s^" and "^(Nts.cnt_pprint_translabel trans )
  



module type NTS_PARAM =
  sig
    type t         (*Type for key id of control states: e.g : int, string*)
    type anot_type (*Type for anotations*)
    val make_anot : 'a -> anot_type
    val pprint_keyid : t -> string 
    val pprint_anot : anot_type -> string (*Types for pprinting anotations*)
  end



module Make =
  functor( Param : NTS_PARAM )->
struct 
  type anotations = Nts_Anot of Param.anot_type
  type control = Nts_State of Param.t (* Control state *)
 
  let pprint_control c =
    match c with
	Nts_State(s) -> Param.pprint_keyid s

  let pprint_anotation a =
    match a with
	Nts_Anot(l)-> Param.pprint_anot l
      
  type nts_automaton =
      {
	mutable nts_automata_name : string; 
	mutable anot : anotations;
	states : (control , unit ) Hashtbl.t;
	init_states : (control , unit ) Hashtbl.t;
	final_states : (control , unit ) Hashtbl.t;
	error_states : (control , unit ) Hashtbl.t;
	mutable input_vars : nts_var list; (*Variable ordering is important*)
	mutable output_vars : nts_var list;
	mutable local_vars : nts_var list;
	transitions : (control, (control , cnt_trans_label list ) Hashtbl.t) Hashtbl.t ;
      }

  type nts_system = 
      {
	mutable nts_system_name : string;
	mutable nts_global_vars : nts_var list;
	mutable nts_automata : nts_automaton list;
      }

  (* Check whether a variable name is a global_var*)	
  let check_var_name_availability_in_cautomaton  vname ntsys c =
    let is_taken vname var =
      match var with
	  NtsIVar(vn)|NtsRVar(vn)
	    -> if (String.compare vn vname == 0)
	      then true
	      else
		false
    in
    let res =
      List.exists  (is_taken vname) c.intput_vars in
    let res =
      (res &&
	 List.exists  (is_taken vname) c.output_vars) in
    let res = ( res &&
		  List.exists (is_taken vname) c.local_vars )  
      
    in
    (
      res && 
	List.exits (is_taken vname ) ntsys.global_vars
    )
      
      
  let check_var_name_availability_in_ntsystem  vname ntsys  =
    let is_taken vname var =
      match var with
	  NtsIVar(vn)|NtsRVar(vn)
	    -> 
		  if (String.compare vn vname == 0)
		  then true
		  else
		    false
    in
    List.exits (is_taken vname ) ntsys.global_vars
	  


  let create_nts_system name =
    {
      nts_system_name = name;
      nts_global_vars = [];
      nts_automata = [];
    }

  let add_nts_int_vars_to_nts_system nts_sys 
      (vnames : string list) =
    List.iter (fun s -> begin
      if (check_var_name_availability_in_ntsystem s nts_sys)
	then	
	nts_sys.global_vars <- (NtsIVar(s)::nts_sys.global_vars)
      end			
      ) vnames
      
  let add_nts_real_vars_to_nts_system nts_sys
      (vnames : string list ) =
    List.iter (fun s -> begin
      if (check_var_name_availability_in_ntsystem s nts_sys)
	then	
	nts_sys.global_vars <- (NtsRVar(s)::nts_sys.global_vars)
      end		
      ) vnames


  let create_nts_automaton name = 
    let anot_i = Param.make_anot () in
    let states_i = Hashtbl.create size_hash in
    let final_states_i = Hashtbl.create size_hash in
    let init_states_i = Hashtbl.create size_hash in
    let error_states_i = Hashtbl.create size_hash in
    let transition_i = Hashtbl.create size_hash in
      {
	nts_automata_name = name;
	anot = Nts_Anot(anot_i);
	states = states_i;
	init_states = init_states_i;
	final_states = final_states_i;
	error_states = error_states_i;
	input_vars = [];
	output_vars = [];
	local_vars = [];
	transitions=transition_i;
      }
     

  let control_of_id_para p =
    NtsState (p)

  let rename_nts_automaton c name =
    c.nts_automata_name <- name

	
  let add_globvar_to_nts_system  gvar nts_sys =
    nts_sys.nts_global_vars <- gvar::(nts_sys.nts_global_vars) 
  let add_inputvar_left  (c : nts_automaton)  ( v : nts_var) =
    c.input_vars <- (v::c.input_vars)
    
  let add_outputvar_left c v =
    c.output_vars <- (v::c.output_vars)
     
  let add_init_state cautomata (s : control ) =
    if not (Hashtbl.mem cautomata.init_states s)
    then Hashtbl.add cautomata.init_states s ()
    else ()

  let add_error_state cautomata (s : control ) =
    if not (Hashtbl.mem cautomata.error_states s)
    then Hashtbl.add cautomata.error_states s ()
    else ()
      
  let add_final_state cautomata (s : control ) =
    if not (Hashtbl.mem cautomata.error_states s)
    then Hashtbl.add cautomata.error_states s ()
    else ()

  (**I dont check the unicity of s1->l->s2 transition.
     should I ?
  *) 
  let add_transition (cautomata : nts_automaton) 
    ( orig : control ) (dest : control) ( lab : cnt_trans_label list) =
    let master_rel = cautomata.transitions in
    if Hashtbl.mem master_rel orig then
      begin
	let orig_binding = Hashtbl.find master_rel orig
	in 
	  Hashtbl.add orig_binding dest lab
      end
    else
      begin
	let orig_binding = Hashtbl.create size_hash in
	  Hashtbl.add orig_binding dest lab;
	  Hashtbl.add master_rel orig orig_binding
      end


  (**Returns the collection of transitions betwenn sorg and sdests
     The result has type cnt_translabel list list
  *)
  let get_transition_from  cautomata sorg sdest =
    if not (Hashtbl.mem cautomata.transitions sorg)
    then None
    else 
      begin
	let origin_table = Hashtbl.find cautomata.transitions sorg in
	  try
	    let transitions = Hashtbl.find_all origin_table sdest in
	      Some(transitions)
	  with
	      Not_found -> None
      end
  (*None is returned if no transition exists*)


  let pprint_inputvars cautomata = 
     Nts.pprint_typeinfo_nts_var_list cautomata.input_vars
       
  let pprint_outputvars cautomata =
    Nts.pprint_typeinfo_nts_var_list cautomata.output_vars

  let pprint_localvars cautomata =
    Nts.pprint_typeinfo_nts_var_list cautomata.local_vars
    
  let pretty_label tlist =
    let  str = List.fold_left pprint_trans_list_foldleft "" tlist in
      str
  
  (* The function below need not appear in the interface file*)
  let pprint_initial_states c =
     let elem_left = ref 0 in
      let pprint_folder id () prescript =
	if !elem_left <= 1 then
	  prescript^(Format.sprintf "s%s" ( pprint_control id ))
	else
	  begin
	    elem_left := !elem_left-1;
	    prescript^(Format.sprintf "s%s," ( pprint_control id ))
	  end
      in
      elem_left := (Hashtbl.length c.init_states);
	if !elem_left >0 then
	  let retstring = Hashtbl.fold pprint_folder c.init_states ""
	  in
	    "initial "^retstring^";"
	else
	  ""

  let pprint_final_states c =
    let elem_left = ref 0 in
    let pprint_folder id () prescript =
      if !elem_left <= 1 then
	prescript^(Format.sprintf "s%s" ( pprint_control id ))
      else
	begin
	  elem_left := !elem_left-1;
	  prescript^(Format.sprintf "s%s," ( pprint_control id ))
	end
    in
      elem_left := (Hashtbl.length c.final_states);
      if !elem_left >0 then
	let retstring = Hashtbl.fold pprint_folder c.final_states ""
	in
	  "final "^retstring^";"
      else
	""
	    
  let pprint_error_states c =
    let elem_left = ref 0 in
    let pprint_folder id () prescript =
      if !elem_left <= 1 then
	prescript^(Format.sprintf "s%s" ( pprint_control id ))
      else
	begin
	  elem_left := !elem_left-1;
	  prescript^(Format.sprintf "s%s," ( pprint_control id ))
	end
    in
      elem_left := (Hashtbl.length c.error_states);
      if !elem_left >0 then
	let retstring = Hashtbl.fold pprint_folder c.error_states ""
	in
	  "error "^retstring^";"
      else
	""


  let pprint_transitions (prescript :string) (cautomata : nts_automaton )=
    let intermediate_sid = ref 0 in
    let dest_table_print_folder ( origin : control ) (dest : control ) label 
	(prescript : string ) =
      if (Nts.static_check_if_translist_unsat label) 
      then prescript 
      else
	if not (Nts.need_split_transition label) 
	then
	  begin
	    let label = Nts.rewrite_ndet_assignation label in
	    let label = Nts.havocise label in
	    let post_script = Format.sprintf "%s \n s%s->s%s { %s }" prescript ( pprint_control origin)  ( pprint_control dest) 
	      (pretty_label label)
	    in 
	      post_script
	  end
	       
	else
	  begin
	    let label =  Nts.rewrite_ndet_assignation label in
	    let (pre,post) = Nts.split_guard_call_transition label in
	       let pre= Nts.havocise_label pre in
	       let post = Nts.havocise_label post in
	       let post_script = Format.sprintf "%s \n s%s->sinter%d { %s }" prescript   ( pprint_control origin) ( !intermediate_sid)
		 (pretty_label pre) in
	       let post_script=Format.sprintf "%s \n sinter%d->s%s { %s }" post_script ( !intermediate_sid)  ( pprint_control dest) 
		 (pretty_label post) in
	       intermediate_sid:=!intermediate_sid + 1;
	       post_script
	     end 

      in
      let origin_table_print_folder (origin : control ) table_dest 
	  (pre_script :  string ) =
	Hashtbl.fold (dest_table_print_folder origin) table_dest pre_script
      in
      Hashtbl.fold origin_table_print_folder cautomata.transitions prescript


  let pprint_to_nts cautomata = 
      (* let current_ecfg_node = Hashtbl.get vertex current_vertex_id in *)
      let res_string = cautomata.nts_automata_name^"{\n" in
      let res_string = (
	if List.length cautomata.input_vars > 0 then
	res_string^"in "^(pprint_inputvars cautomata )^";\n"
	else res_string
      )
      in
      let pprint_loc = pprint_localvars cautomata in
      let res_string=res_string^"\n"^Nts.concat_if_first_arg_nonzero pprint_loc " : int ;\n" in
    
      
      let ret_vars = pprint_outputvars cautomata in
      Format.printf "Outvars are : %s \n" ret_vars;
      let res_string =  (
	if String.length ret_vars > 0 
	then res_string^"\n"^ret_vars^"\n"
	else
	  res_string
      ) 
      in
      let res_string = res_string^((pprint_initial_states cautomata))^"\n"  in
      let res_string = res_string^((pprint_final_states cautomata))^"\n" in
      let res_string = res_string^((pprint_error_states cautomata)) in
      let res_string = res_string^((pprint_transitions "" cautomata))
      in
      let res_string = res_string^"\n}" in
      res_string

    
end
