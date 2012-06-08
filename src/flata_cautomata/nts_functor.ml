open Nts_types
open Hashtbl




exception Var_name_already_used
exception Found_var of nts_var
exception No_such_counter_automata_in_nts_system of string * string list


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
 
  let size_hash = 97
  let pprint_control c =
    match c with
	Nts_State(s) -> Param.pprint_keyid s

  (*let pprint = Param.pprint_keyid*)
  

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
        nts_automata : ( string , nts_automaton ) Hashtbl.t;
      }

  (*Need not appear in the API*)
  let get_cautomata_names_of_nts nts_sys =
    let key_name_folder vname _ retlist  =
      vname :: retlist
    in
    (Hashtbl.fold key_name_folder  nts_sys.nts_automata [])


  (* Check whether a variable name is a global_var*)	
  let check_var_name_availability_in_cautomaton  vname ntsys c  =
    let is_taken vname var =
      match var with
	  NtsIVar(vn) | NtsRVar(vn)
	    -> if (String.compare vn vname == 0)
	      then true
	      else
		false
    in
    let res =
      List.exists  (is_taken vname) c.input_vars in
    let res =
      (res &&
	 List.exists  (is_taken vname) c.output_vars) in
    let res = ( res &&
		  List.exists (is_taken vname) c.local_vars )  
      
    in
    (
      res && 
	List.exists (is_taken vname ) ntsys.nts_global_vars
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
    List.exists (is_taken vname ) ntsys.nts_global_vars
	  


  let create_nts_system name =
    {
      nts_system_name = name;
      nts_global_vars = [];
      nts_automata = Hashtbl.create size_hash;
    }

  let add_nts_int_vars_to_nts_system nts_sys 
      (vnames : string list) =
    List.iter (fun s -> begin
      if (check_var_name_availability_in_ntsystem s nts_sys)
	then	
	nts_sys.nts_global_vars <- (NtsIVar(s)::nts_sys.nts_global_vars)
      end			
      ) vnames
      
  let add_nts_real_vars_to_nts_system nts_sys
      (vnames : string list ) =
    List.iter (fun s -> begin
      if (check_var_name_availability_in_ntsystem s nts_sys)
	then	
	nts_sys.nts_global_vars <- (NtsRVar(s)::nts_sys.nts_global_vars)
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
     

  let add_cautomata_to_nts c nts_sys =
    Hashtbl.add nts_sys.nts_automata (c.nts_automata_name) c

  let control_of_id_param p =
    Nts_State (p)

  let rename_nts_automaton c nts_sys name =
    if not( Hashtbl.mem nts_sys.nts_automata c.nts_automata_name)
    then 
      begin
	let cautomaton_names =  get_cautomata_names_of_nts nts_sys in
	let except =  No_such_counter_automata_in_nts_system 
	  (name, cautomaton_names) in
	raise except
      end
    else
      begin
	let former_name = c.nts_automata_name in
	c.nts_automata_name <- name;
	Hashtbl.remove nts_sys.nts_automata former_name;
	Hashtbl.add nts_sys.nts_automata c.nts_automata_name c
      end
	
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
	
	
  (*  Search for a variable name, return Some(sort)  if referenced
      in the globals or within the cautomaton, or none if not found at 
      all 
  *)

  

  let get_varinfo nts_sys  (cname : string option) (vname : string) =
    let search_varname_iterator vname ntvar =
      match ntvar with
	| NtsIVar(name) | NtsRVar(name) ->
	  if (String.compare name vname )==0 then
	    raise (Found_var(ntvar))
	  else ()
    in
    try
      List.iter (search_varname_iterator vname) nts_sys.nts_global_vars;
      match cname with
	  Some(cname)-> 
	    begin
	      try
		let c = Hashtbl.find nts_sys.nts_automata cname 
		in
		List.iter (search_varname_iterator vname) c.input_vars;
		List.iter (search_varname_iterator vname) c.output_vars;
		List.iter (search_varname_iterator vname) c.local_vars;
		(*If found, the raised exception of type Found_var is
		handled in the topmost try ... with block.*)
	
		None (* This is the default value, i.e. matching variable*)
	      with
		  Not_found -> 
		    begin
		      let cautomata_name_list = 
			get_cautomata_names_of_nts nts_sys in
		      let ex = 
			No_such_counter_automata_in_nts_system
			  (cname,cautomata_name_list) in
		      raise ex
		    end   
	    end
	| None -> None
    with 
	Found_var v -> Some(v)



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
