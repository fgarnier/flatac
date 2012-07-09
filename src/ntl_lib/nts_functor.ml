open Nts_types
open Hashtbl
open Nts_generic



exception Var_name_already_used
exception Found_genvar of nts_genrel_var
exception No_such_counter_automata_in_nts_system of string * string 
exception UnboundVarName of string 

let pprint_trans_list_foldleft (s : string ) ( trans : nts_trans_label ) =
  match (s,trans) with 
    | ("",CntGenGuard(guard))-> 
      let s_guard = Nts_generic.simplify_gen_rel guard in
      begin
	match s_guard with 
	    CntGenTrue -> ""
	  | _ -> Nts_generic.nts_pprint_genrel s_guard
      end
    | ("",_) ->
      (Nts_generic.nts_pprint_gen_trans_label trans )
    | (_,CntGenGuard(guard)) -> 
      let s_guard = Nts_generic.simplify_gen_rel guard in
      begin
	match s_guard with 
	    CntGenTrue -> s
	  | _ -> s^ " and "^(Nts_generic.nts_pprint_genrel s_guard) 
      end
	
    | (_,_) -> s^" and "^(Nts_generic.nts_pprint_gen_trans_label trans )
  



module type NTS_PARAM =
  sig
    type t         (*Type for key id of control states: e.g : int, string*)
    type anot_type (*Type for anotations*)
    val anot_parser : unit -> anot_type
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
	(*states : (control , unit ) Hashtbl.t;*)
	init_states : (control , unit ) Hashtbl.t;
	final_states : (control , unit ) Hashtbl.t;
	error_states : (control , unit ) Hashtbl.t;
	input_vars : nts_genrel_var list; (*Variable ordering is important*)
        output_vars : nts_genrel_var list;
        local_vars : nts_genrel_var list;
	transitions : (control, (control , nts_trans_label list ) Hashtbl.t) Hashtbl.t ;
      }

  type nts_system = 
      {
        nts_system_name : string;
        nts_global_vars : nts_genrel_var list;
        nts_automata : ( string , nts_automaton ) Hashtbl.t;
      }
	
  let anot_parser = (fun s -> Nts_Anot((Param.anot_parser s)))

  (*Need not appear in the API*)
  let get_cautomata_names_of_nts nts_sys =
    let key_name_folder vname _ retstring  =
      match retstring with 
	  "" -> vname
	| _ -> vname ^","^ retstring
    in
    (Hashtbl.fold key_name_folder  nts_sys.nts_automata "")




  let control_of_id_param p =
    Nts_State (p)

 

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

  

  let get_varinfo_by_optname nts_sys  (cname : string option) (vname : string) =
    let search_varname_iterator vname ntvar =
      match ntvar with
	| NtsGenVar(NtsIVar(name),_) | NtsGenVar(NtsRVar(name),_) ->
	  if (String.compare name vname )==0 then
	    raise (Found_genvar(ntvar))
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
			  (vname,cautomata_name_list) in
		      raise ex
		    end   
	    end
	| None -> None
    with 
	Found_genvar v -> Some(v)


  let get_varinfo_by_optcautomaton nts_sys  (cautomatopt : nts_automaton option) (vname : string) =
    let search_varname_iterator vname ntvar =
      match ntvar with
	| NtsGenVar(NtsIVar(name),_) | NtsGenVar(NtsRVar(name),_) ->
	  if (String.compare name vname )==0 then
	    raise (Found_genvar(ntvar))
	  else ()
    in
    try
      List.iter (search_varname_iterator vname) nts_sys.nts_global_vars;
      match cautomatopt with
	  Some(c)-> 
	    begin
	      try
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
			  (vname,cautomata_name_list) in
		      raise ex
		    end   
	    end
	| None -> None
    with 
	Found_genvar v -> Some(v)




  let pprint_inputvars cautomata = 
     Nts_generic.pprint_typeinfo_nts_genvar_list cautomata.input_vars
      
  let pprint_outputvars cautomata =
    Nts_generic.pprint_typeinfo_nts_genvar_list cautomata.output_vars

  let pprint_localvars cautomata =
    Nts_generic.pprint_typeinfo_nts_genvar_list cautomata.local_vars
    
  let pretty_label tlist =
    Nts_generic.nts_pprint_gen_trans_label_list tlist
   






  (* The function below need not appear in the interface file*)
  let pprint_initial_states c =
     let elem_left = ref 0 in
      let pprint_folder id () prescript =
	if !elem_left <= 1 then
	  prescript^(Format.sprintf "%s" ( pprint_control id ))
	else
	  begin
	    elem_left := !elem_left-1;
	    prescript^(Format.sprintf "%s," ( pprint_control id ))
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
	prescript^(Format.sprintf "%s" ( pprint_control id ))
      else
	begin
	  elem_left := !elem_left-1;
	  prescript^(Format.sprintf "%s," ( pprint_control id ))
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
	prescript^(Format.sprintf "%s" ( pprint_control id ))
      else
	begin
	  elem_left := !elem_left-1;
	  prescript^(Format.sprintf "%s," ( pprint_control id ))
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
    let dest_table_print_folder ( origin : control ) (dest : control ) label 
	(prescript : string ) =
      if (Nts_generic.static_check_if_gen_translist_unsat label) 
      then prescript 
      else
	begin
	  (* let label = Nts.rewrite_ndet_assignation label in *)
	  (*let label = Nts.havocise label in*)
	  let post_script = Format.sprintf "%s \n %s->%s { %s }" prescript ( pprint_control origin)  ( pprint_control dest) 
	    (pretty_label label)
	  in 
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
      let res_string=res_string^"\n"^Nts.concat_if_first_arg_nonzero pprint_loc ";\n" in
    
      
      let ret_vars = pprint_outputvars cautomata in
      let res_string =  (
	if String.length ret_vars > 0 
	then res_string^"out "^ret_vars^";\n"
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



  let pprint_all_cautomata cautomata_table =
    let pprint_automata_folder cname cautomaton prev_str =
      match prev_str with
	  "" ->  (pprint_to_nts cautomaton) 
	| _ ->
	  begin
	    let ret_str = prev_str ^"\n"^(pprint_to_nts cautomaton)
	    in ret_str
	  end
    in
    Hashtbl.fold pprint_automata_folder cautomata_table "" 


  let pprint_nts nt_system =
    let ret_string =  Format.sprintf "nts %s ; \n"
      nt_system.nts_system_name 
    in 
    let gvars_pprint =
      Nts_generic.pprint_typeinfo_nts_genvar_list nt_system.nts_global_vars 
    in
    let gvars_pprint = 
      (
      if String.length gvars_pprint > 0
      then gvars_pprint^";\n"
      else
	""
      )
    in
    let ret_string= ret_string^gvars_pprint
    in
    let all_automata = pprint_all_cautomata  nt_system.nts_automata
    in
    ret_string^all_automata^"\n"
    
   
    
end
