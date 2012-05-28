open Nts_types
open Hashtbl
open Option



module type NTS_PARAM =
  sig
    type t         (*Type for key id of control states: e.g : int, string*)
    type anot_type (*Type for anotations*)
    val pprint_keyid : t -> string 
    val pprint_anot : anot_types -> string (*Types for pprinting anotations*)
  end



module Nts_gen =
  functor( Param : NTS_PARAM ) =
struct 
  type anotations = Nts_Anot of Param.anot_type
  type control = Nts_State of Param.t (* Control state *)
 
  let pprint_control c =
    match c with
	Nts_State(s) -> Param.pprint_keyid s

  let pprint_anotation a =
    match c with
	Nts_Anot(l)-> Param.pprint_anot l
      
  type nts_automaton =
      {
	mutable nts_automata_name : string 
	anot : anotations
	states : (control , unit ) Hastbl.t;
	init_states : (control , unit ) Hashtbl.t;
	final_states : (control , unit ) Hashtbl.t;
	error_states : (control , unit ) Hastbl.t;
	mutable intput_vars : nts_var list; (*Variable ordering is important*)
	mutable output_vars : nts_var list;
	mutable local_vars : nts_var list;
	transitions : (control, (control , cnt_trans_label list ) Hashtbl.t) Hashtbl.t ;
      }

  val size_hash = 97

  let create_nts () = 
    let anot_i = Param.make_anot () in
    let states_i = Hashtbl.create size_hash in
    let final_states_i = Hashtbl.create size_hash in
    let init_states_i = Hashtbl.create size_hash in
    let error_states_i = Hashtbl.create size_hash in
    let transition_i = Hashtbl.create size_hash in
      {
	nts_automata_name <- "";
	anot=anot_i;
	states=states_i;
	init_states=init_states_i;
	final_states=init_states_i;
	error_states = error_states_i;
	input_vars <- [];
	output_vars <- [];
	local_vars <- [];
      }
      
  val pprint_nts_automata : nts_automata -> string
  
  let add_inputvar_left  (c : nts_automata)  ( v : nts_var) =
    c.input_vars <- (v::c.input_vars) in
    c
      
  let add_outputvar_left c v =
    c.output_vars <- (v::c.output_vars) in
    c
     
  let add_init_state cautomata (s : control ) =
    if (not Hashtbl.mem cautomata.init_states s)
    then Hashtbl.add cautomata.init_states s
    else ()

  let add_error_state cautomata (s : control ) =
    if (not Hashtbl.mem cautomata.error_states s)
    then Hashtbl.add cautomata.error_states s
    else ()
      
  let add_final_state cautomata (s : control ) =
    if (not Hashtbl.mem cautomata.error_states s)
    then Hashtbl.add cautomata.error_states s
    else ()

  (*I dont check the unicity of s1->l->s2 transition.*) 
  let add_transition (cautomata : nts_automata) 
    ( orig : control ) (dest : control) ( lab : cnt_trans_label list) =
    let master_rel = cautomata.transitions in
    if Hastbl.mem master_rel orig then
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

  (*Returns the collection of transitions betwenn sorg and sdests
    The result has type cnt_translabel list list
  *)
  let get_transition_from  cautomata sorg sdest =
    if not (Hashtb.mem cautomata.transitions sorg)
    then None
    else 
      begin
	let origin_table = Hashtbl.find cautomata.transitions sorg in
	  try
	    let tansitions = Hashtbl.find_all origin_table sdest in
	      Some(transitions)
	  with
	      Not_found -> None
      end
  (*None is returned if no transition exists*)
  
  (* The function below need not appear in the interface file*)
  let pprint_initial_states c =
     let elem_left = ref 0 in
      let pprint_folder id () prescript =
	if !elem_left <= 1 then
	  prescript^(Format.sprintf "s%d" ( pprint_control id ))
	else
	  begin
	    elem_left := !elem_left-1;
	    prescript^(Format.sprintf "s%d," ( pprint_control id ))
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
	prescript^(Format.sprintf "s%d" ( pprint_control id ))
      else
	begin
	  elem_left := !elem_left-1;
	  prescript^(Format.sprintf "s%d," ( pprint_control id ))
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
	prescript^(Format.sprintf "s%d" ( pprint_control id ))
      else
	begin
	  elem_left := !elem_left-1;
	  prescript^(Format.sprintf "s%d," ( pprint_control id ))
	end
    in
      elem_left := (Hashtbl.length c.error_states);
      if !elem_left >0 then
	let retstring = Hashtbl.fold pprint_folder c.error_states ""
	in
	  "error "^retstring^";"
      else
	""

  let pprint_to_nts cautomata = 
      (* let current_ecfg_node = Hashtbl.get vertex current_vertex_id in *)
      let res_string = cautomata.name^"{\n" in
      let res_string = (
	if List.length cautomata.input_vars > 0 then
	res_string^"in "^(pprint_input_vars ())^";\n"
	else res_string
      )
      in
      let pprint_loc = self#pprint_local_vars () in
      let pprint_loc_pre = front_end#pprint_list_of_malloc_vars () in
      let pprint_loc=Nts.concat_comma_both_arg_non_empty pprint_loc_pre 
	pprint_loc in
      let res_string=res_string^"\n"^Nts.concat_if_first_arg_nonzero pprint_loc " : int ;\n" in
    
      
      let ret_vars = self#pprint_out_vars () in
      Format.printf "Outvars are : %s \n" ret_vars;
      let res_string =  (
	if String.length ret_vars > 0 
	then res_string^"\n"^ret_vars^"\n"
	else
	  res_string
      ) 
      in
      let res_string = res_string^((self#pprint_inits ()))^"\n"  in
      let res_string = res_string^((self#pprint_finals ()))^"\n" in
      let res_string = res_string^((self#pprint_error_states())) in
      let res_string = res_string^((self#pprint_transitions()))
      in
      let res_string = res_string^"\n}" in
      res_string




  let pprint_inputvars cautomata = 
     Nts.pprint_typeinfo_nts_var_list c.input_vars
       
  let pprint_outputvars cautomata =
    Nts.pprint_typeinfo_nts_var_list c.input_vars

  let pprint_cautomata cautomata =
    
    
end
