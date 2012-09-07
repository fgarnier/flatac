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
    val compare_keyid : t -> t -> int
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
   


  let pprint_states_list l =
    let lfolder pre elem =
      match pre with
	  "" -> pprint_control elem
	| _ -> pre^","^(pprint_control elem)
    in
    List.fold_left lfolder "" l  

  let list_of_hastbl_states t =
    let lfolder var () l =
      var::l
    in
    let state_list =  Hashtbl.fold lfolder t [] in
    let sorted_state_list =
      List.sort 
	( fun s t -> 
	  begin
	    match s,t with 
		Nts_State(s),Nts_State(t) -> Param.compare_keyid s t
	  end 
	) state_list
    in
    sorted_state_list
      
  let pprint_initial_states c =
    let c_list = list_of_hastbl_states c.init_states in
    let ret_candidate = pprint_states_list  c_list in
    match ret_candidate 
    with
	"" -> ""
      | _ -> "initial "^ret_candidate^";"
	
  let pprint_final_states c =
    let c_list = list_of_hastbl_states c.final_states in
    let ret_candidate = pprint_states_list c_list in
    match ret_candidate 
    with
	"" -> ""
      | _ -> "final "^ret_candidate^";"	

  let pprint_error_states c =
    let c_list = list_of_hastbl_states c.error_states in
    let ret_candidate = pprint_states_list c_list in
    match ret_candidate 
    with
	"" -> ""
      | _ -> "error "^ret_candidate^";"	

 

	  
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
      



 

  (** This function aims at printing all the transitions in a fixed 
      order, using the lexicographical order on the couples of orig and
destination states.*)

  let pprint_transitions_lexico_sorted (prescript :string) (cautomata : nts_automaton ) =
    let lex_control c d =
      match c, d with 
	  ((Nts_State(g),Nts_State(d)),(Nts_State(l),Nts_State(r))) 
	  ->
	    begin
	      let cmpare =  Param.compare_keyid g l in 
	      if cmpare = 0 
	      then 
		begin
		  Param.compare_keyid d r
		end 
	      else 
		cmpare
	    end
    in
    let dest_table_to_list_folder ( origin : control ) (dest : control ) 
	label dest_list =
      (origin,dest,label)::dest_list 
    in
    let dest_orig_list_to_list_folder (origin : control ) dest_table  
	dest_list =
      let inner_list = Hashtbl.fold  ( dest_table_to_list_folder origin) dest_table [] 
      in
      inner_list@dest_list
    in
    
    let pprint_list_folder prepprint (origin,dest,label)  = 
      let post_script = Format.sprintf "%s \n %s->%s { %s }" prepprint ( pprint_control origin)  ( pprint_control dest) 
	(pretty_label label)
      in 
      post_script
    in
    let flat_list = 
      (Hashtbl.fold dest_orig_list_to_list_folder cautomata.transitions []) 
    in
    let sorted_list  =  List.sort 
      ( fun (g,d,_) (l,r,_) -> 
	lex_control (g,d) (l,r) 
      ) 
      flat_list 
    in
    prescript^(List.fold_left pprint_list_folder "" sorted_list) 
      
      

    


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
     (* let res_string = res_string^((pprint_transitions "" cautomata))*)
      let res_string = (pprint_transitions_lexico_sorted res_string cautomata )
      in
      let res_string = res_string^"\n}" in
      res_string


(** This function prints all the automata of an nts w.r.t. the lexicographical
ordering on their name. *)

 let pprint_automata_lexico_sorted ( cautomata_table : 
					(string, nts_automaton ) Hashtbl.t ) =
    
    let pprint_folder prev_str (_,cautomaton) =
      match prev_str with
	   "" ->  (pprint_to_nts cautomaton) 
	| _ ->
	  begin
	    let ret_str = prev_str ^"\n"^(pprint_to_nts cautomaton)
	    in ret_str
	  end
    in
    let extract_list_folder a_name automat l  =
      (a_name,automat)::l in

    let ret_list =  Hashtbl.fold extract_list_folder cautomata_table []   in
    let ret_list = List.sort (fun (a,_) (b,_) -> String.compare a b) 
      ret_list in
    (List.fold_left pprint_folder "" ret_list)
      


 let compute_pred_relation cautomaton =
   let invert_table = Hashtbl.create 7 in
   let inner_relation_iterator curr_state succs _ =
     if not ( Hashtbl.mem invert_table succs )
     then
       begin
	 let succs_entry = Hashtbl.create 7 in
	 Hashtbl.add succs_entry curr_state ();
	 Hashtbl.add invert_table succs succs_entry 
       end
     else
       begin
	 let inner_table = Hashtbl.find invert_table succs in
	 if not (Hashtbl.mem inner_table curr_state) 
	 then Hashtbl.add inner_table curr_state ()
	 else ()
       end
   in
   let outer_relation_iterator curr_state succs_table =
     Hashtbl.iter (inner_relation_iterator curr_state ) succs_table
   in
   Hashtbl.iter outer_relation_iterator cautomaton.transitions;
   invert_table



   
     

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
    (*
    let all_automata = pprint_all_cautomata  nt_system.nts_automata
    *)
    let all_automata = pprint_automata_lexico_sorted nt_system.nts_automata
    in
    ret_string^all_automata^"\n"
    
   






    (** Types and functions used to generate a control flow graph
    from the numerical transition system description*)



  type nts_basic_block {
    mutable head_label : string ;
    mutable block : (control * nts_trans_label_list) list; 
    (** Current control state,
	nts_trans_label_list corresponds
	to what changes/is called before
	transiting*)
    
    mutable block_succs : ( nts_basic_block ref * nts_trans_label_list ) list option;
    (** transitions between blocks. Nexts blocks and the transisions being
	described.
	None is in the case the last control state is an error state.
	It's also a convenience for the buiding process.   
    *) 
  } 
    
    
  type nts_automaton_cfg {
    mutable nts_cfg_name : string; 
    mutable cfg_anot : anotations;
    (*states : (control , unit ) Hashtbl.t;*)
    nts_cfg_init_block : (string , unit ) Hashtbl.t;
    nts_cfg_final_block : (string , unit ) Hashtbl.t;
    nts_cfg_error_block : (string , unit ) Hashtbl.t;
    nts_input_vars : nts_genrel_var list; (*Variable ordering is important*)
    nts_output_vars : nts_genrel_var list;
    nts_local_vars : nts_genrel_var list;
    nts_blocks_transitions : ( string , nts_basic_blocks ) Hashtbl.t
  }




  let out_degree_of_control_state (control_state : control ) 
      (cautomaton : nts_automaton ) =
    let count_folder _ _ sharp_entry =
      sharp_entry + 1
    in
    try
      let control_table = Hashtbl.find cautomaton.transitions control in
      Hashtbl.fold count_folder control_table 0
    with
	Not_found -> 0
	  

  exception First_elem of 'a * 'b 
      
  let pick_elem_in_hastbl t =
    let get_fist_elem a b =
      raise First_elem (a , b);
      () 
    in
    try 
      Hashtbl.iter Fist_elem t
    with
	First_elem (a,b) -> (a,b)
	  


  exception Cant_be_head_of_basic_block of control

  (* Creates a basic block header from a control state, the latter must
  have one successor at most, else an exception is raised.*)
  let create_basic_block (control_state : control ) ( label : string ) 
    (cautomaton : nts_automaton ) =

    if (out_degree_of_control_state control cautomaton) <> 1 then
      raise Cant_be_head_of_basic_block ( control )
    else
      begin
	let entry_table = 
	  Hashtbl.find cautomaton.transitions control_state 
	in
	let block_head = pick_elem_in_hashtbl entry_table in
	{
	  head_label = label;
	  block  = block_head :: [] ;
	  block_succs = None;
	}
      end

  (* Create one block per initial states successor. The blocks are not
     filled entirely, moreover the initial states are not marked.
  *)
	
  let add_init_states_to_cblocks_queue nts_automaton q 
      (label_id : int Pervasives.ref ) =
    
    let init_state_rel_iterator control _ =
      let label =  Format.sprintf "lab%s" !label_id in
      label_id := !label_id + 1;
      let init_block = create_basic_block control label nts_automaton
      in
      Queue.push init_block a
    in
    Hashtbl.iter init_state_rel_iterator nts_automaton.init_states
    

  type visited_table = Visited of (control, unit ) Hashtbl.t
  type label_index = Label_index of (string , control ) Hashtbl.t
  type control_label_index = Control_label of (control , string ) Hashtbl.t
  type block_control_index = Control_block_index of ( control, basic_block ) Hashtbl.t
  type block_label_index = Label_block_index of (string , basic_block ) Hashtbl.t



  (** Returns the number of control states registered as a predecessor
  of the control state "state" given as a parameter. *)

  let in_degree_of_control_state state pre_relation =
    try 
      let matches = Hashtbl.find_all pre_relation state in
      List.length matches
    with
	Not_found -> 0




  (** 
      
      
  *)

  let label_contol_state control =
    
	  
  let build_basic_block_for_branching_statement 
      bblock (vtable : visited_table ) 
      (lindex : label_index) (cindex : control_lablel_index )
      (bindex : block_index) (pred_relation : (control , unit) Hashtbl.t )
      cautomaton 
      (label_id : int ref ) = 
    

    (* 
       Creates, label and chain a block per branch. Mark them as visited
       Updates the lindex and bindex with the newly created control 
       blocks.
    *)
    

    let sequentialize_branching control_state_org 
	control_state_dest nts_label block_list =

      let  
	  
      let successor_table = 
      Hashtbl.find control_state cautomaton.transitions in
      

  (** Takes as input a basic block header and completes the list
      of couples ( controls * nts_transition list) and 
      returns the filled basic block plus the 
      list of the basic blocks headers that have not
      been visited that succeeds the currently
      visited block
  *)
  let fill_basic_block bblock (vtable : visited_table ) 
      (lindex : label_index) (cindex : control_lablel_index )
      (bindec : block_index) (pred_relation : (control , unit) Hashtbl.t )
      cautomaton 
      (label_id : int ref )  = 
    
    let rec add_elem_of_segment current_control =
      if ( out_degree_of_control_state current_control cautomaton = 1 
	  && in_degree_of_control_state current_control pred_relation = 1 )
      then
	begin
	  let out_relation = Hashtbl.find 
	    cautomaton.transitions current_control in
	  let (ctr, trans_list ) =  pick_elem_in_hastbl out_relation in
	  bblock.block <- (bblock.block @ (ctr,trans_list)) ;
	  add_elem_of_segment ctr
	end
      else
	(* In this case, the current element given as parameter, is
	the first element of another block. It is either branching
	or have many predecessors*)
	let 
		
    in
    
    
    


  let cfg_of_nts_automaton ca =
    (*One shall memorize wich are the control strates
    that have aleady been traversed.*)
    let visited_states = Hashtbl.create 97 in    
    (* Labelleling statement with are at the begining of
       a basic block.*)
    let label_statement = Hashtbl.create 97 in
    (* Unique id counter for labelling basic blocks*)
    let counter_label = ref 0 in
    (* Basic blocks*)
    let blocks_index = Hashtbl.create 97 in
    (* Blocks being built *)
    let current_blocks = Queue.create () in
    add_init_states_to_cblocks_queue ca current_blocks;
    
    (* Initial states are marked for not being traversed again.*)
    
    let mark_init_as_visited_iterator control _ =
      Hashtbl.add visited_states control ()
    in
    Hashtbl.iter mark_init_as_visiter_iterator ca.initial_states ;
       
    
    

end
