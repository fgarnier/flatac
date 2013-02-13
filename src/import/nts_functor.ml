(*

This files contains the implementation of the Numerical Transition Library 
--see http://richmodels.epfl.ch/ntscomp-- main objects, namely :

_ Numerical transitions subsystems, (i.e. parametric counter automaton
  with return values upon return)
_ Hyerarchical transistions subsystems .


Plus a parser, a pretty printer as well as cleanup functions.
A type checker will be added.



Written by Florent Garnier, at Verimag Labs  2012 
Contact florent dot garnier at gmail dot com for  further informations.

This files is released under the terms of the LGPL v2.1 Licence.

 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor,
 Boston, MA  02110-1301  USA

*)

open Nts_types
open Hashtbl
open Nts_generic
open Simplification (* Contains what needed to remove
		    useless variable declarations*)


exception Var_name_already_used
exception Found_genvar of nts_genrel_var
exception No_such_counter_automata_in_nts_system of string * string 
exception UnboundVarName of string 
exception Nts_i_have_a_binding 




 type ('a , 'b) gen_bin_relation =  [`Generic_relation of 'a * 'b]
 

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
    val key_val_of_string : string -> t
    val compare_keyid : t -> t -> int
    val pprint_anot : anot_type -> string (*Types for pprinting anotations*)
  end


module Make =
  functor( Param : NTS_PARAM )->
struct 
  type anotations = Nts_Anot of Param.anot_type
  type control = Nts_State of Param.t (* Control state *)

  type states_container = (control , unit ) Hashtbl.t
  type transitions_container = (control, (control , nts_trans_label list ) Hashtbl.t) Hashtbl.t
  type inv_relation_container = (control, (control , unit) Hashtbl.t ) Hashtbl.t
      
  let size_hash = 97
  let pprint_control c =
    match c with
	Nts_State(s) -> Param.pprint_keyid s

  (* let pprint = Param.pprint_keyid *)
  let pprint_anotation a =
    match a with
	Nts_Anot(l)-> Param.pprint_anot l
      
  type nts_automaton =
      {
	mutable nts_automata_name : string; 
	mutable anot : anotations;
	(*states : (control , unit ) Hashtbl.t;*)
	init_states : states_container ;
	final_states : states_container;
	error_states : states_container;
	input_vars : nts_genrel_var list; (*Variable ordering is important*)
        output_vars : nts_genrel_var list;
        local_vars : nts_genrel_var list;
	transitions : transitions_container ;
      }

  type nts_system = 
      {
        nts_system_name : string;
        nts_global_vars : nts_genrel_var list;
        nts_automata : ( string , nts_automaton ) Hashtbl.t;
	nts_gvars_init : nts_gen_relation list option; (* 
							  Initial conditions
							  that global variables
							  satisfy. 
						       *)
	nts_system_threads : ( string * Big_int.big_int ) list  option;   (*
							Optional
							threads declaration,
							name of the nts subsystem
							associated to the number
							of occurences.
						      *)

      }

  type num_subrel_in_cautomaton = {
    subrel_root : control ;
    sub_vertices : states_container;
    sub_transitions : transitions_container;
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
      
  let prepostfix_id_of_control cstate prefix suffix =
    let new_key_id = Format.sprintf "%s%s%s" prefix (pprint_control cstate) 
      suffix in
    let new_id = Param.key_val_of_string new_key_id 
    in
    control_of_id_param new_id
    

  let is_state_in_inv_relation table cstate =
    Hashtbl.mem table cstate


  let list_of_opt_list opt_l =
    match opt_l with 
      None -> []
    | Some(l) -> l



 exception Found_control_state_exception
    
  let is_state_in_transition_container cstate cont =
    let inner_rel_iterator _ inner_table =
      if Hashtbl.mem inner_table cstate then
	raise Found_control_state_exception
      else ()
    in
    try
      if not (Hashtbl.mem cont cstate) 
      then
	begin
	  Hashtbl.iter inner_rel_iterator cont;
	  false
	end
      else true
    with
      Found_control_state_exception -> true
	
  let is_state_in_cautomaton state caut =
    if Hashtbl.mem  caut.init_states state then true
    else if Hashtbl.mem  caut.final_states state then true
    else if Hashtbl.mem  caut.error_states state then true 
    else is_state_in_transition_container state caut.transitions

  let is_state_pair_in_inv_relation table corg cdest =
    if Hashtbl.mem table corg then
      (let cin = Hashtbl.find table corg in Hashtbl.mem cin cdest)
    else
      false
    


  let is_successor_of cautomaton sorg sdest =
    try
      let cin = Hashtbl.find cautomaton.transitions sorg 
      in 
      Hashtbl.mem cin sdest	  
    with
      Not_found -> false

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
	NtsGenVar(NtsVar(name,_),_)  ->
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
	 NtsGenVar(NtsVar(name,_),_)  ->
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


	  
  (** Binding between the generic definition of fold_states_containers  
      provided in the interface and the specific implementation proposed
      in this file.
  *)

  let fold_states_containers statec folder_fun init_val =
    let bind_folder control () prefix =
      folder_fun prefix control
    in
    Hashtbl.fold bind_folder statec init_val
    

  (** In this implementation, transc has type 
      (control,(control, nts_gen_rel list) t ) t. 
  *)
  let fold_transitions_container transc folder_fun init_val =
    let inner_folder external_control curr_control transit prefix =
      let ret_val = folder_fun prefix external_control  transit curr_control 
      in
      ret_val
    in
    let outter_folder external_control inner_table prefix =
      Hashtbl.fold ( inner_folder external_control) inner_table prefix
    in
    Hashtbl.fold  outter_folder transc init_val

 
  let iter_transitions_container transc iter_fun =
    let inner_iterator  external_control curr_control transit =
      iter_fun external_control  transit curr_control
    in
     let outter_iterator external_control inner_table =
       Hashtbl.iter ( inner_iterator external_control) inner_table
     in
     Hashtbl.iter  outter_iterator transc


  let iter_state_container statec iterfun =
    let binding_iter_fun s  () = iterfun s in 
    Hashtbl.iter binding_iter_fun statec

  let add_transition_to_container  transc corg trans_label cdest = 
    if not ( Hashtbl.mem transc corg ) 
    then
      begin
	let inner = Hashtbl.create 7 
	in
	Hashtbl.add inner cdest trans_label;
	Hashtbl.add transc corg inner 
      end
    else
      begin
	let inner= Hashtbl.find transc corg in
	if not (Hashtbl.mem inner cdest )
	then
	  begin
	    Hashtbl.add inner cdest trans_label
	  end
	else 
	  begin
	    let lab = Hashtbl.find inner cdest in
	    if trans_label = lab then ()
	    else Hashtbl.add inner cdest trans_label
	  end
      end
    

(** Both target and source are transition containers. target will
be added all element of source. *)
  let union_of_transition_container target source =
    let adder_iterator corg label cdest =
      add_transition_to_container target corg label cdest 
    in
    iter_transitions_container source adder_iterator 


  let is_error_state ca control =
    Hashtbl.mem  ca.error_states control
  
  let is_final_state ca control =
    Hashtbl.mem  ca.final_states control 
      
  let is_initial_state ca control =
    Hashtbl.mem ca.init_states control    

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

 

  let pprint_transitions_in_transcontainer transc =
    let transfolder pre org_state label dest_state =
      let post_script = 
	Format.sprintf "%s \n %s->%s { %s }" pre ( pprint_control org_state)  ( pprint_control dest_state) 
	    (pretty_label label) 
      in
      post_script
    in
    fold_transitions_container transc transfolder "" 

  let pprint_subgraph_transitions subgraph =
    pprint_transitions_in_transcontainer subgraph.sub_transitions


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
      



  (** Returns the number of successor of a control state in a 
given automaton.*)

  let out_degree_of_control_state (control_state : control ) 
      (cautomaton : nts_automaton ) =
    let count_folder a b sharp_entry =
      sharp_entry + 1
    in
    try
      let control_table = 
	Hashtbl.find cautomaton.transitions control_state 
      in
      (Hashtbl.fold count_folder control_table 0)
    with
	Not_found -> 0
  

  let in_degree_of_control_state (control_state : control)
      (inv_rel : inv_relation_container ) =
    let count_folder a b sharp_entry =
      sharp_entry + 1
    in
    try
      let control_table = 
	Hashtbl.find inv_rel control_state 
      in
      (Hashtbl.fold count_folder control_table 0)
    with
	Not_found -> 0
  
 

  let is_cstate_merge_point (control_state : control ) 
      inv_rel =
    let deg_in_cstate = in_degree_of_control_state control_state inv_rel 
    in
    ( deg_in_cstate > 1 )   


  let is_cstate_branching (control_state :control ) 
      cautomaton = 
    let deg_out_cstate = out_degree_of_control_state control_state cautomaton
    in
    ( deg_out_cstate > 1)


  let is_cstate_in_linear_chain (control_state : control ) cautomaton
      inv_rel =
    let deg_out =  out_degree_of_control_state control_state cautomaton in
    let deg_in =  in_degree_of_control_state control_state inv_rel 
    in
    (deg_in <= 1)&&(deg_out= 1)


  let has_successor control_state cautomaton =
    let deg_out_cstate = out_degree_of_control_state control_state cautomaton
    in
    deg_out_cstate >= 1
    
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
      
      
  
  let subst_dot_by_underscore str =
    let res = ref "" in
    let index = ref 0 in
    let len = String.length str in
    while (!index < len ) 
    do
      if (str.[!index]='.' || str.[!index]='/' ||str.[!index]='-' ) then
	res := !res^"_"
      else
	res := !res ^(String.make 1 (str.[!index]));
      index:=!index + 1
    done;
    !res
      

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
      

 let get_outing_transitions_of cautomaton state =
   let returned_table = Hashtbl.create 7 in
   let succss_table = Hashtbl.find cautomaton.transitions state in
   let build_iterator state l =
     Hashtbl.add returned_table state l 
   in
   Hashtbl.iter  build_iterator succss_table ;
   returned_table

 let get_successor_of cautomaton state =
   let returned_table = Hashtbl.create 7 in
   let succss_table = Hashtbl.find cautomaton.transitions state in
   let build_iterator state _ =
     Hashtbl.add returned_table state () 
   in
   Hashtbl.iter  build_iterator succss_table ;
   returned_table



(**
Returns a binding of the hashtbl tbl.
*)     

 let get_one_binding tbl =
   let gen_binding = ref None in
    let get_first_elem_iterator a b =
      gen_binding := Some( ( a , b ) );
      raise Nts_i_have_a_binding
    in
    try 
      Hashtbl.iter get_first_elem_iterator tbl;
      !gen_binding
    with
	Nts_i_have_a_binding -> !gen_binding
   

(**
 Same as above, but raises a Not_found_exception if tbl is empty.
*)

 let one_binding tbl =
   let gen_binding = ref None in
    let get_first_elem_iterator a b =
      gen_binding := Some( ( a , b ) );
      raise Nts_i_have_a_binding
    in
    try 
      Hashtbl.iter get_first_elem_iterator tbl;
      raise Not_found
    with
	Nts_i_have_a_binding ->
	  begin
	    match !gen_binding with
	      Some(s)->s
	    | None -> assert false
	  end

 let get_one_state tbl =
   let one_binding = get_one_binding tbl in
   match one_binding with 
       Some((a,b)) -> Some(a)
     | None -> None
   
 
(** Picks-up one transition among that which are exiting the 
control state "state" in the subsystem cautomaton.
*)

 let get_one_transition cautomaton state =
   let tbl = get_outing_transitions_of cautomaton state in
   let one_binding = get_one_binding tbl 
   in
   match one_binding with 
       Some((a,b)) -> (a,b)
     | None -> assert false 


 let pred_relation_of_relation rel =
   let invert_table = Hashtbl.create 97 in
   let inner_relation_iterator curr_state succs _ =
     if not ( Hashtbl.mem invert_table succs )
     then
       begin
	 let succs_entry = Hashtbl.create 7 in
	 Hashtbl.add succs_entry  curr_state ();
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
   Hashtbl.iter outer_relation_iterator rel;
   invert_table



 let compute_pred_relation cautomaton =
   pred_relation_of_relation cautomaton.transitions

 let get_cautomaton_by_name ntsys name =
   try
     Hashtbl.find ntsys.nts_automata name 
   with Not_found ->
     raise 
       (No_such_counter_automata_in_nts_system(ntsys.nts_system_name,name)) 


 (* In this block, one define functions that aims at computing the
    Subgraph having c as root element/root. Bascially, we compute
    the set of all edges and vertices travesable/reachable from c in
    the given cautomaton.*)


 let copy_out_transition_from_rel  t_def =
   let copied_def = Hashtbl.create 7 in
   let copy_iterator key tlabel =
     Hashtbl.add copied_def key tlabel
   in
   Hashtbl.iter copy_iterator t_def;
   copied_def

  
 let transition_out_of_control_state cstate transition =
   let new_transition_container = Hashtbl.create 97 in
   let copy_iterator corg label cdest =
     if corg == cstate 
     then
	 add_transition_to_container new_transition_container corg label cdest
     else ()
   in
   iter_transitions_container transition copy_iterator;
   new_transition_container
 



(** 
Selects the set of transitions of transitions which applied upon
edge_accept_fun evaluates to true.
*)

 let select_accepted_edges edge_accept_fun transitions =
   let new_rel = Hashtbl.create 97 in
   let select_iterator corg label cdest = 
     if edge_accept_fun corg label cdest 
     then
       add_transition_to_container new_rel corg label cdest 
     else
       ()
   in
   iter_transitions_container transitions select_iterator;
   new_rel

 let select_edges_from_inner_table edge_accept corg inner_table =
   let new_rel = Hashtbl.create 97 in
   let select_iterator  cdest label = 
     if edge_accept corg label cdest 
     then
       Hashtbl.add new_rel cdest label 
     else
       ()
   in
   Hashtbl.iter select_iterator inner_table;
   new_rel


 let rec build_subg_iterator ?(edge_accept_fun = fun corg label cdest -> true ) relation_table visited_vertices 
    traversed_edges v_current _ =
   
   if (Hashtbl.mem visited_vertices v_current) 
   then ()
   else 
     begin
       Hashtbl.add visited_vertices v_current () (*Marks v_current as visited*);
       if Hashtbl.mem relation_table v_current 
       (*Check whether
	 there exists an outgoing transition from this control
	 state.*)
          
       then
	 begin
	   (*let outing_edges = transition_out_of_control_state v_current 
	     relation_table 
	   *)
	   let outing_edges_table = Hashtbl.find relation_table 
					     v_current
	   in
	   let outing_edges_table = Hashtbl.copy outing_edges_table 
	   
	   (*
	     copy_out_transition_from_rel (Hashtbl.find relation_table 
					     v_current ) *)
	   in
	   let outing_edges =
	     select_edges_from_inner_table edge_accept_fun v_current outing_edges_table 
	   in
	   (*union_of_transition_container traversed_edges outing_edges;*)
	   Hashtbl.add traversed_edges v_current outing_edges;
	   let recurse_iterator = (build_subg_iterator 
	     ~edge_accept_fun:edge_accept_fun 
	     relation_table visited_vertices traversed_edges) in
	   Hashtbl.iter recurse_iterator outing_edges 
	     
	 end
       else ();(* No outgoing transition ? No recursion.*)
     end
       
       
  (** Takes as input a relation and returns the collection
of transition that are in the transitive closure of any relation starting 
from a given control state c *)


 let sub_rel_rooted_in relation c =
   let visited_vertices = Hashtbl.create 97 in
   let traversed_edges = Hashtbl.create 97 in   
    ( build_subg_iterator relation visited_vertices
    traversed_edges) c ();
   traversed_edges
 

 let subgraph_rooted_in_c  cautomaton c =
   let visited_vertices = Hashtbl.create 97 in
   let traversed_edges = Hashtbl.create 97 in
   
   (build_subg_iterator cautomaton.transitions visited_vertices 
      traversed_edges) c []; 
   
   {
     subrel_root = c ;
     sub_vertices = visited_vertices ;
     sub_transitions = traversed_edges ;
   }
     

let subgraph_rooted_in_c_edge_condition edge_accept cautomaton c =
   let visited_vertices = Hashtbl.create 97 in
   let traversed_edges = Hashtbl.create 97 in
   
   (build_subg_iterator ~edge_accept_fun:edge_accept cautomaton.transitions visited_vertices 
      traversed_edges) c []; 
   
   {
     subrel_root = c ;
     sub_vertices = visited_vertices ;
     sub_transitions = traversed_edges ;
   }
     
 


(** This function returns a transition relation, which corresponds
to *)
 
let prune_rel (rel : transitions_container )  
     ( con_relation : inv_relation_container ) =
  
  let prunned_relation = Hashtbl.create 97 in
  let iter_adder  lhs_control transit rhs_control  =
    if is_state_pair_in_inv_relation con_relation rhs_control lhs_control 
    then
      let _ = add_transition_to_container prunned_relation 
	lhs_control transit rhs_control 
      in ()
    else
      ()
  in
  iter_transitions_container rel iter_adder;
  prunned_relation
    
    
(** Computes and return the collection of all vertices that
appear at one side of a transition.*)

let vertices_in_rel rel =
  let vertex_collection = Hashtbl.create 97 in
  let collect_name_iterator corg _ cdest =
    if not ( Hashtbl.mem vertex_collection corg ) 
    then Hashtbl.add vertex_collection corg ()
    else ();
    if not ( Hashtbl.mem vertex_collection cdest )
    then Hashtbl.add vertex_collection cdest ()
    else ()
  in
  iter_transitions_container rel collect_name_iterator;
  vertex_collection
    
 (** This function computes the subrelation of the one described in 
 cautomaton, that is composed of all the arrows that are between 
 max and min*)  


 let subgraph_between cautomaton max min =
  
   let subgraph_max = subgraph_rooted_in_c cautomaton max in
   let inv_rel_min = pred_relation_of_relation subgraph_max.sub_transitions in
   let reach_inv_rel_min = sub_rel_rooted_in  inv_rel_min min in
   let sub_graph_pruned_relation =  prune_rel subgraph_max.sub_transitions 
  reach_inv_rel_min in
   
   {
     subrel_root = subgraph_max.subrel_root ;
     sub_vertices = vertices_in_rel sub_graph_pruned_relation ;
     sub_transitions = sub_graph_pruned_relation ;
   }
     
   

 let subgraph_between_cond_on_edges edge_accept cautomaton max min =
   let subgraph_max = subgraph_rooted_in_c_edge_condition 
     edge_accept cautomaton max in
   let inv_rel_min = pred_relation_of_relation subgraph_max.sub_transitions in
   let reach_inv_rel_min = sub_rel_rooted_in  inv_rel_min min in
   let sub_graph_pruned_relation =  prune_rel subgraph_max.sub_transitions 
  reach_inv_rel_min in
   
   {
     subrel_root = subgraph_max.subrel_root ;
     sub_vertices = vertices_in_rel sub_graph_pruned_relation ;
     sub_transitions = sub_graph_pruned_relation ;
   }


(** Checks that a control state does not belong to a path, encoded as
a list of transitions. You must make sure that two following transitions
ave value such that (a,b,_)(b,c,_)(c,d,_) etc ... *)

 let not_looping inv_prefix_path curr_state next_state =
   let rec not_looping_history h next_state =
     match h with
       [] -> true
     | (pre,last,_) :: [] ->  ( next_state <> pre ) && (next_state <> last)
     | (pre,_,_)::h' -> 
       if ( next_state <> pre ) 
       then (* Not looping here, see one step back in history*)
	 begin
	   not_looping_history h' next_state
	 end
       else
	 false (* Equality means it's looping*)
	 
   in
   if next_state = curr_state then false (*It's looping*)
   else 
     not_looping_history inv_prefix_path next_state
     
   
(** Computes the subgraph of subgraph_param that is formed of all the
vertices, resp. edges that are reachable, resp. travesable, form
the root of subgraph_param through a chain of edges that are all 
accepted by the function accept_fun. 

Parameter min represents the accepting control state, i.e. the set
of all transitions is the set of all transition that apprear in
a path starting from the root of subgraph_parm and whose last transition
rhs is min, and for each transition t of that path accept_fun(t) evaluates
to true.

accept_fun maps the transition to the booleans.
*)


 

 let states_container_of_states_list ( l : control list) =
   let ret_hash = Hashtbl.create 97 in
  List.iter ( fun s -> Hashtbl.add ret_hash s () ) l;
  ret_hash   


(* Converts the (control * control * tlist) list 
   into
   (control, (control, tlist) Hashtbl.t ) Hastbl.t
*)

let transitions_container_of_trans_list ( tlist :  (control * control * Nts_types.nts_trans_label list) list ) =
  let ret_hash = Hashtbl.create 97 in
  let build_iterator (c1,c2, tlabel ) =
    if not (Hashtbl.mem ret_hash c1) then
      begin
	let new_rel_hash = Hashtbl.create 97 in
	Hashtbl.add new_rel_hash c2 tlabel;
	Hashtbl.add ret_hash c1 new_rel_hash
      end
    else
      begin
	let inner_relation = Hashtbl.find ret_hash c1 in
	Hashtbl.add inner_relation c2 tlabel
      end
  in
  List.iter build_iterator tlist; ret_hash


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



 let pprint_nts_init_condition_folder pre ntrel =
    match pre with
        "" -> (Nts_generic.nts_pprint_genrel ntrel)
      | _ -> pre^" and "^(Nts_generic.nts_pprint_genrel ntrel)


  let pprint_optional_init nt_sys prefix =
    match nt_sys.nts_gvars_init with

      | Some([]) -> prefix
      | Some(l) ->
          begin
            let cnd = List.fold_left  pprint_nts_init_condition_folder "" l in
              Format.sprintf "%sinit %s;\n" prefix cnd
          end
      | None -> prefix



  let pprint_optional_thread_list nt_sys prefix =
    let thread_pprint_iterator ret_string (thread_name, nb_instances) =
      match ret_string with
	  "" -> Format.sprintf "instances %s [%s]" 
	    thread_name (Big_int.string_of_big_int nb_instances)
	| _ -> Format.sprintf "%s,%s [%s]" 
	  ret_string thread_name (Big_int.string_of_big_int nb_instances) 
    in
    match nt_sys.nts_system_threads  with
	Some(thread_list ) ->
	  let str = List.fold_left thread_pprint_iterator "" thread_list in
	  Format.sprintf "%s%s;\n" prefix str 
      | None -> prefix
	


  let pprint_nts nt_system =
    let ret_string =  Format.sprintf "nts %s ; \n"
      (subst_dot_by_underscore nt_system.nts_system_name) 
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
    let ret_string= pprint_optional_init nt_system ret_string
    in
    let ret_string = pprint_optional_thread_list nt_system ret_string
    in
    let all_automata = pprint_automata_lexico_sorted nt_system.nts_automata
    in
    ret_string^all_automata^"\n"
    
   

  (**

     One propose a set of functions that get rid of declared variables
     that don't appear in the transtions
     
  *)

  let locally_used_variables diary nt_aut =
    let log_var_in_transitions_iterator _ nts_translabel =
      Simplification.add_vars_of_trans_label_list_to_diary diary nts_translabel
    in
    let trans_table_iterator _ inner_table =
      Hashtbl.iter log_var_in_transitions_iterator inner_table
    in
    Hashtbl.iter trans_table_iterator nt_aut.transitions
   (* ;
    Format.printf "[locally_used_variables] \n"; (Simplification.pprint_diary diary)*)

  
  let update_local_list nt_aut loc_vars_list =
    {
      nts_automata_name=nt_aut.nts_automata_name;
      anot=nt_aut.anot;
      init_states=nt_aut.init_states;
      final_states=nt_aut.final_states;
      error_states=nt_aut.error_states;
      input_vars = nt_aut.input_vars; 
      output_vars = nt_aut.output_vars;
      local_vars = loc_vars_list; 
      transitions = nt_aut.transitions;
    }



  let nts_sys_with_update_cautomaton_table nt_sys ctable =
    {
      nts_system_name = nt_sys.nts_system_name ;
      nts_global_vars = nt_sys.nts_global_vars ;
      nts_automata = ctable ; 
      nts_gvars_init = nt_sys.nts_gvars_init ; 
      nts_system_threads = nt_sys.nts_system_threads;  
    }


  (**
     This functions removes the local variables of an automaton
     if if they are not listed in the diary.
  *)


  let clean_unlisted_local_vars  nt_aut =
   
    let local_list_folder diary glist gvar =
      if (contains_nts_genrel_var diary gvar) 
      then 
	gvar::glist
      else
	begin
	  (*Format.printf "eliminating variable %s \n" (Nts_generic.nts_pprint_genvar  gvar);*)
	  glist
	end
	
    in

    let diary = create_empty_var_diary () in 
    locally_used_variables diary nt_aut;
    
    let clean_local_list =
      (List.fold_left ( local_list_folder diary) []  nt_aut.local_vars )
    in
    update_local_list nt_aut clean_local_list

      
      
  let clean_unlisted_vars_on_all_system_table nt_system =    
    let cleaner_folder cname nt_aut n_table =
      let local_diary = create_empty_var_diary () 
      in
      locally_used_variables local_diary nt_aut; 
      (* Fills diarry with used
	 variables*)
      let clean_entry = clean_unlisted_local_vars  nt_aut
      in
      Hashtbl.add n_table cname clean_entry; n_table
    (* Modify each automaton
       within the hashtbl.*)
    in
    let new_table = Hashtbl.create 97 in
    Hashtbl.fold cleaner_folder nt_system.nts_automata new_table
    
      

  (** Adds the name of the called subsystems in cautomaton, in called_fun,
  if the former are not already referenced.*)

  let register_called_subsystem called_fun cautomaton =
   
    let called_fun_of_transition _ translist _ =
      (List.iter (fun s -> Simplification.register_called_subsystems called_fun s) translist)
    in
    iter_transitions_container  cautomaton.transitions called_fun_of_transition  
    

  let reference_called_nts nt_system =
    let called_fun = Simplification.create_fun_name_in_call_table () 
    in
    Simplification.add_fun_name_in_call_table called_fun "main";
    let register_callees_of_each_subsystems _ subs =
      register_called_subsystem called_fun subs 
    in
    Hashtbl.iter register_callees_of_each_subsystems nt_system.nts_automata;
    called_fun
    
    
(**
Returns as input a numerical transition system in which all local variables
list of each automaton has been cleared of non used varibles
*)
  let nt_system_var_cleaner nt_sys =
    let clean_system_table = clean_unlisted_vars_on_all_system_table nt_sys
    in
    nts_sys_with_update_cautomaton_table nt_sys clean_system_table
    



  let c_table_having_keys_in ctable called_fun =
    let new_table = Hashtbl.create 97 in
    let fill_clean_table name cautomaton =
      if Simplification.is_name_in_call_table called_fun name 
      then Hashtbl.add new_table  name cautomaton
      else ()
    in
    Hashtbl.iter fill_clean_table ctable;
    new_table




(** Returns the copy of a transition container.*)

let copy_transitions_container trans =
  let copy = Hashtbl.create 97 in
  let copy_iterator key inner_rel =
    let inncpy = Hashtbl.copy inner_rel in
    Hashtbl.add copy key inncpy
  in
  Hashtbl.iter copy_iterator trans;
  copy


(**

*)

let cautomaton_of_subrelation_cautomaton new_ca_name base_automaton sub_rel =
{
  nts_automata_name = new_ca_name;
  anot = base_automaton.anot;
  init_states  = Hashtbl.copy base_automaton.init_states;
  final_states = Hashtbl.copy base_automaton.final_states;
  error_states = Hashtbl.copy base_automaton.error_states;

  input_vars = base_automaton.input_vars;
  output_vars = base_automaton.output_vars;
  local_vars = base_automaton.local_vars;

  transitions = copy_transitions_container sub_rel.sub_transitions;

}    


(** *)
let cautomaton_of_transitions_container new_ca_name base_automaton sub_trans =
{
  
  nts_automata_name = new_ca_name;
  anot = base_automaton.anot;
  init_states  = Hashtbl.copy base_automaton.init_states;
  final_states = Hashtbl.copy base_automaton.final_states;
  error_states = Hashtbl.copy base_automaton.error_states;

  input_vars = base_automaton.input_vars;
  output_vars = base_automaton.output_vars;
  local_vars = base_automaton.local_vars;

  transitions =  copy_transitions_container sub_trans;

}
    



(**
 This function returns a numerical transition system where all the subsystem
are called at some point. Calls might be performed from the same subsystem.
*)

let nt_system_uncalled_subsystem_cleaner nt_sys =
  let called_fun = reference_called_nts nt_sys in
  let cleaned_call_table =  
    c_table_having_keys_in nt_sys.nts_automata called_fun 
  in
  nts_sys_with_update_cautomaton_table nt_sys cleaned_call_table
    

   

(**

Typing and typechecking section.

*)

  exception UndefinedVariable of string * nts_automaton
  exception UndefinedSubsystem of string

  exception ArgumentTypesMissMatch of nts_gen_relation
  (** Wrong number of arguments or arguments types don't
  match the subsystem call signature.*)

  exception TypeErrorInVar of nts_genrel_var
  exception TypeErrorInArithmExpr of nts_genrel_arithm_exp
  exception TypeErrorInNtsGenRelation of nts_gen_relation


  

  (** Try to find a varible definition in the sybsystem c of
      the nts n*)
  exception MultipleDeclarationOfVar of nts_genrel_var * nts_automaton
      
  let get_definition_of_variable_by_name n c vname =
    
    let is_some t =
      match t with 
	  None -> true
	| Some(_) -> false
    in
    let come_get_some t =
      match t with
	  Some(t) -> t
	| None -> assert false
    in
    let check_unicity elem itemfound =
      match elem with 
	  None -> itemfound
	| Some(v) ->
	  begin
	    if is_some itemfound then
	      raise (MultipleDeclarationOfVar(v,c))
	    else 
	      elem
	  end
    in
    let rec find_var_by_name name vlist =
      match vlist with
	  (NtsGenVar(NtsVar(vname,_),_) as hl)::l ->
	    begin
	      if String.compare vname name = 0 then
		Some(hl)
	      else
		find_var_by_name name l
	    end
	| [] -> None
    in
    let res1 = check_unicity (find_var_by_name vname c.input_vars) 
      None
    in
    let res1 = check_unicity (find_var_by_name vname c.output_vars) 
      res1 in
    let res1 = check_unicity (find_var_by_name vname c.local_vars)
      res1 in
    if is_some res1 then
      come_get_some res1 
    else
      begin
	let res1 = find_var_by_name vname n.nts_global_vars in
	if  is_some res1 then
	  come_get_some res1 
	else
	  raise ( UndefinedVariable ( vname , c))
      end

	
	
  (** Need to be moved in module Nts*)
  let nts_var_of_nts_genvar gv =
    match gv with
	NtsGenVar(v,_)-> v
	    
  (** Need to be moved in Nts module*)
  let has_type v t =
    match v with
	NtsGenVar(NtsVar(vname,typedef),priminfo) ->
	  if typedef = t then
	    true
	  else
	    false



    
	      
    (** This function might raise an exception whenever v is not
	a defined variable within the current context of c. *)
  let type_ntsgen_var n c v =
    match v with
	NtsGenVar(NtsVar(vname,NtsUnTyped),priminfo) ->
	  begin
	      (** No type is yet associated to this variable, hence
		  we need to look forward to its definition in 
		  the automaton.*)
	    let typedvar_from_def = 
	      get_definition_of_variable_by_name n c vname
	    in
	    let v= nts_var_of_nts_genvar typedvar_from_def in
	    NtsGenVar(v,priminfo)
	  end
	    
      |  NtsGenVar(NtsVar(vname,typedef),priminfo) ->
	begin
	  let typedvar_from_def = 
	    get_definition_of_variable_by_name n c vname
	  in
	  if has_type typedvar_from_def typedef then v
	    (** If well typed, then one returns the same variable*)
	  else
	    raise (TypeErrorInVar(v))
	  (** Aborting upon type check failure*)
	  end
	      
	      
    (** Returns the type of constant *)
  let gentype_of_cst c =
    match c with
	CntGenICst (_) -> NtsIntType
      | CntGenFCst(_) -> NtsRealType
      | CntGenBCst (_) -> NtsBoolType	

 

  (** This function only returns the value the type in the
  topmost position of an arithmetical expression tree.*)
 
  let type_of_arithm_tree_header exp =
    match exp with
	CntGenCst(_,t)-> t
      | CntGenSymCst (_,t)-> t
      | CntGenVar(NtsGenVar(NtsVar(_,typedef),_)) -> typedef 
      | CntGenArithmBOp(_,_,_,t) -> t
      | CntGenArithmUOp(_,_,t) -> t

	
 (** Bottom to typecheck : One shall make sure that each subtrees of
      an arithemtical expression is : typed and all subtree that are
      shared by an expression node have the same type.
  *)
	  
  let rec type_gen_arithm_expression n c exp =
    match exp with
	CntGenCst( base_cst , btype) ->
	  begin
	    match btype with
		NtsUnTyped -> 
		  let btype = gentype_of_cst base_cst in
		  CntGenCst( base_cst , btype)
	      | _ ->
		begin
		  let t = gentype_of_cst base_cst in
		  if t = btype then
		    exp
		  else 
		    raise  (TypeErrorInArithmExpr (exp) )
		end
	  end
      
      | CntGenSymCst(symcst,btype) ->
	begin
	  match symcst with
	      CntSymCst (_,t) -> CntGenSymCst(symcst,t) 
	(* I need to check whether this constant is not
	   declared elsewhere using another type*)
	end
	  
      | CntGenVar(v) -> 
	begin
	  let v = type_ntsgen_var n c v in
	  CntGenVar(v)
	end
	  
      |  CntGenArithmBOp (bop,eg,ed,t) ->
	begin
	  let eg = type_gen_arithm_expression n c eg in
	  let ed = type_gen_arithm_expression n c ed in
	  
	  let type_eg = type_of_arithm_tree_header eg in
	  let type_ed = type_of_arithm_tree_header ed in
	  if  type_eg = type_ed  
	  then
	    begin
	      match t with
		  NtsUnTyped -> 
		    CntGenArithmBOp (bop,eg,ed,type_eg)
		| _ ->
		  if t=type_eg 
		  then exp (* Returning the same subtree, as it is
			      already well types*)
		  else (* In this case, subtrees have not the same
			  type as expected*)
		    begin
		      let ex = TypeErrorInArithmExpr(exp) in
		      raise ex
		    end	      
	    end
 	  else
	    begin
	      (** In this case, eg and ed don't have the same type,
	      which is not permitted in the Ntl semantic*)
	       let ex = TypeErrorInArithmExpr(exp) in
	       raise ex
	    end   
	end

      |  CntGenArithmUOp( uop , exp , t ) ->
	begin
	  let exp = type_gen_arithm_expression n c exp in
	  let type_exp =  type_of_arithm_tree_header exp  in
	 
	  match t with
	      NtsUnTyped ->	  
		CntGenArithmUOp( uop , exp , type_exp )
		  
	    | _ ->
	      begin
		if type_exp = t then  
		  CntGenArithmUOp( uop , exp , type_exp )
		else
		  (
		  let ex = TypeErrorInArithmExpr(exp) in
		  raise ex )
	      end     
	end

    

(** Type checking the nts relation, return a well typed
relation upon success, an exception if some operation failed*)


 let rec type_check_ntsgen_rel n c genrel =
   match genrel with
       CntGenRel (bop, eag ,ead ) ->
	 let eag = type_gen_arithm_expression n c eag  in
	 let ead = type_gen_arithm_expression n c ead in	  
	 let type_eag = type_of_arithm_tree_header eag in
	 let type_ead = type_of_arithm_tree_header ead in
	 if type_eag =type_ead 
	 then
	   CntGenRel (bop, eag ,ead )
	 else
	   begin
	     let ex =  TypeErrorInNtsGenRelation( genrel ) in
	     raise ex
	   end

  | CntGenRelComp (bop , relg, reld) ->
    begin
      let relg = type_check_ntsgen_rel n c relg  in
      let reld = type_check_ntsgen_rel n c reld in	 
      CntGenRelComp (bop, relg ,reld )
	
    end
      
  | CntGenNot(rel) ->
    begin
      let rel = type_check_ntsgen_rel n c rel
      in
      CntGenNot(rel)
    end

  | CntGenTrue -> CntGenTrue
  | CntGenFalse -> CntGenFalse
  | CntQVarsGenRel ( vlist , quantifier , relation )
    -> 
    begin
      let  rel = type_check_ntsgen_rel n c relation in
      CntQVarsGenRel ( vlist , quantifier , rel )
    end
    



(** Check whether a call to subsystem is well defined
*)
      
 let vars_have_same_type v1 v2 =
   match v1, v2 with 
       (NtsGenVar(NtsVar(_,t1),_),NtsGenVar(NtsVar(_,t2),_))
       ->
	 if (t1=t2) then true
	 else false

 let compare_var_and_exp_type n c v1 e2 =
   match v1 with
       NtsGenVar(NtsVar(_,t1),_) ->
	 let type_e2 = type_of_arithm_tree_header e2 in
	 (t1 = type_e2 )
	  

 exception CallToUndefinedSubsystem of string
 exception BadNumberofArgumentInCall of string * nts_genrel_arithm_exp list
 exception BadNumberofElemtInLvalsOfCall of string * nts_genrel_var list option
 exception TypeMismatchInArgCall of string * nts_genrel_arithm_exp list
 exception TypeMismatchInRetCall of string * nts_genrel_var list option

 let check_call_type n c sysname opt_retlist arg_list =
   (** *)
   if not (Hashtbl.mem n.nts_automata sysname) then
     raise (CallToUndefinedSubsystem(sysname))
   else ();
   let subs = Hashtbl.find n.nts_automata sysname in
   let well_typed_arg_call =
     try
       (List.for_all2 (compare_var_and_exp_type n c ) subs.input_vars arg_list)
     with
	 Invalid_argument _ -> raise( BadNumberofArgumentInCall(sysname,arg_list))
   in
   if ( not  well_typed_arg_call) then
     raise (TypeMismatchInArgCall(sysname,arg_list))
   else ();

   let well_typed_ret_val =
     try
       match opt_retlist, subs.output_vars with
	   (None,[]) -> true 
	 | (Some(lg),ld) ->
	   begin
	     (List.for_all2 (vars_have_same_type ) lg ld )
	   end
	 | (_,_) -> false
     with
	 Invalid_argument _ -> raise(BadNumberofElemtInLvalsOfCall (sysname,opt_retlist))
   in
   if (not ( well_typed_ret_val)) then
     raise 
       (TypeMismatchInRetCall(sysname,opt_retlist))
   else ();
   (** Here, all is fine*)
   CntGenCall(sysname, opt_retlist, arg_list)
   
   
   

(** Type checking transitions *)




 let type_check_nts_trans_label n c l =
   match l with
       CntGenGuard(r) -> let r = type_check_ntsgen_rel n c r in
			 CntGenGuard(r)
     | CntGenCall(sysname, Some(ret_list), arg_list) ->
       begin
	 (* One need to type vars of ret_list and arg_list and
	 check that their types match with sysname subsystem
	 signature.*)
	 let ret_list = List.map (fun v -> type_ntsgen_var n c v) 
	   ret_list 
	 in
	 let arg_list = List.map (fun v -> type_gen_arithm_expression n c v ) 
	   arg_list 
	 in
	 check_call_type n c sysname (Some(ret_list)) arg_list 
       end

     | CntGenCall(sysname, None, arg_list) ->
       begin
	 (* One need to type vars of ret_list and arg_list and
	 check that their types match with sysname subsystem
	 signature.*)
	 let arg_list = List.map (fun v -> type_gen_arithm_expression n c v ) 
	   arg_list 
	 in
	 check_call_type n c sysname None arg_list 
       end
      
     | CntGenHavoc (_) as h -> h 
			   
      

  (** Types and functions used to generate a control flow graph
      from the numerical transition system description*)
   
 type nts_type_basic_block = Nts_branching_block
			     | Nts_standard_block
   




  type nts_basic_block = {
    block_head_label : string ;
    block_head_state : control;
    block_type : nts_type_basic_block ;

    mutable block : (control * nts_trans_label list * control) list; 
    (** Current control state,
	nts_trans_label_list corresponds
	to what changes/is called before
	transiting*)
    
    mutable block_succs : ( nts_basic_block ref * nts_trans_label list ) list option;
    (** transitions between blocks. Nexts blocks and the transisions being
	described.
	None is in the case the last control state is an error state.
	It's also a convenience for the buiding process.   
    *) 
  } 
    
    
  type nts_automaton_cfg = {
    mutable nts_cfg_name : string; 
    mutable cfg_anot : anotations;
    (*states : (control , unit ) Hashtbl.t;*)
    nts_cfg_init_block : (string , nts_basic_block ) Hashtbl.t;
    nts_cfg_final_block : (string , nts_basic_block ) Hashtbl.t;
    nts_cfg_error_block : (string , nts_basic_block ) Hashtbl.t;
    
    nts_input_vars : nts_genrel_var list; (*Variable ordering is important*)
    nts_output_vars : nts_genrel_var list;
    nts_local_vars : nts_genrel_var list;
    nts_blocks_transitions : ( string , nts_basic_block ) Hashtbl.t
  }



  let pprint_nts_basic_bloc bblock =
    let blocks_list_folder pre (corg,label,cdest) =
      Format.sprintf "%s %s->%s {%s}\n" pre (pprint_control corg)
	(pprint_control cdest)  (pretty_label label)
    in
    let blocks_instr = List.fold_left blocks_list_folder "" bblock.block
    in
    let pprint_type_of_block btype =
      match btype with
	Nts_branching_block -> "branching"
      | Nts_standard_block -> "standard"
    in
    Format.sprintf "block{ label : %s \n head_cstate : %s \n btype : %s\n %s \n}\n"
      bblock.block_head_label 
      (pprint_control bblock.block_head_state)
      blocks_instr
      (pprint_type_of_block bblock.block_type)


(**

 Creates a nts_automaton_cfg with all non mutable fields filled,
using a nts_automaton definition. 

*)
  let nts_automaton_cfg_header_of_cautomaton ca =
    {
      nts_cfg_name = ca.nts_automata_name ;
      cfg_anot = ca.anot ;
      nts_cfg_init_block = Hashtbl.create 97;
      nts_cfg_final_block =  Hashtbl.create 97;
      nts_cfg_error_block = Hashtbl.create 97;
      nts_input_vars = ca.input_vars ;
      nts_output_vars = ca.output_vars ;
      nts_local_vars = ca.local_vars ;
      
      nts_blocks_transitions = Hashtbl.create 97 ;
    }


  let add_init_block_to_nts_cfg block nts_cfg =
    Hashtbl.add nts_cfg.nts_cfg_init_block block.block_head_label block
      
  let add_final_block_to_nts_cfg block nts_cfg =
    Hashtbl.add nts_cfg.nts_cfg_final_block block.block_head_label block

  let add_error_block_to_nts_cfg block nts_cfg =
    Hashtbl.add nts_cfg.nts_cfg_error_block block.block_head_label block

  let add_block_transitions_to_nts_cfg block nts_cfg =
    Hashtbl.add nts_cfg.nts_blocks_transitions block.block_head_label block


  exception Empty_basic_block

  let last_transition_of_basic_block block =
    let rec pick_last_trans blist =
      match blist with
	a::[] -> a
      | [] -> raise Empty_basic_block
      | a::l -> pick_last_trans l
    in
    pick_last_trans block.block

  let get_last_control_state_of_bblock block =
    let (_,_,ret_c) = last_transition_of_basic_block block in
    ret_c

  let is_block_initial block ca =
    is_initial_state ca block.block_head_state

  let is_block_error block ca =
    let (last_translhs,_,last_transrhs) = 
      last_transition_of_basic_block 
	block 
    in
    if not
    ( is_error_state ca last_translhs ) 
    then 
      begin
	is_error_state ca last_transrhs
      end
    else true


  let is_block_final block ca =
    let (last_translhs,_,last_transrhs) =
      last_transition_of_basic_block block 
    in
    if not
    ( is_final_state ca last_translhs ) 
    then 
      begin
	is_final_state ca last_transrhs
      end
    else true

   (** This function adds a block in the initial/final/error/transition
       section w.r.t. the fact that block given as parameter contains
       initial/final/error state or no such control states.
   *)
  let add_block_in_proper_section_of_nts_cfg block nts_cfg ca_def =
    if is_block_initial block ca_def 
    then
      add_init_block_to_nts_cfg block nts_cfg
    else if is_block_final block ca_def
    then
      add_final_block_to_nts_cfg block nts_cfg
      
    else if is_block_error block ca_def 
    then
      add_error_block_to_nts_cfg block nts_cfg
    else
      add_block_transitions_to_nts_cfg block nts_cfg



  let type_of_bblock_starting_with_cstate head_state cautomaton =
    if ( is_cstate_branching head_state cautomaton)
    then Nts_branching_block
    else Nts_standard_block

  (* 
     The types below are used for building the 
  *)
  exception  Basic_block_already_registered of nts_basic_block

  type visited_table = Visited of (control, unit ) Hashtbl.t
  type label_index = Label_index of (string , control ) Hashtbl.t
  type control_label_index = Control_label of (control , string ) Hashtbl.t
  type block_control_index = Control_block_index of ( control, nts_basic_block ) Hashtbl.t
  type block_label_index = Label_block_index of (string , nts_basic_block ) Hashtbl.t


  let make_visited_table () =
    let t = Hashtbl.create 97 in
    Visited(t)
  
  let make_label_index () =
    let t =Hashtbl.create 97 in
    Label_index (t)

  let make_control_label_index () =
    let t = Hashtbl.create 97 in
    Control_label(t)

  let make_block_control_label () = 
    let t = Hashtbl.create 97 in
    Control_block_index(t)
  
  let make_block_label_index () =
    let t = Hashtbl.create 97 in
    Label_block_index(t)


  let is_cstate_visited cstate vtable =
    match vtable with 
      Visited(tbl) ->
	Hashtbl.mem tbl cstate
    
  let mark_cstate_as_visited cstate vtable =
    match vtable with
      Visited(tbl) ->	
	if not (Hashtbl.mem tbl cstate)
	then Hashtbl.add tbl cstate ()
	else ()
 

  let is_cstate_in cstate cindex =
    match cindex with
      Control_label(tbl) ->
	Hashtbl.mem tbl cstate

  let get_label_of_cstate cstate cindex =
    match cindex with
      Control_label(tbl) ->
	Hashtbl.find tbl cstate

  let bind_label_to_cstate label cstate lindex =
    match lindex with
      Label_index(tbl) ->
	Hashtbl.add tbl label cstate
      
  let is_label_in label lindex =
    match lindex with
      Label_index(tbl) ->
	Hashtbl.mem tbl label

  let get_cstate_of_label label lindex =
    match lindex with 
      Label_index(tbl) ->
	Hashtbl.find tbl label
	  
  let bind_cstate_to_label cstate label cindex =
    match cindex with
      Control_label(tbl) ->
	Hashtbl.add tbl cstate label
	  
  let add_block_to_bindex block bindex =
    match bindex with 
      Label_block_index( tbl) ->
	Hashtbl.add tbl block.block_head_label block
	  

  let is_block_of_label_in label bindex =
    match bindex with 
      Label_block_index( tbl) ->
	Hashtbl.mem tbl label

  let get_block_of_label label bindex = 
    match bindex with 
      Label_block_index( tbl) ->
	Hashtbl.find tbl label

  let get_label_of_block block =
    block.block_head_label
    
 let block_of_head cstate cindex blabel_index =
    let label = get_label_of_cstate cstate cindex in
    get_block_of_label label blabel_index


 let append_opt_list a b =
   match a,b with 
     None,None -> None
   | Some(l),None | None,Some(l) -> Some(l)
   | Some(g),Some(d) -> Some(g@d)


  (** creates a basic block that have cstate as head control state
      if this state is not yes contained in cindex. 
  *)

 let opt_block_of_head  cstate cindex bindex =
   try
     let b = block_of_head  cstate cindex bindex in
     Some(b)
   with
     Not_found -> None


  (** Marks a block as visited in vtable*)
 let mark_block_header_as_visited bblock vtable =
   mark_cstate_as_visited bblock.block_head_state vtable
   
 (** Checks whether some block header is marked as visited*)
 let is_block_header_marked_as_visited bblock vtable =
   is_cstate_visited bblock.block_head_state vtable

(* removed first argument btype*)
  let create_block_of_control_state cstate cindex lindex blabel_index 
      ( label_id : int ref)
      cautomaton =
   
    if not (is_cstate_in cstate cindex) 
    then
      begin
	label_id := !label_id + 1;
	let label_of_block =  Format.sprintf "%s_block_%d" cautomaton.nts_automata_name 
	 !label_id in
	bind_cstate_to_label cstate label_of_block cindex;
	bind_label_to_cstate label_of_block cstate lindex;
	let btype =  type_of_bblock_starting_with_cstate cstate cautomaton in
	let ret_block =
	  {
	    block_head_label = label_of_block;
	    block_head_state = cstate;
	    block_type = btype ;
	    block = [];
	    block_succs = None
	  }
	in
	add_block_to_bindex  ret_block blabel_index;
	ret_block
      end
    else
      begin
	let registered_block_label = get_label_of_cstate cstate cindex in
	let block = get_block_of_label registered_block_label blabel_index  in
	raise ( Basic_block_already_registered(block))
      end


 
    (*
      trans_ref_list_of_succs_transition_folder aims at extracting the list
      of the transition description from the blocks when the latter
      are filled and their reference have been initialized.
      
    *)

  let trans_ref_list_of_succs_transition_folder 
      prev_list control_ref translist =
    (control_ref,translist)::prev_list
      


  let get_succs_ref_list_of_block bl =
    match bl.block_succs with
      None -> []
    | Some(tbl_succs) -> 
      tbl_succs


    

    (** 
	_ cstate label : associates control states with a label
	for contril states that are at the begining of a basic
	block.

	_ lindex : associates labels to their basic blocks/ first
	control state of a basic block.
	
    
    
	let fill_basic_blocs cstate_labels visited_state_index cautomaton_ref 
	cfg_build control_start ( bblock_uid_ref : int ref ) =


	fill_basic_block and make_branching_block and make_final_block
	-- i.e. all three functions defined below returns the
	collection of the non visited block successor header. 
	Basically, when a non empty container of basic block
	is returned, it means that :

	_ A block starting at some non yet visited control state
	has been created, and it has been marked as the successor
	of the block that one of the tree functions below was
	called upon.
	Such a block need to be filled, and shall be scheduled for
	filling process, until no more of such block is returned.
    *)



  
  

  let rec fill_basic_block bblock (vtable : visited_table ) 
      (lindex : label_index) (cindex : control_label_index )
      (bindex : block_control_index)
      (blabel_index : block_label_index )
      (pred_relation : inv_relation_container )
      cautomaton 
      (label_id : int ref )  = 
  
    (** Recursively fills a segment and the successor field of the 
    current block.
	Deals with branching control state --those which have more
	than on successor, merging control states.

	This function need to
    *)
    let final_transition_from_state last_control =
      let key_string = Format.sprintf "%s_fake_final_or_error_state" 
	cautomaton.nts_automata_name in
      let fake_id_key = Param.key_val_of_string key_string 
	 in
      let fake_final_state = control_of_id_param fake_id_key in
      (last_control,(Nts_types.CntGenHavoc([])::[]),fake_final_state)

    in
    
    let rec add_elem_of_segment current_control =

      assert (not (is_final_state cautomaton current_control)); 
      let add_transition_to_next_element_if_single_succs 
	  current_control =
	
	
	let out_relation = 
	  (
	    try 
	      Hashtbl.find 
		cautomaton.transitions current_control 
	    with
	      Not_found -> Format.printf "Failed to find control %s Current block_id is : %s ; block head state is : %s \n Current cautomaton is %s \n" (pprint_control current_control) bblock.block_head_label (pprint_control bblock.block_head_state) cautomaton.nts_automata_name; 
		raise Not_found 
	  )
	in
	let (ctr, trans_list ) =  one_binding out_relation in
	(*
let current_control = prepostfix_id_of_control current_control (cautomaton.nts_automata_name^"_") "" in
	let ctr = prepostfix_id_of_control ctr (cautomaton.nts_automata_name^"_") "" 
	in

	*)
	
	bblock.block <- (bblock.block @( (current_control,trans_list,ctr)::[])) ;
	(** In this part we have to deal with two cases : 
	    _ The next control state belongs to the same block
	    _ The next state is at the head of another block,
	    hence one need to recurse after creating a new block
	    or we have to fetch the block which head is the next control
	    state if such a state is already registered in bindex.
	*)
	
	if (is_cstate_in_linear_chain ctr cautomaton pred_relation ) 
	then (* The next state is in the same block : recursion*)
	  add_elem_of_segment ctr
	else 
	  begin 
	    (** The current control state is the last of the
		current block. Hence one need to create and 
		label the next block if it is not yet referenced in bindex 
		--block index, and select this block as the current block
		  only successor.
	    
		If the "next" block is alredy references, no need to
		create it, just add it as only successor.
	    *)

	    try
	      let next_block_ref =  
		ref (block_of_head ctr cindex blabel_index) 
	      in
	      let trans_next_list =  Some((next_block_ref, trans_list)::[]) 
	      in
	      bblock.block_succs <- trans_next_list;
	      get_succs_ref_list_of_block bblock
	    with
	      Not_found ->
		begin
		  
		  let next_block_ref = ref 
		    ( create_block_of_control_state ctr
			cindex lindex blabel_index label_id cautomaton)
		  in
		  let trans_next_list = Some((next_block_ref,trans_list)::[]) 
		  in
		  bblock.block_succs <- trans_next_list;
		  get_succs_ref_list_of_block bblock
		end
	  end
	    
      in
      if is_cstate_in_linear_chain current_control cautomaton pred_relation
      then
	begin
	  add_transition_to_next_element_if_single_succs current_control
	end
      else 
	if (is_cstate_branching current_control cautomaton)
	then 
	(*
	  One return the collection of successor block, one need
	  create, label and reference them in bindex, if not yet done.
	  One need to associate the head control state of each following
	  block with the block label in cindex.
	*)
	begin
	  assert (current_control = bblock.block_head_state);
	  let branching_block = 
	    make_branching_blocks bblock vtable
	      lindex cindex blabel_index cautomaton label_id 
	  in
	  get_succs_ref_list_of_block branching_block
	  
	  
	end
      else if (is_cstate_merge_point current_control pred_relation)
      then
	begin
	  assert (current_control = bblock.block_head_state);
	  
	  (*
	    One single successor, however it's a mergepoint, hence
	    it must be at the head of a block. The same process as above
	    needs to be applied upon this same control state/block Head.
	  *)
	      
	  add_transition_to_next_element_if_single_succs current_control
	end
	
     
      else if ( not (has_successor current_control cautomaton))
      then  
	(*
	  No outgoing transition from the current control block.
	  one need to return an empty successor descriptor.
	*)
	begin
	  block_of_final_statement bblock vtable cautomaton;
	  [] (*Returning the set of successor as the empty set*)
	end
      else
	begin
	  Format.printf "Neither Branching, nor merging nor linear control
state %s\n%!" (pprint_control current_control);
	  Format.printf "Counter automaton is : %s \n %!" 
	    ( pprint_to_nts cautomaton );
	  if is_cstate_in_linear_chain current_control cautomaton pred_relation then Format.printf "%s is in a linear component \n %!" (pprint_control current_control )
      else Format.printf "%s is not in a linear component \n %!"(pprint_control current_control) ;
      assert false
	end
    in  
    (
      if (bblock.block_type = Nts_standard_block) 
      then
	begin
	  if (is_error_state cautomaton bblock.block_head_state) ||
	    (is_final_state cautomaton bblock.block_head_state)
	  then
	    (
	      bblock.block <-
		(final_transition_from_state bblock.block_head_state)::[]; 
	      [] 
	    )
	  
	  else
	  add_elem_of_segment bblock.block_head_state
	end
      else 
	begin
	  let branching_block = 
	    make_branching_blocks bblock vtable
	      lindex cindex blabel_index cautomaton label_id 
	  in
	  get_succs_ref_list_of_block branching_block
	end
	(*let ret_v = list_of_opt_list
	bblock.block_succs in 
	ret_v*)
    ) 

	    
  (* In this case, the current element given as parameter, is
     the first element of another block. It is either branching
     or have many predecessors, or have no succesor at all.*)
	  
 	  
  (* When several transitions go out of a control state :
      
      s_i -> s_j1 {guard_1 and op_1}
      ...
      s_i -> s_jn {guard_n and op_n}

      one create :

      s1 -> s'_i {havoc}
      and 
      for all 1<= k <= n

      s_jk->s_jk' {op_k}

      and 

      block(s_i) -> block(s_jk) {guard_k}

      Here block(s_i) describes the block that contains the single
      transition s_i->s_i'{havoc}
  *)


  and make_branching_blocks bblock (vtable : visited_table) 
      (lindex : label_index) (cindex : control_label_index)
      (blabel_index : block_label_index) cautomaton
      (label_uid_counter : int ref ) =
      

    (*
      let (bblock_head,_,_) = List.hd bblock.block in
      assert (is_cstate_branching cautomaton);
    *)

      assert( is_cstate_branching bblock.block_head_state cautomaton );
    
    (* I need to split transitions label between guards and operations
       Not yet done.
    *)
    let dummy_cstate_id = Param.key_val_of_string "dummy_branching_block_state" in
    let dummy_cstate = control_of_id_param dummy_cstate_id in
    let single_trans = (bblock.block_head_state,Nts_types.CntGenHavoc([])::[],
			dummy_cstate) 
    in
    bblock.block <- single_trans::[];
    
    
    let basic_block_iterator  next_cstate trans_list =
      let oblock = opt_block_of_head next_cstate cindex blabel_index in
      
      match oblock with
	Some(blck) ->
	  begin
	    let bref = ref blck 
	    in
	    let trans_descr = Some((bref,trans_list)::[]) in
	    bblock.block_succs <- (append_opt_list trans_descr bblock.block_succs)
	  end
	    
      | None ->
	begin
	  let succs_bloc_ref = ref (create_block_of_control_state 
				      next_cstate cindex lindex blabel_index
				      label_uid_counter cautomaton) 
	  in
	  let trans_descr = Some((succs_bloc_ref,trans_list)::[]) 
	  in
	  bblock.block_succs <- (append_opt_list trans_descr bblock.block_succs)
	end
	  
    in
    let succs_state_table = Hashtbl.find cautomaton.transitions 
      bblock.block_head_state in
    Hashtbl.iter basic_block_iterator succs_state_table;
    bblock
      
      
    (** This function fills a block which head control state is
	a final control state in cautomaton. One just add a dummy
	transition, and the set of successor block is set as the empty
	set.
    *)
   
 
    and block_of_final_statement bblock vtable cautomaton =
      assert (is_final_state cautomaton bblock.block_head_state);
      let dummy_cstate_id = Param.key_val_of_string  "fake final contol state" in
      let dummy_cstate = control_of_id_param dummy_cstate_id in
      let single_trans = (bblock.block_head_state,(Nts_types.CntGenHavoc([])::[]),
			  dummy_cstate) 
      in
      bblock.block <- ( single_trans :: []);    
      bblock.block_succs <- None
    
	
  (*let cfg_of_nts_automaton cautomaton = *)



  let blocks_compression_of_nts_automaton cautomaton =
    (** scheduler contains the set of the basic blocks that have not
    yet be filled. 

    Initially, this queue contains the set of blocks that starts with
    the set of initial states.
    *)

    let label_uid = ref 0 in
    let vtable = make_visited_table () in
    let lindex = make_label_index () in
    let cindex = make_control_label_index () in
    let bindex = make_block_control_label () in
    let blabel_index = make_block_label_index () in
   
    let pred_relation = compute_pred_relation cautomaton in
      

    let ret_nts_cfg = nts_automaton_cfg_header_of_cautomaton cautomaton in
    
    let scheduler = Queue.create () 
    in
    (* Iterator used to schedule the initial basic blocks*)
    let initialize_scheduler_iterator control_state =
      let new_block_ref  = ref(
	create_block_of_control_state control_state cindex lindex
	  blabel_index label_uid cautomaton)
      in
      Queue.push new_block_ref scheduler
    in

    (* Actual initialisation of the basic blocks *)
    iter_state_container cautomaton.init_states initialize_scheduler_iterator ;

    (* This iterator is used in the main loop, to schedule the non yet
    filled basic-blocks for processing *)
    let schedule_next_element_iterator (bblock_ref, _ ) =
      if not (is_block_header_marked_as_visited (!bblock_ref) vtable ) 
      then
	Queue.push bblock_ref scheduler
      else
	()
    in
    
   

    (* 
       Here is the main loop. 
    *)
   
    
    while (not (Queue.is_empty scheduler )) 
    do
      let curr_block_ref = Queue.pop scheduler 
      in
      if (is_block_header_marked_as_visited  !curr_block_ref vtable)
      then () (* If several blocks points to the same one,
	      the latter will be scheduled several times. One
	      make sure to deal with this case by dropping those
	      already dealt with once alredy marked as visited
	       *)
      else 
	begin
	  mark_block_header_as_visited !curr_block_ref vtable;
	  
	  let successors_list = fill_basic_block 
	    (!curr_block_ref) vtable lindex cindex
	    bindex blabel_index pred_relation cautomaton label_uid
	  in
	  Format.printf "current block is %s \n %! " 
	    (pprint_nts_basic_bloc 
	       !curr_block_ref);
	  List.iter schedule_next_element_iterator successors_list;
	  add_block_in_proper_section_of_nts_cfg !curr_block_ref ret_nts_cfg 
	    cautomaton;
	  
	end
    done;
    ret_nts_cfg
	



	
end
