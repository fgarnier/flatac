open Nts_functor
open Nts_types
open Hashtbl
open Nts_generic


module Make = 
  functor ( Param  : Nts_functor.NTS_PARAM ) ->
    struct
      module NFParam=Nts_functor.Make(Param)
      type anotations = NFParam.anotations
      type control = NFParam.control
	  
      type nts_automaton = NFParam.nts_automaton
      type nts_system = NFParam.nts_system

      open NFParam


      let pprint_control = NFParam.pprint_control
	
	
	
      let dot_of_init_nodes (ca : nts_automaton) =
	let in_folder control () prefix =
	  Format.sprintf "%s %s [label=\"initial\";color=blue];\n" 
	    prefix (pprint_control control) 
	in
	Hashtbl.fold  in_folder ca.NFParam.init_states ""
     
	  
      let dot_of_final_nodes (ca : nts_automaton) =
	let in_folder control () prefix =
	  Format.sprintf "%s %s [label=\"final\";color=green];\n" 
	    prefix (pprint_control control) 
	in
	Hashtbl.fold  in_folder ca.NFParam.final_states ""
	  
	  
      let dot_of_error_nodes (ca : nts_automaton) =
	let in_folder control () prefix =
	  Format.sprintf "%s %s [label=\"error\";color=red];\n" 
	    prefix (pprint_control control) 
	in
	Hashtbl.fold in_folder ca.NFParam.error_states	""
	  
	  
      let transitions_of_nts_automaton  (ca : nts_automaton ) prefix  =
	let inner_folder outter_control  inner_control _ prefix =
	  Format.sprintf "%s %s_%s->%s_%s;\n" prefix ca.NFParam.nts_automata_name (pprint_control outter_control) 
	    ca.nts_automata_name (pprint_control inner_control )
	in
	let outter_folder outter_control inner_table prefix =
	  Hashtbl.fold (inner_folder outter_control) inner_table prefix
	in
	Hashtbl.fold  outter_folder ca.transitions prefix
	  

      let dot_of_cautomaton ?(standalone_graph = false) (ca : nts_automaton )=
	
	let ret_str = ( if standalone_graph then
	    Format.sprintf " digraph %s {" ca.nts_automata_name 
	  else 
	    Format.sprintf " subgraph %s {" ca.nts_automata_name 
	)
	in
	let res = Format.sprintf "%s %s %s %s %s } " ret_str 
	  (dot_of_init_nodes ca) (dot_of_error_nodes ca)
	  (dot_of_final_nodes ca) (dot_of_transitions ca)
	in 
	res
	
	

      let dot_of_nts (nt : nts_system ) =
	let automata_folder name caut pre_str =
	  Format.sprintf "%s \n %s " pre_str (dot_of_cautomaton nts_automaton)
	in
	let automata_dump = 
	  Hashtbl.fold automata_folder ca.nts_automata [] 
	in
	let ret_string = 
	  Format.sprintf "digraph %s { %s }" nt.nts_system_name automata_dump
	in
	ret_string
	
	
	
	  
    end;;