(*


Need to add an appropriated copyright Notice at some point.


This files belongd to the FLATA-C plugin.

Verimag 2011


 A compact implementation of the Ecfg, with an emphasis on performance.
 This file is a part of the flatac plugin for FRAMA-C Carbon.

 Florent Garnier and Maxime Gaudin.

*)



open Self
open Cil
open Cil_types
open Cfg
open Visitor
open Sem_and_logic_front_end
open Nts_types
open Extended_cfg_types
open Intermediate_language
open Cnt_interpret
open Queue
open Var_registration


exception Entry_point_not_registered 
exception Entry_point_already_registered 
exception Marking_unregistered_vertex of ecfg_id
exception Ecfg_vertex_not_registered
exception No_outgoing_edges_from_state of ecfg_id
exception No_such_state_id
exception Building_an_edge_between_inexisting_node_ids of ecfg_id
exception Label_already_registered_for_this_edge
exception Not_a_switch_stmt

exception Debug_exception of string



let get_id_of_ecfg_id ( id : ecfg_id) =
  match id with
      Ecfg_id(i) -> i


let make_empty_cil_statement () =
  {
    labels = [] ;
    skind = UnspecifiedSequence ([]) ;
    sid = -1 ;
    succs = [] ;
    preds = [] ;
    ghost = true ;
  }


let make_empty_cil_statement_with_successor ( successor : Cil_types.stmt) =
   {
    labels = [] ;
    skind = UnspecifiedSequence ([]) ;
    sid = -2 ;
    succs = (successor::[]) ;
    preds = [] ;
    ghost = true ;
  }
  


module Extended_cfg_definition  = 
  functor ( A : sig type abstract_type type label_type end ) ->
struct
  module Extended_cfg_base_types = Extended_cfg_types ( A ) 
  open Extended_cfg_base_types
  
  


  

  
  class extended_cfg (name_function : string )(finfo : Cil_types.file) (funinfo : Cil_types.fundec) 
    frontend   = object(self)
	
    val fun_def= funinfo
    val mutable name = name_function 
    val max_args_of_all_callee = Ast_goodies.max_args_numbers_of_callees funinfo.sallstmts  
   
    val mutable intermediate_sid = 0 
    val mutable is_computed = false
    val mutable entry_point_set = false (* Initial control state of the 
					ecfg set ?*)

    val mutable nts_sformal = [] (* Must be replaced when the extended
				 cfg class will inherit from the nts
				 class.*)
    val mutable nts_outval = [] (* same as above.*)
    val mutable nts_slocals= []


    val not_visited_vertices = Queue.create ()

    val mutable front_end :  ( (Extended_cfg_base_types.abs_dom_val, 
				  Extended_cfg_base_types.trans_label_val)
                                 sem_and_logic_front_end ) = frontend 

    (*val mutable absval_of_ep = Ssl_valid_abs_dom.create_validity_abstdomain ()*)

    
    val edges : ( ecfg_id , (ecfg_id , trans_label_val ) Hashtbl.t ) Hashtbl.t = 
      Hashtbl.create init_hashtbl_size
    
    val edges_inv : ( ecfg_id, (ecfg_id , unit) Hashtbl.t ) Hashtbl.t =
      Hashtbl.create init_hashtbl_size
    
    val vertices : (ecfg_id , ecfg_vertex) Hashtbl.t = 
      Hashtbl.create init_hashtbl_size

    (** The four next hashtbl contain the indexes ot the initial
    states, final states and errors states. *)
    val init_state : ( ecfg_id , unit ) Hashtbl.t = 
      Hashtbl.create init_hashtbl_size
    val final_state : ( ecfg_id , unit ) Hashtbl.t = 
      Hashtbl.create init_hashtbl_size
    val error_state : ( ecfg_id , unit ) Hashtbl.t =
       Hashtbl.create init_hashtbl_size
    (* The common state table contains the index of all states that
    don't fall in any category above.*)
    val common_state : ( ecfg_id , unit ) Hashtbl.t =  
      Hashtbl.create init_hashtbl_size
	
    (** The key shall be the Cil_stmt.sid and the second element contains
     all the ids of the ecfg nodes that were visited.*)
    (*val visited_index : ( sid_class ,  (ecfg_id ,  unit ) Hashtbl.t ) Hashtbl.t =
      Hashtbl.create init_hashtbl_size*)

    val visited_nodes_id = Hashtbl.create init_hashtbl_size
      
    (* The key corresponds to the Cil_type.stmt.sid and the corresponding
       hash table contains all the id of the note of the ecfg which have
       the same sid as the key.*)
      
    val unfoldsid_2_abs_map : (sid_class , (ecfg_id , unit ) Hashtbl.t) Hashtbl.t =
      Hashtbl.create init_hashtbl_size
	
    (* Associates to each
       abstract id the corresponding
       sid. Used to spare a double
       call on Hashtbl.find.
       Encodes the inverse relation of unfoldsid_2_abs_map.
    *)
    val fold_abs_map_2_sid : ( ecfg_id , sid_class ) Hashtbl.t =
      Hashtbl.create init_hashtbl_size
                                                   
    val mutable current_node_id = (Ecfg_id (0)) 
                            (*    Basically, the total number of
				  nodes, as well as the id of the next
				  to be created node, if any.
			     *)
      
    initializer (*Cfg.cfgFun funinfo;*) self#register_in_out_nts_vars ();
      self#register_local_vars(); self#register_ecfg_entry_point funinfo;
      self#build_ecfg ()

    method private incr_current_node_id () =
      match current_node_id with
	  Ecfg_id(i) -> 
	    let nid = i+1 in
	    (current_node_id <- Ecfg_id(nid))

	

      
    method get_name () =
      name

    method private mark_as_visited (e : ecfg_id ) =
      if not (Hashtbl.mem visited_nodes_id e) then
	Hashtbl.add visited_nodes_id e ()

    method private is_visited (e : ecfg_id ) =
       Hashtbl.mem visited_nodes_id e 
	

    method register_in_out_nts_vars () =
      let in_out_map_folder (nts_var_list) (v : Cil_types.varinfo ) =
	match v.vtype with
	    TPtr(_,_) ->
	      begin
		NtsIVar("offset__"^v.vname^"_")::(NtsIVar("validity__"^v.vname^"_")::nts_var_list)
	      end
	  | _ ->
	    begin
	      match (Composite_types.is_integer_type v.vtype) with
		  Some(_) -> NtsIVar(v.vname)::(NtsIVar("validity__"^v.vname^"_")::nts_var_list)
		| None -> NtsMiscType(v.vname)::nts_var_list
	    end
      in 
      nts_sformal <- (List.fold_left in_out_map_folder [] funinfo.sformals  )
      


	
    method register_local_vars () =
      let in_out_map_folder (nts_var_list) (v : Cil_types.varinfo ) =
	match v.vtype with
	    TPtr(_,_) -> NtsIVar("offset__"^v.vname^"_")::nts_var_list
	  | _ ->
	    begin
	      match (Composite_types.is_integer_type v.vtype) with
		  Some(_) -> NtsIVar(v.vname)::nts_var_list
		| None -> NtsMiscType(v.vname)::nts_var_list
	    end
      in 
	nts_slocals <- (List.fold_left in_out_map_folder [] funinfo.slocals  );
	for i=0 to max_args_of_all_callee do
	  nts_slocals <- (Nts.name_ndet_arg i)::nts_slocals
	done;
	nts_slocals <- ((NtsIVar("__ndet_cond__"))::(NtsIVar("__if_ndet_cond__"))::nts_slocals)
 



    method is_label_registered dest_ref label_ref 
      (dest_table : (ecfg_id, trans_label_val) Hashtbl.t ) =
      let answer_iterator (dest_ref : ecfg_id)
	(label_ref : trans_label_val) dest label =
	if (front_end#equals_labels label  label_ref) && 
	  ( dest = dest_ref) 
	then
	  raise Label_already_registered_for_this_edge
	else ()
      in
      try
	Hashtbl.iter (answer_iterator dest_ref label_ref) dest_table;
	false
      with
	| Label_already_registered_for_this_edge -> true


      
    (** Adds a vertex to the ecfg*)
    method private add_abstract_state ( s : Cil_types.stmt ) 
      ( absval : abs_dom_val ) =      
      let new_vertex = {
	id = current_node_id;
	statement = s;
	abstract_val = absval ; 	
      } 
      in
	Hashtbl.add vertices current_node_id new_vertex;
	if Hashtbl.mem unfoldsid_2_abs_map (Sid_class(s.sid)) 
	then 
	  begin
	    let entry_table = Hashtbl.find unfoldsid_2_abs_map (Sid_class(s.sid))  
	    in
	      Hashtbl.add entry_table current_node_id ()
	  end
	else
	  begin
	    let entry_table = Hashtbl.create init_hashtbl_size in
	      Hashtbl.add entry_table current_node_id ();
	      Hashtbl.add unfoldsid_2_abs_map (Sid_class(s.sid)) entry_table
	  end;
	Hashtbl.add fold_abs_map_2_sid new_vertex.id (Sid_class(s.sid));
	self#incr_current_node_id ();
	
	begin
	  match new_vertex.id with
	      Ecfg_id(id) ->
		Format.printf "[ Extended cfg Adding : Ecfg node id  : %d, Framac sid %d ] \n " new_vertex.statement.sid id
	end;
	if (front_end#is_error_state absval || 
	      front_end#is_control_state_erroneous s.skind )then
	  begin
	    if Hashtbl.mem error_state new_vertex.id
	    then ()
	    else
 	      Hashtbl.add error_state new_vertex.id ()
	  end
	    
	else if ( (List.length s.succs) == 0 ) then
	  begin
	    if Hashtbl.mem final_state new_vertex.id
	    then ()
	    else
	      Hashtbl.add final_state new_vertex.id ()
	  end	
	else
	  
	  Hashtbl.add common_state new_vertex.id ();
	new_vertex.id (**  Returns the id of the created vertex*)
	  

    (** Adds a labelled edge between two vertexes of the
	extended control flow graph.*)


    method private register_edge (origin : ecfg_id ) ( dest : ecfg_id )
      (label : trans_label_val ) =
      (*Used to check whether there alredy exists a (dest,label) binding
      in order to ensure uniquedeness of edges labelling*)
      
      try
	if not (Hashtbl.mem edges origin) then
	  begin
	    let tabl_for_origin = Hashtbl.create init_hashtbl_size in
	    Hashtbl.add edges origin tabl_for_origin
	  end;
	
	let entry_tab =( Hashtbl.find edges origin) in
	  begin
	  if not (self#is_label_registered  dest label entry_tab) 
	  then
	    Hashtbl.add entry_tab dest label
	  else ()
	  end;
	  if Hashtbl.mem edges_inv dest then
	    begin
	      let reverse_table = Hashtbl.find edges_inv dest in
		Hashtbl.add reverse_table origin ()
	    end
	  else
	    begin
	      let reverse_table = Hashtbl.create init_hashtbl_size in
		Hashtbl.add reverse_table origin () ;
		Hashtbl.add edges_inv dest reverse_table 
	    end
	      (* store that post has pre as predecessor, obvious isn't it ?*)   
      with
	  Not_found -> raise (Debug_exception (" method register edge, Not_found caught"))	    
	    
    (** This method checks whether some abstract states 
       (sid , absdomvalue) is not entailed by another
	abstract state (sid, abv), i.e. abv |- absdomvalue.
	If no abs value is associated to this sid, then
	the method answers ( false , -1 ).
	Otherwise, it returns a tuple ( true ,  sid'), where
        abs' |- absdomvalue .
    *)
	    

    method private register_ecfg_entry_point ( funinfo : Cil_types.fundec ) =
      
      if entry_point_set then raise Entry_point_already_registered 
      else
	begin 
	  
	  let absval_of_ep = front_end#get_entry_point_from_fundec finfo funinfo in
	  let first_cil_statement_of_cfg = List.hd funinfo.sallstmts in
	  let stmt_of_ecfg_entry_point =  
	    make_empty_cil_statement_with_successor first_cil_statement_of_cfg 
	  in
	  let id_ep = self#add_abstract_state stmt_of_ecfg_entry_point 
	    absval_of_ep in
	  Queue.push id_ep not_visited_vertices;
	  Hashtbl.add init_state id_ep ();
	  entry_point_set <- true 
	end
	   


    method entailed_by_same_id_absvalue  (next_stmt : Cil_types.stmt)
      ( absval : abs_dom_val ) =
      let entail_folder (id_abs_brothers : ecfg_id ) () (already_found :(bool * ecfg_id ) ) =
	match already_found with
	    ( true , _ ) ->
	      already_found
	  | ( false , _ ) ->
	    let brother_ecfg_node = Hashtbl.find vertices id_abs_brothers in
	    let brother_abs = brother_ecfg_node.abstract_val in
	    if ( front_end#entails  brother_abs absval) 
	    then
	      (true , id_abs_brothers)
	    else 
	      already_found
      in
      try
	let brotherhood_abs = Hashtbl.find unfoldsid_2_abs_map (Sid_class(next_stmt.sid)) in
	let candidate = (Hashtbl.fold entail_folder brotherhood_abs (false , Ecfg_id( - 2 ))) in
	candidate
     
      with
	  Not_found -> (false ,Ecfg_id(-1)) 
	    

 
   (*
      This operation takes as input the current state and the next abstract
      state, and :
      If there exits an abstract state in the extended cfg that entails/(
      implies) the "asbtract domain valuation" of the next state, with the
      mame Cil_types.sid, then one adds an edge from the current state to
      this state. Otherwise, create a new abstact state, and add an edge
      between the current vertex and the new one, labelled using the
     label parameter.
    *) 

    method add_transition_from_to ( current : ecfg_vertex ) 
      (next_stmt : Cil_types.stmt ) (next_abs : abs_dom_val ) 
      ( label : trans_label_val) =
      try
	Format.printf "Entering add transition_from_to \n";
	let is_entailed_abstraction = self#entailed_by_same_id_absvalue 
	  next_stmt next_abs in
	  begin
	    match is_entailed_abstraction with
		(false , _ ) -> 
		  begin
		    Format.printf "Add transition from to Adding new node to ecfg \n";
		  let new_abs_state_id = 
		    self#add_abstract_state next_stmt next_abs in
		    self#register_edge current.id new_abs_state_id label;
		  if (not (front_end#is_error_state next_abs ))
		  then
		    begin
		      Format.printf 
			"[add_transition_from_to] Queuing a new ecfg node id \n";
		       Queue.push new_abs_state_id not_visited_vertices
		    end
		  else
		    begin
		      Format.printf "Next state is an error state. not schduled for travesal. This state goes nowhere \n";
		      Hashtbl.add error_state new_abs_state_id ()
		    end
		  
		  end
	      | (true , entailing_state_id ) ->
		 Format.printf "Not adding a new node, creating a new edge\n";
		  self#register_edge current.id entailing_state_id label;
		  if not( self#is_visited entailing_state_id ) then
		    Queue.push entailing_state_id not_visited_vertices
	  end
      with
	  Not_found -> 
raise (Debug_exception("In method add_transition_from_to, a Not_found exception was raised"))



 (* Pre and post reprensent the identificators of the abstract states,
 i.e. states in the sid * abs_dom_val cross product.*)
    method add_edge_by_id (pre : ecfg_id) (post : ecfg_id ) (label : trans_label_val) =
      try
	let entry_table = Hashtbl.find edges pre in
	Hashtbl.add entry_table post label;
	let reverse_table = Hashtbl.find edges_inv post in
	Hashtbl.add reverse_table pre;
      (* store that post has pre as predecessor, obvious isn't it ?*)
	    
      with
	  Not_found -> raise (Debug_exception("Method add_edge_by_id, Not_found caught"))  

	    
    method private get_succs_of_ecfg_node ( node : ecfg_vertex ) =
      try
	let prim_table = Hashtbl.find edges (node.id) 
	in
	  prim_table
      with
	  Not_found -> 
	    raise  (No_outgoing_edges_from_state(node.id))
	      
	      
	      
    method private get_abstract_succs_of_ecfg_node (node : ecfg_vertex)
      (succs_stmt : Cil_types.stmt )=
  
      let current_absvalue =
	node.abstract_val (*This value is copied in the next method
			    of the front end.*)
      in
      let empty_label = 
	front_end#get_empty_transition_label () in
	front_end#next current_absvalue empty_label 
	  succs_stmt.skind
    


    method private add_to_not_visited_iterator (current_node : ecfg_vertex)
      (next_stmt : Cil_types.stmt) 
      ((abs , label ) :( abs_dom_val * trans_label_val )) =
      
      let is_entailed_by_existing_vertex_abs = 
	self#entailed_by_same_id_absvalue next_stmt abs 
      in
	
      match is_entailed_by_existing_vertex_abs with
	  (true, more_genid ) ->
	    begin
	      self#register_edge current_node.id more_genid label
		(*
		if (not (self#is_visited more_genid)) then 
		  (Queue.push more_genid not_visited_vertices)*) 
		  (* Shall I remove
		  that ?  Queue.push at some point ? It's not obvious
		  that it shall be there.*)
	    end
	| (false , _ ) ->
	  begin
	    let new_ecfg_vertex_id = 
	      self#add_abstract_state next_stmt abs in
	    self#register_edge current_node.id new_ecfg_vertex_id label;
	    if (not (front_end#is_error_state abs || 
		       front_end#is_control_state_erroneous next_stmt.skind))
	    then
	      begin
		Format.printf "Scheduling another vertex for execution \n";
		Queue.push new_ecfg_vertex_id not_visited_vertices
	      end
	      else 
	      begin
		Format.printf "This state is an error state, I'm not going to traverse it.\n";
		self#mark_as_visited new_ecfg_vertex_id
	
	      end
	  end
	    

    method private register_swich_statement_successors
      current_node swith_stmt_succs_list =
      match swith_stmt.skind with
	  Switch(exp_test, block_sw , stmt_succs, _) ->
	    begin
	      let broken_mem_abs = 
		front_end#copy_absdom_label current_node.abstract_val in
	      font_end#make_absdom_errorval broken_mem_abs;
	      
	      
	      let broken_mem_test = 
	      
	    end
	  
	| _ -> 
	  raise Not_a_switch_stmt

	  

	    (*  Create ecfg nodes for If stmt successors if necessary
	    and then adds the labelled edges between the current state
		and the two next ones.*)
	    
    method private register_if_statement_successors 
      current_node ((abs_true,trans_true),(abs_false,trans_false),(abs_mem_broken,trans_mem_broken)) 
      (true_stmt_opt,false_stmt_opt) =

      match true_stmt_opt,false_stmt_opt with
	  (None,None) ->
	    
	    begin
	       Format.fprintf Ast_goodies.debug_out "[If test, successors (None,None) ] \n %!";
	      (*Degenerated case, both then and else blocks are
	      empty. In this case one need to consider two transitions :
	      The first one reach the successor of the test, without
	      changing the abstract value if the test doesn't generate
	      any memory fault.
		Second case : transits to the successor state in the case
		when the test performs a memory fault for evaluation the 
		if conditions.
	      *)
	      
	      let succs_mem_valid_stmt =  List.hd current_node.statement.succs in
	      let succs_mem_broken_stmt = List.hd current_node.statement.succs in
	      let succs_abs_mem_valid = 
		front_end#copy_absdom_label current_node.abstract_val
	      in
	      let succs_abs_mem_invalid =
		front_end#copy_absdom_label current_node.abstract_val in
	      front_end#make_absdom_errorval succs_abs_mem_invalid;
	     
	      let valid_access_test_mem_guard = 
		front_end#positive_guard_from_error_guard trans_mem_broken
	      in
	      let nexts_of_succs_mem_valid_stmt = 
		front_end#next succs_abs_mem_valid valid_access_test_mem_guard 
		  succs_mem_valid_stmt.skind
	      in
	      let nexts_of_succs_mem_broken_stmt =
		front_end#next succs_abs_mem_invalid  
		  trans_mem_broken succs_mem_broken_stmt.skind
	      in
 
	      List.iter (self#add_to_not_visited_iterator current_node 
			   succs_mem_valid_stmt)  
		nexts_of_succs_mem_valid_stmt ;
	      
	      
	       (*(succs_abs_mem,valid_access_test_mem_guard);*)
	      
	      List.iter (self#add_to_not_visited_iterator current_node 
			   succs_mem_valid_stmt) 
		nexts_of_succs_mem_broken_stmt
	
	    end

	| (Some(true_stmt),None) ->
	  begin

	     Format.fprintf Ast_goodies.debug_out "[If test, successors (Some(true_stmt),None) ] \n %!";
	  (*four successors : test_true and valid_mem_op to true_stmt
	      test_true and not valid_mem_op to (true_stmt , bot)
	      test_false and valid_mem_op to (if_stmt.succs.nth 1, abs)
	      test_false and not valid_mem_op to (if_stmt.succs nth 1, bot)
	    *)
	    let succs_mem_valid_stmt = true_stmt in
	    let succs_mem_valid_abs = 
	      front_end#copy_absdom_label current_node.abstract_val
	    in
	    let succs_mem_invalid_stmt = true_stmt in
	    let succs_mem_invalid_abs =  
	      front_end#copy_absdom_label current_node.abstract_val in
	    front_end#make_absdom_errorval succs_mem_invalid_abs;
	    let fail_mem_valid_stmt = List.nth current_node.statement.succs 
	      0 in
	    let fail_mem_valid_abs = 
	      front_end#copy_absdom_label current_node.abstract_val
	    in
	    let fail_mem_invalid_stmt = List.nth current_node.statement.succs 
	      0 in
	    let fail_mem_invalid_abs = 
	      front_end#copy_absdom_label current_node.abstract_val in
	    front_end#make_absdom_errorval fail_mem_invalid_abs;
	    

	    let nexts_of_succs_mem_valid_stmt = 
		front_end#next succs_mem_valid_abs trans_true 
		  succs_mem_valid_stmt.skind
	    in
	    let nexts_of_succs_mem_broken_stmt =
	      front_end#next succs_mem_invalid_abs  
		trans_mem_broken succs_mem_invalid_stmt.skind
	    in

	    let nexts_of_fail_mem_valid_stmt =
	      front_end#next fail_mem_valid_abs trans_false 
		fail_mem_valid_stmt.skind
	    in
	    let nexts_of_fail_mem_invalid_stmt =
	      front_end#next fail_mem_invalid_abs trans_mem_broken
		fail_mem_invalid_stmt.skind
	    in
	  
	    List.iter (self#add_to_not_visited_iterator current_node 
	      succs_mem_valid_stmt)  nexts_of_succs_mem_valid_stmt;
	    List.iter (self#add_to_not_visited_iterator current_node
	      succs_mem_invalid_stmt)  nexts_of_succs_mem_broken_stmt;
	    List.iter (self#add_to_not_visited_iterator current_node 
	      fail_mem_valid_stmt) nexts_of_fail_mem_valid_stmt;
	    List.iter (self#add_to_not_visited_iterator current_node
	      fail_mem_invalid_stmt) nexts_of_fail_mem_invalid_stmt 

	  end
	    
	| (None,Some(false_stmt)) ->
	  begin
	    Format.fprintf Ast_goodies.debug_out "[If test, successors (None,Some(false_stmt)) ]  \n %!";
	      
	    let succs_mem_valid_stmt = List.nth current_node.statement.succs
	      1
	    in
	    let succs_mem_valid_abs = 
	      front_end#copy_absdom_label current_node.abstract_val
	    in
	    let succs_mem_invalid_stmt = List.nth 
	      current_node.statement.succs 1
	    in
	    let succs_mem_invalid_abs =  
	      front_end#copy_absdom_label current_node.abstract_val in
	    front_end#make_absdom_errorval succs_mem_invalid_abs;
	    let fail_mem_valid_stmt = false_stmt 
	    in
	    let fail_mem_valid_abs = 
	      front_end#copy_absdom_label 
		current_node.abstract_val
	    in
	    let fail_mem_invalid_stmt = false_stmt in
	    let fail_mem_invalid_abs = 
	      front_end#copy_absdom_label 
		current_node.abstract_val
	    in

	    let nexts_of_succs_mem_valid_stmt = 
		front_end#next succs_mem_valid_abs trans_true 
		  succs_mem_valid_stmt.skind
	    in
	    let nexts_of_succs_mem_broken_stmt =
	      front_end#next succs_mem_invalid_abs  
		trans_mem_broken succs_mem_invalid_stmt.skind
	    in

	    let nexts_of_fail_mem_valid_stmt =
	      front_end#next fail_mem_valid_abs trans_false 
		fail_mem_valid_stmt.skind
	    in
	    let nexts_of_fail_mem_invalid_stmt =
	      front_end#next fail_mem_invalid_abs trans_mem_broken
		fail_mem_invalid_stmt.skind
	    in
	  
	    List.iter (self#add_to_not_visited_iterator current_node 
	      succs_mem_valid_stmt)  nexts_of_succs_mem_valid_stmt;
	    List.iter (self#add_to_not_visited_iterator current_node
	      succs_mem_invalid_stmt)  nexts_of_succs_mem_broken_stmt;
	    List.iter (self#add_to_not_visited_iterator current_node 
	      fail_mem_valid_stmt) nexts_of_fail_mem_valid_stmt;
	    List.iter (self#add_to_not_visited_iterator current_node
	      fail_mem_invalid_stmt) nexts_of_fail_mem_invalid_stmt 

	  end

	    

	| (Some(true_stmt),Some(false_stmt)) ->
	  begin
	     Format.fprintf Ast_goodies.debug_out "[If test, successors (Some(true_stmt),Some(false_stmt)) ]  \n %!";
	    let succs_mem_valid_stmt = true_stmt in
	    let succs_mem_valid_abs = 
	      front_end#copy_absdom_label current_node.abstract_val
	    in
	    let succs_mem_invalid_stmt = true_stmt in
	    let succs_mem_invalid_abs =  
	      front_end#copy_absdom_label current_node.abstract_val in
	    front_end#make_absdom_errorval succs_mem_invalid_abs;
	    let fail_mem_valid_stmt = false_stmt in
	    let fail_mem_valid_abs = 
	      front_end#copy_absdom_label current_node.abstract_val
	    in
	    let fail_mem_invalid_stmt = false_stmt in
	    let fail_mem_invalid_abs = 
	      front_end#copy_absdom_label current_node.abstract_val
	    in
	    front_end#make_absdom_errorval fail_mem_invalid_abs;
	  
	    let nexts_of_succs_mem_valid_stmt = 
		front_end#next succs_mem_valid_abs trans_true 
		  succs_mem_valid_stmt.skind
	    in
	    let nexts_of_succs_mem_broken_stmt =
	      front_end#next succs_mem_invalid_abs  
		trans_mem_broken succs_mem_invalid_stmt.skind
	    in
	    let nexts_of_fail_mem_valid_stmt =
	      front_end#next fail_mem_valid_abs trans_false 
		fail_mem_valid_stmt.skind
	    in
	    let nexts_of_fail_mem_invalid_stmt =
	      front_end#next fail_mem_invalid_abs trans_mem_broken
		fail_mem_invalid_stmt.skind
	    in
	  
	    List.iter (self#add_to_not_visited_iterator current_node 
	      succs_mem_valid_stmt)  nexts_of_succs_mem_valid_stmt;
	    List.iter (self#add_to_not_visited_iterator current_node
	      succs_mem_invalid_stmt)  nexts_of_succs_mem_broken_stmt;
	    List.iter (self#add_to_not_visited_iterator current_node 
	      fail_mem_valid_stmt) nexts_of_fail_mem_valid_stmt;
	    List.iter (self#add_to_not_visited_iterator current_node
	      fail_mem_invalid_stmt) nexts_of_fail_mem_invalid_stmt 

	
	  end
 
    

    method private build_ecfg () =
      if (not entry_point_set ) then raise Entry_point_not_registered
      else begin

      
      let succs_fc_sid_iterator (current_node : ecfg_vertex) 
	  (succ_sid : Cil_types.stmt ) =
	match current_node.statement.skind with
	   If(cdition,byes,bno,_) ->

	     Format.fprintf Ast_goodies.debug_out 
	       "[ECFG : In if/then/else ] Number of successor is :%d \n" (List.length current_node.statement.succs);
		begin
		  let sslv = front_end#copy_absdom_label 
		    current_node.abstract_val in
		  let (trans_true,trans_false,trans_mem_broken) = 
		  front_end#next_on_if_statement sslv cdition in
		  
		  let (true_stmt,false_stmt)  = 
		    
		    Ast_goodies.get_if_then_first_block_stmts 
		      current_node.statement byes bno
		  in
		  self#register_if_statement_successors
		    current_node
		    (trans_true,trans_false,trans_mem_broken)
		    (true_stmt,false_stmt)
		end
	   
	(* Gotos ought be dealt with here, if necessary.*)
	  

	  (*Switch statments also need to be dealt with at this
	  level.*)

	  | Switch(_,_,_,_) ->
	    let stmt_times_succs = 
	      self#next_on_switch_statement sslv current_node.statement in
	    self#register_switch_stmt_succs stmt_times_succs
	     
	  | _ ->
	      let abs_succ_list =     
		self#get_abstract_succs_of_ecfg_node current_node  succ_sid
	      in
		List.iter (self#add_to_not_visited_iterator current_node succ_sid ) 
		  abs_succ_list
		  
      in
      try
	while ( not (Queue.is_empty not_visited_vertices) )
	do
	  let current_node_id = Queue.pop not_visited_vertices in
	  self#mark_as_visited current_node_id;
	  let current_node = Hashtbl.find vertices current_node_id in
	  let framac_sid_successor_list = current_node.statement.succs in
	  List.iter (succs_fc_sid_iterator current_node ) 
	    framac_sid_successor_list  
	done
      with
	  Empty -> raise Empty
      end
	  
   

	
    method pprint_node ( node_id : ecfg_id ) =
      Format.sprintf "%d" (get_id_of_ecfg_id node_id)
	
	
    method private pprint_edge ( orig : ecfg_id ) ( dest : ecfg_id ) =
      let orig_tabl_out = Hashtbl.find edges orig in
      let label_trans = Hashtbl.find orig_tabl_out dest in
      let str_label = front_end#pretty_label label_trans in
      let str_label = "{"^str_label^"} \n" in
      let str_res = (self#pprint_node orig)^"->"^(self#pprint_node dest)^" "^str_label in
      str_res
	

	
    method pprint_transitions () =
      
      

      let dest_table_print_folder ( origin : ecfg_id ) (dest : ecfg_id ) label 
	  (prescript : string ) =
	 if (front_end#static_unsat_label label) 
	 then prescript 
	 else
	   if not (front_end#need_split_transition label) 
	   then
	     begin
	       let label = front_end#havocise_label label in
	       let post_script = Format.sprintf "%s \n s%d->s%d { %s }" prescript ( get_id_of_ecfg_id origin)  ( get_id_of_ecfg_id dest) 
		 (front_end#pretty_label label)
	       in 
	       post_script
	     end
	       
	   else
	     begin
	       let (pre,post) = front_end#split_guard_call_transition label in
	       let pre= front_end#havocise_label pre in
	       let post = front_end#havocise_label post in
	       let post_script = Format.sprintf "%s \n s%d->sinter%d { %s }" prescript   ( get_id_of_ecfg_id origin) ( intermediate_sid)
		 (front_end#pretty_label pre) in
	       let post_script=Format.sprintf "%s \n sinter%d->s%d { %s }" post_script ( intermediate_sid)  ( get_id_of_ecfg_id dest) 
		 (front_end#pretty_label post) in
	       intermediate_sid<-intermediate_sid + 1;
	       post_script
	     end 

      in
      let origin_table_print_folder (origin : ecfg_id ) table_dest 
	  (pre_script :  string ) =
	Hashtbl.fold (dest_table_print_folder origin) table_dest pre_script
      in
      let init_values_label = front_end#get_initialize_label() in
      let prescript =  Format.sprintf " sinit->s0 { %s }" (front_end#pretty_label init_values_label) in
      Hashtbl.fold origin_table_print_folder edges prescript

		
    method private pprint_inits () =
      "initial sinit ;"

    (*  let elem_left = ref 0 in
      let pprint_folder id () prescript =
	if (!elem_left) <= 1 then
 	  (prescript^(Printf.sprintf "s%d" ( get_id_of_ecfg_id  id )))
	else
 	  begin
	    elem_left  := (!elem_left) - 1;
 	    prescript^(Printf.sprintf "s%d," ( get_id_of_ecfg_id  id ))
	  end
      in 
       elem_left := (Hashtbl.length init_state);
      let retstring = Hashtbl.fold pprint_folder init_state ""
       in
      "initial "^retstring^";"*)
	

    method private pprint_finals () =
      let elem_left = ref 0 in
      let pprint_folder id () prescript =
	if !elem_left <= 1 then
	  prescript^(Format.sprintf "s%d" ( get_id_of_ecfg_id id ))
	else
	  begin
	    elem_left := !elem_left-1;
	    prescript^(Format.sprintf "s%d," ( get_id_of_ecfg_id id ))
	  end
      in
      elem_left := (Hashtbl.length final_state);
	if !elem_left >0 then
	  let retstring = Hashtbl.fold pprint_folder final_state ""
	  in
	    "final "^retstring^";"
	else
	  ""
	

    method private pprint_out_vars () =
      match fun_def.svar.vtype with
	  TFun(TInt(_,_),_,_,_) -> " out ret_val_, validity__ret_val__  : int;" 
	| TFun(TPtr(_,_),_,_,_) -> " out offset__ret_val__, validity__ret_val__ : int;"
	|  TFun(t,_,_,_) ->
	  begin
	    match (Composite_types.is_integer_type t) 
	    with 
		Some(_) -> " out ret_val_, validity__ret_val__ : int;"
	      |	None -> ""
	  end
	
      
      
    method private pprint_input_vars () =
       Nts.pprint_typeinfo_nts_var_list nts_sformal
	 
    method private pprint_local_vars () =
      let absval_of_ep = 
	front_end#get_entry_point_from_fundec finfo funinfo 
      in
     
      let unaffected_ret_vals = Nts.pprint_nts_var_list (Nts.make_ntsvars_of_intvars( Ast_goodies.name_of_non_assigned_ret_val  ( ) )) in
      let validvar_names = front_end#pprint_list_of_valid_locals_var 
	absval_of_ep funinfo in
      let validvar_names = 
	Nts.concat_comma_both_arg_non_empty unaffected_ret_vals
	validvar_names in
      Nts.concat_comma_both_arg_non_empty validvar_names 
	((Nts.pprint_typeinfo_int_nts_var_list nts_slocals))
	
      
    method private pprint_error_states () =
      let elem_left = ref 0 in
      let pprint_folder id () prescript =
	if !elem_left <= 1 then
	  prescript^(Format.sprintf "s%d" ( get_id_of_ecfg_id id ))
	else
	  begin
	    elem_left := !elem_left-1;
	    prescript^(Format.sprintf "s%d," ( get_id_of_ecfg_id id ))
	  end
      in
      elem_left := (Hashtbl.length error_state);
      let retstring = Hashtbl.fold pprint_folder error_state ""
      in
      if String.length retstring > 0 then 
      "error "^retstring^";"
      else ""

	
	
    method pprint_to_nts () = 
      (* let current_ecfg_node = Hashtbl.get vertex current_vertex_id in *)
      let res_string = name^"{\n" in
      let res_string = (
	if List.length nts_sformal > 0 then
	res_string^"in "^(self#pprint_input_vars ())^";\n"
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



    method pprint_ecfg_vertex () =
      let ecfg_vertex_folder id vertex str =
	match id  with
	    Ecfg_id(id_reg) ->
	      str^(Format.sprintf "Key_id %d, absval : %s \n" id_reg (front_end#pretty  vertex.abstract_val) )

      in 
      let prefix = Format.sprintf " \n States of : %s \n" name in
	Hashtbl.fold  ecfg_vertex_folder vertices prefix
	
  end;; (* End of the class ecfg*) 
end;; (* End of the module extended_cfg.ml*)

