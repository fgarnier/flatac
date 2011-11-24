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

open Extended_cfg_types


exception Entry_point_already_registered 
exception Marking_unregistered_vertex of ecfg_id
exception Ecfg_vertex_not_registered
exception No_outgoing_edges_from_state of ecfg_id
exception No_such_state_id
exception Building_an_edge_between_inexisting_node_ids of ecfg_id
exception Debug_exception of string

let get_id_of_ecfg_id ( id : ecfg_id) =
  match id with
      Ecfg_id(i) -> i


let make_empty_cil_statement =
  {
    labels = [] ;
    skind = UnspecifiedSequence ([]) ;
    sid = -1 ;
    succs = [] ;
    preds = [] ;
    ghost = true ;
  }
  

module Extended_cfg_definition  = 
  functor ( A : sig type abstract_type type label_type end ) ->
struct
  module Extended_cfg_base_types = Extended_cfg_types ( A ) 
  open Extended_cfg_base_types
  
  
  

  
  class extended_cfg (name_function : string ) (funinfo : Cil_types.fundec) 
    frontend   = object(self)

    val mutable name = name_function 
    val mutable is_computed = false
    val mutable entry_point_set = false (* Initial control state of the 
					ecfg set ?*)

      
    val mutable front_end :  ( (Extended_cfg_base_types.abs_dom_val, 
				  Extended_cfg_base_types.trans_label_val)
                                 sem_and_logic_front_end ) = frontend 

    
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
    val visited_index : ( sid_class ,  (ecfg_id ,  unit ) Hashtbl.t ) Hashtbl.t =
      Hashtbl.create init_hashtbl_size

    (* The key corresponds to the Cil_type.stmt.sid and the corresponding
       hash table contains all the id of the note of the ecfg which have
       the same sid as the key.*)
	
    val unfoldsid_2_abs_map : (sid_class , (ecfg_id , unit ) Hashtbl.t) Hashtbl.t =
      Hashtbl.create init_hashtbl_size

    (* Associates to each
       abstract id the correspond
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
      
    initializer self#build_fun_ecfg funinfo

    method private incr_current_node_id =
      match current_node_id with
	  Ecfg_id(i) -> 
	    let nid = i+1 in
	    (current_node_id <- Ecfg_id(nid))

	      
   (* Sets a state as being initial.*)
    method private register_init_state ( state_id : ecfg_id ) = 
      if Hashtbl.mem vertices state_id
      then 
	Hashtbl.add init_state state_id ()
      else
	raise No_such_state_id

    
    method private add_ecfg_entry_point  ( s : Cil_types.stmt ) 
      ( absval : abs_dom_val ) =     
       let new_vertex = {
	id = current_node_id;
	statement = s;
	abstract_val = absval ; 	
      }
       in
       Hashtbl.add vertices current_node_id new_vertex;
       let entry_table = Hashtbl.create init_hashtbl_size in
       Hashtbl.add entry_table current_node_id ();
       Hashtbl.add unfoldsid_2_abs_map (Sid_class(s.sid)) entry_table;
       Hashtbl.add fold_abs_map_2_sid new_vertex.id (Sid_class(s.sid));
       self#incr_current_node_id;
       Hashtbl.add init_state new_vertex.id ();
       new_vertex.id
       
       
       
      
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
	self#incr_current_node_id;
	(*current_node_id <- (current_node_id + 1);*)
	
	(* Error states shall be considered as an absorbing class.
	 *)
	begin
	  match new_vertex.id with
	      Ecfg_id(id) ->
		Format.printf "[ Extended cfg Adding : Ecfg node id  : %d, Framac sid %d ] \n " new_vertex.statement.sid id
	end;
	if front_end#is_error_state absval then
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
      try
	if not (Hashtbl.mem edges origin) then
	  begin
	    let tabl_for_origin = Hashtbl.create init_hashtbl_size in
	    Hashtbl.add edges origin tabl_for_origin
	  end;
	
	let entry_tab = Hashtbl.find edges origin in
	  Hashtbl.add entry_tab dest label;
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
	  let statment_of_ep= make_empty_cil_statement in
	  let absval_of_ep = front_end#get_entry_point_from_fundec funinfo in
	  let id_ep = self#add_ecfg_entry_point statment_of_ep 
	    absval_of_ep in
	   entry_point_set <- true;
	    id_ep
	   (* returns the id of the node, shall be 0*) 
	      
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
	let candidate = (Hashtbl.fold entail_folder brotherhood_abs (false , Ecfg_id( - 1 ))) in
	candidate
      (*match candidate with 
	    (true, _ ) -> candidate
	  if id_candidate >= 0 then
	    (true , id_candidate )
	  else
	    (false , -1 )
	*)
      with
	  Not_found -> (false ,Ecfg_id(-1)) 
	    

    method is_sid_visited ( sid : sid_class ) =
      Hashtbl.mem visited_index sid

    method is_ecfg_vertex_id_visited ( id  : ecfg_id ) =
      let sid_of_id = Hashtbl.find fold_abs_map_2_sid id in
      try
	let visit_tbl_of_sid = Hashtbl.find visited_index sid_of_id in
	Hashtbl.mem visit_tbl_of_sid id
      with
	| Not_found -> false (* No ecfg node whose sid equals sid_of_id has
			     yet been visited*)


(** Returns true if the s * abs has not yet been visited for building the ecfg.*)
    method recurse_to_abs_succs ( s : Cil_types.stmt ) ( abs : abs_dom_val ) =
      let sid_of_stmt = Sid_class( s.sid ) in
      if (not (Hashtbl.mem visited_index sid_of_stmt ) )
      then true 
      else
	let ( is_entailed , _ ) = self#entailed_by_same_id_absvalue s abs in
	  is_entailed
	  

    method mark_as_error_state (vertex_id : ecfg_id ) =
      if not (Hashtbl.mem error_state vertex_id)
      then Hashtbl.add error_state vertex_id ()
      else ()
      

    method mark_as_visited ( vertex_id : ecfg_id ) =      
      let v = Hashtbl.find vertices vertex_id in
      try    
	if Hashtbl.mem visited_index (Sid_class(v.statement.sid)) then 
	  begin
	    let sid_table = Hashtbl.find visited_index (Sid_class(v.statement.sid)) in
	    if Hashtbl.mem sid_table vertex_id then ()
	    else Hashtbl.add sid_table vertex_id ()
	  end
	else
	  let new_visited_tab = Hashtbl.create init_hashtbl_size in
	  Hashtbl.add new_visited_tab vertex_id ();
	  Hashtbl.add visited_index (Sid_class(v.statement.sid)) new_visited_tab
	  
      with
	  Not_found -> let excep = Marking_unregistered_vertex ( vertex_id ) in
		       raise excep

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
		    Format.printf "Adding new node to ecfg \n";
		  let new_abs_state_id = 
		    self#add_abstract_state next_stmt next_abs in
		    self#register_edge current.id new_abs_state_id label
		  end
	      | (true , entailing_state_id ) ->
		 Format.printf "Not adding a new node, creating a new edge\n";
		  self#register_edge current.id entailing_state_id label
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
	    




    (* This function is used to recursivey call recusive_build_ecfg 
       on all the nodes that are registered as successor of  the parameter
       current_node. *)	    
    method private recursive_build_ecfg ( current_node : ecfg_vertex ) =
     
      (*let current_sid = current_node.statement.sid in*)
      let ecfg_succ_recursor  (index : ecfg_id ) _ =
	Format.printf "recursor : successor id is %d \n" 
	  ( get_id_of_ecfg_id index);
	
	let next_ecfg_vertex = Hashtbl.find vertices index in
	if( self#is_ecfg_vertex_id_visited index)
	then 
	  begin
	    Format.printf "Ecfg vertex %d already visided \n" 
	      (get_id_of_ecfg_id index)
	  end

	else if ( self#recurse_to_abs_succs next_ecfg_vertex.statement
		    next_ecfg_vertex.abstract_val ) 
	then
	  begin
	    Format.printf "Ecfg vertex %d not yet marked as visited \n" 
	      ( get_id_of_ecfg_id index);
	    self#recursive_build_ecfg next_ecfg_vertex
	  end
	else ()
      in
      let next_list_adder_iterator (next_stmt : Cil_types.stmt) 
	  (e : (abs_dom_val * trans_label_val ) ) =
	match e with
	    ( absvalue , labval ) ->
	      begin
		self#add_transition_from_to current_node next_stmt absvalue
		  labval  
	      end
      (* End of  next_list_adder_iterator *)

      in
      let build_iterator ( succs_stmt : Cil_types.stmt ) =
	let current_absvalue = front_end#copy_absdom_label 
	  current_node.abstract_val
	in
	let empty_label = front_end#get_empty_transition_label () in
	let succs_list = front_end#next current_absvalue empty_label 
	  succs_stmt.skind 
	in
	List.iter (next_list_adder_iterator succs_stmt) succs_list
      (* End of the build_iterator definition *)
      in 
      self#mark_as_visited current_node.id;
      if (front_end#is_error_state current_node.abstract_val) 
      then  (* One stops the recursive call on the set of
	       states whose abstract domain value is an erro
	       state, i.e. memory leak or broken heap *)
	self#mark_as_error_state current_node.id
      else
	begin 
	  try
	    let current_statment_successors = current_node.statement.succs 
	    in
	    List.iter build_iterator current_statment_successors;
		(* We get the set of the current vertex successor and
		   we iterate on each of them*)
	    let ecfg_succs_indexes = self#get_succs_of_ecfg_node current_node 
	    in 
	    Hashtbl.iter ecfg_succ_recursor ecfg_succs_indexes
	      
	(* The recursive call is performed in the iterator*)
	  with
	     
  	    |  No_outgoing_edges_from_state ( node_id ) ->
	      begin
		if not (Hashtbl.mem final_state  node_id  ) 
		then
		  Hashtbl.add final_state node_id () 
	      (* The current node ahs no successor
		 in the ecfg*)
    		else ()
	      end
	end
	  
	  
    method build_fun_ecfg ( funinfo : Cil_types.fundec ) =
      
      let ep_succs_iterator (entry_point :  ecfg_vertex ) (next_stmt : Cil_types.stmt) 
	   (e : (abs_dom_val * trans_label_val ) ) =
	match e with
	    ( absvalue , labval ) ->
	      begin
		self#add_transition_from_to entry_point next_stmt absvalue
		  labval  
	      end
      in
	Cfg.cfgFun funinfo;
	let empty_label = front_end#get_empty_transition_label () in
	let ecfg_entry_point_id = self#register_ecfg_entry_point funinfo in 
	let ecfg_entry_point = Hashtbl.find vertices ecfg_entry_point_id in
	let cil_rootstmt = List.hd funinfo.sallstmts in
	let succs_of_ecfg_entry_point = front_end#next 
	  (ecfg_entry_point.abstract_val) empty_label cil_rootstmt.skind
	in
	  List.iter ( ep_succs_iterator ecfg_entry_point  cil_rootstmt )  
	    succs_of_ecfg_entry_point;
	  self#recursive_build_ecfg  
	    (Hashtbl.find vertices ecfg_entry_point_id)
          
	
    method pprint_node ( node_id : ecfg_id ) =
      Format.sprintf "%d" (get_id_of_ecfg_id node_id)
	
	
    method private pprint_edge ( orig : ecfg_id ) ( dest : ecfg_id ) =
      let orig_tabl_out = Hashtbl.find edges orig in
      let label_trans = Hashtbl.find orig_tabl_out dest in
      let str_label = front_end#pretty_label label_trans in
      let str_label = "{"^str_label^"} \n" in
      let str_res = (self#pprint_node orig)^"->"^(self#pprint_node dest)^" "^str_label in
      str_res
	
	
    method private pprint_to_nts_rec (current_vertex_id : ecfg_id )(printed_index : (ecfg_id , unit ) Hashtbl.t ) (pre_print : string ) =
      let transitions_folder (id : ecfg_id ) _ (previous_trans : string ) =
	let previous_trans = 
	  previous_trans^(self#pprint_edge current_vertex_id id) 
	in
	previous_trans
      in
      let  recurse_folder (succs_id : ecfg_id ) _ ( nts_script : string ) =
	if Hashtbl.mem vertices succs_id then  nts_script
	else self#pprint_to_nts_rec succs_id printed_index nts_script 
      in
      Hashtbl.add printed_index current_vertex_id (); (* Marks the
							 current vertex as 
							 visited
						      *)
      let succ_id = Hashtbl.find edges current_vertex_id in
      let trans_from_current = Hashtbl.fold transitions_folder succ_id "" in
      let ret_succs  = pre_print^trans_from_current in
      Hashtbl.fold recurse_folder succ_id ret_succs
	
	
    method pprint_transitions =
      let dest_table_print_folder ( origin : ecfg_id ) (dest : ecfg_id ) label 
	  (prescript : string ) =
	let post_script = Format.sprintf "%s \n %d -> %d { %s } \n" prescript ( get_id_of_ecfg_id origin)  ( get_id_of_ecfg_id dest) 
	  (front_end#pretty_label label)
	in 
	post_script 
      in
      let origin_table_print_folder (origin : ecfg_id ) table_dest 
	  (pre_script :  string ) =
	Hashtbl.fold (dest_table_print_folder origin) table_dest pre_script
      in
      Hashtbl.fold origin_table_print_folder edges ""

		
	
    method private pprint_inits  =
      let elem_left = ref 0 in
      let pprint_folder id () prescript =
	if (!elem_left) <= 1 then
	  (prescript^(Printf.sprintf "%d" ( get_id_of_ecfg_id  id )))
	else
	  begin
	    elem_left  := (!elem_left) - 1;
	    prescript^(Printf.sprintf "%d," ( get_id_of_ecfg_id  id ))
	  end
      in 
      elem_left := (Hashtbl.length init_state);
      let retstring = Hashtbl.fold pprint_folder init_state ""
      in
      "init: "^retstring
	
	
	
    method private pprint_finals =
      let elem_left = ref 0 in
      let pprint_folder id () prescript =
	if !elem_left <= 1 then
	  prescript^(Format.sprintf "%d" ( get_id_of_ecfg_id id ))
	else
	  begin
	    elem_left := !elem_left-1;
	    prescript^(Format.sprintf "%d," ( get_id_of_ecfg_id id ))
	  end
      in
      elem_left := (Hashtbl.length final_state);
      let retstring = Hashtbl.fold pprint_folder final_state ""
      in
      "final: "^retstring
	
	
    method private pprint_error_states  =
      let elem_left = ref 0 in
      let pprint_folder id () prescript =
	if !elem_left <= 1 then
	  prescript^(Format.sprintf "%d" ( get_id_of_ecfg_id id ))
	else
	  begin
	    elem_left := !elem_left-1;
	    prescript^(Format.sprintf "%d," ( get_id_of_ecfg_id id ))
	  end
      in
      elem_left := (Hashtbl.length error_state);
      let retstring = Hashtbl.fold pprint_folder error_state ""
      in
      "error: "^retstring

	
	
    method pprint_to_nts  = 
      (* let current_ecfg_node = Hashtbl.get vertex current_vertex_id in *)
      let res_string = Format.sprintf "nts %s; \n" name in
      let res_string = res_string^name^" {\n" in
      let res_string = res_string^(self#pprint_inits)^"\n"  in
      let res_string = res_string^(self#pprint_finals)^"\n" in
      let res_string = res_string^(self#pprint_error_states)^"\n" in
      let res_string = res_string^(self#pprint_transitions)
      in
      let res_string = res_string^"\n}" in
      res_string
	
  end;; (* End of the class ecfg*)
end;; (* End of the module extended_cfg.ml*)

