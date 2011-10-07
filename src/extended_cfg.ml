(*
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



module Extended_cfg_definition = 
  functor ( A : sig type abstract_type type label_type end ) ->
struct
  module Extended_cfg_base_types = Extended_cfg_types ( A ) 
  open Extended_cfg_base_types
  
  val init_hashtbl_size = 97
    
  class extended_cfg ( prj : Project.t ) = object(self)
    inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
    (** Frama-C related ** TO CHECK IF USED ANYWHERE *)
    val mutable is_computed = false
      
    val mutable front_end : ( ( (Extended_cfg_base_types.abs_dom_type, 
				  Extended_cfg_base_types.label_type)
                                   sem_and_logic_front_end) option ) = None

    val edge : ( int , (int , trans_label_val ) Hashtbl.t ) Hashtbl.t = 
      Hashtbl.create init_hashtbl_size
    val edge_inv : (int , int ) Hashtbl.t = Hashtbl.create init_hashtbl_size
    val vertices : (int , ecfg_vertex) Hashtblt = Hashtbl.create init_hashtbl_size

    (* The key corresponds to the Cil_type.stmt.sid and the corresponding
    hash table contains all the id of the note of the ecfg which have
    the same sid as the key.*)
    val unfoldsid_2_abs_map : (int , (int , ()) Hashtbl.t) Hashtbl.t 

    val mutable current_node_id = 0 (* Basically, the total number of
				    nodes, as well as the id of the next
				    to be created node, if any.*)



    (** Adds a vertex to the ecfg*)
    method private add_abstract_state ( s : cil_types.stmt ) ( absval : abs_domain_type ) =
      let new_vertex = {
	id = current_node_id;
	statement = s;
	abstract_val = absval ; 
	
      } 
      in
	Hashtbl.add vertices vertices current_node_id new_vertex; 
	current_node_id <- (current_node_id + 1);
	new_vertex.id (**  Returns the id of the created vertex*)

	  
    (** Adds a labelled edge between two vertexed of the
	extended control flow graph.*)
    method private register_edge (origin : int )( dest : int  )
      (label : label_type ) =
      try
	let entry_tab = Hashtbl.find edges origin in
	  Hashtbl.add entry_tabl dest label;
	  let reverse_table = Hashtbl.find edge_inv post in
	    Hashtbl.add reverse_table pre ();
	    (* store that post has pre as predecessor, obvious isn't it ?*)
	  
      with
	  Not_found -> raise Not_found



	    
    (** This method checks whether some abstract state 
       (sid , absdomvalue) is not entailed by another
	abstract state (sid, abv), i.e. abv |- absdomvalue.
	If no abs value is associated to this sid, then
	the method answers ( false , -1 ).
	Otherwise, it returns a tuple ( true ,  sid'), where
        abs' |- absdomvalue .
    *)
    method entailed_by_same_id_absvalue  (next_stmt : Cil_types.stmt)
      ( absval : abs_domain_type ) =
      let entail_folder (id_abs_brothers : int ) () (already_found : int ) =
	if already_found > 0 then already_found
	else
	  let brother_ecfg_node = Hashtbl.find vertices id_abs_brother in
	  let brother_abs = brother_ecfg_node.abstract_val in
	  if ( front_end#accept brother_abs absval ) then id_abs_brother
	  else 
	    already_found
      in
      try
	let brotherhood_abs = Hashtbl.find next_stmt.sid in
	let id_candidate = Hashtbl.fold entail_folder brotherhood_abs -1 in
	  if id_candidate > 0 then
	    (true , id_candidate )
	  else
	    (false , -1 )
      with
	  Not_found -> (false , -1 ) 
  


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
      (next_stmt : Cil_types.stmt ) (next_abs : abs_domain_type ) 
      ( label : label_type) =
      try
	let dest_sid_abs_table = Hashtbl.find unfoldsid_2_abs_map dest_sid 
	in
	let is_new_abstraction =  self#entailed_by_same_id_absvalue 
	  next_stmt next_abs in
	  begin
	    match is_new_abstraction with
		(false , _ ) -> 
		  begin
		  let new_abs_state_id = 
		    self#add_abstract_state next_stmt next_abs in
		    self#register_edge current.id new_abs_state_id label
		  end
	      | (true , entailing_state_id ) ->
		  self#register_edge current.id entailin_state_id label
	  end
      with
	  Not_found -> raise Not_found (* TODO Write an exception treatment
				       that suits ...*)
	    

 (* Pre and post reprensent the identificators of the abstract states,
 i.e. states in the sid * abs_dom_val cross product.*)
    method add_edge_by_id (pre : int) (post : int ) (label : trans_label_val) =
      try
	let entry_table = Hashtbl.find edge pre in
	  Hashtbl.add entry_table post label;
	  let reverse_table = Hashtbl.find edge_inv post in
	    Hashtbl.add reverse_table pre;
	    (* store that post has pre as predecessor, obvious isn't it ?*)
	    
      with
	  Not_found -> raise Not_found  (* Put that here to note that
					there exists a smartes way to 
					deal with this kind of exception
					*)


    method private recurive_build_ecfg (current_node : ecfg_vertex) (stmt : Cil_types.stmt)( abst : label_type ) =
      
      
    method build_fun_ecfg ( funinfo : Cil_types.fundec ) =
      prepareCFG funinfo; computeCFGInfo funinfo true;
      let rootstmt = List.hd funinfo.sallstmts in
      let root_abstraction = get_entry_point_abstraction () in
	self#add_abstract_state rootstmt root_abstraction;
	recursive_build_ecfg 
	

front_end#make_entry_node_from_cil_stmt rootstmt
      in
      
    
      
    method build_node_list ( funInfo : Cil_types.fundec ) front_end =
      Hashtbl.clear current_ecfg;
      prepareCFG funInfo; computeCFGInfo funInfo true;
      let rootStmt = (List.hd funInfo.sallstmts) in
      let _ = self#_build_node_list rootStmt
                (front_end#get_entry_point_abstraction ())
                (front_end#get_empty_transition_label ()) front_end in
        Hashtbl.copy current_ecfg

    method vglob_aux (g : Cil_types.global ) =
      match (g, _front_end) with 
        | ( GFun ( funInfo, _ ), Some ( front_end ) ) -> 
            Hashtbl.add ecfgs funInfo.svar.vname 
              (self#build_node_list funInfo front_end); 
            DoChildren
        | _ -> DoChildren  

      
      
      
  end;;


