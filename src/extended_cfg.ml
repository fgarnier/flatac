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


    method add_abstract_state ( s : cil_types.stmt ) ( absval : abs_domain_type ) =
      let new_vertex = {
	id = current_node_id;
	statement = s;
	abstract_val = absval ; 
	
      } 
      in
	Hashtbl.add vertices vertices current_node_id new_vertex; 
	current_node_id <- (current_node_id + 1);
	new_vertex.id (**  Returns the id of the created vertex*)



    method add_edge (pre : int) (post : int ) (label : trans_label_val) =
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


