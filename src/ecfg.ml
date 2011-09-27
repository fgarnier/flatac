(**
  This module contains the definition of an ecfg and implements a generic 
  algorithm to fill it with the correct Abstract Interpretation and counter 
  automata label.

  Maxime Gaudin - VERIMAG 2011 
  ** THIS MODULE IS A PART OF FLATA-C, DEVELOPED AT VERIMAG (2011)
  For any question mail us to  maxime.gaudin [AT] imag [DOT] fr or 
  florent.garnier [AT] imag [DOT] fr
  *)
open Self
open Cil
open Cil_types
open Cfg
open Visitor
open Sem_and_logic_front_end

open Ecfg_types
(** 
  This module contains every structures and algorithms
  relatives to ecfg. It's parametrized by the type of the 
  abstract interpretation.  Obviously, this type must match 
  with the front-end inherited type.
  *)
module Ecfg = 
  functor ( A : sig type abstract_type type label_type end ) ->
struct
  module P_ecfg_types = Ecfg_types ( A ) 
  open P_ecfg_types

  class cfg_visitor ( prj : Project.t ) = object(self)
    inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
    (** Frama-C related ** TO CHECK IF USED ANYWHERE *)
    val mutable is_computed = false
   
    val mutable _front_end : ( ( (semantic_abstraction, counter_expression)
                                   sem_and_logic_front_end) option ) = None

    (** This hash table contains the already built eCFGs *)
    val ecfgs : (string, ecfg) Hashtbl.t = Hashtbl.create 97

    (** This one contains every node associeted to the eCFG in construction.
    * After it's construction, it will be added to the previously defined 
    * ecfgs hash table.*)
    val current_ecfg : ecfg = Hashtbl.create 97

    (** For speed purpose, we had to duplicate datas along two structures. 
    * The first one contains the computed uid (if it was computed yet),
    * the second one contains every abstractions associated to sids. *)
    val visited_nodes : ((ecfg_node_id * semantic_abstraction), int) Hashtbl.t =
      Hashtbl.create 97
    val visited_sids : (ecfg_node_id, semantic_abstraction list)
        Hashtbl.t = Hashtbl.create 97

    val mutable node_count = 0
    val mutable transition_count = 0

    (** This methods return a uid based on the current node's
      * sid and abstraction.
      * Warning, if the node was not visited_yet, it's 
      * added to the visited node list and therefore, will
      * be considered as visited.
      *)
    method get_uid sid abstraction = 
      if Hashtbl.mem visited_nodes (sid, abstraction) 
        then Hashtbl.find visited_nodes (sid, abstraction)
        else
        begin
          node_count <- node_count + 1; 
          Hashtbl.add visited_nodes (sid, abstraction) node_count;
          node_count
        end

    (** Tool method to encapsulate the get_uid behavior 
    * ( uid + add node to visited node list ) *)
    method add_visited_node sid abstraction = 
      let _ = self#get_uid sid abstraction in ()

    (** This method returns true if the node is accepted, false otherwise.
      * The condition for acceptation is documented bellow. *)
    method is_accepted  
     (sid : ecfg_node_id) 
     (abstraction : semantic_abstraction) 
     (front_end : (semantic_abstraction, counter_expression) 
        sem_and_logic_front_end)  =
      (** If the node's sid was unseen yet, accept it *)
      if not (Hashtbl.mem visited_sids sid)
      then begin 
        Hashtbl.add visited_sids sid [abstraction]; 
        true 
      end
      else 
      begin 
        let visited_abstractions = Hashtbl.find visited_sids sid in
          (** Is there any node such as for this sid, it's abstraction is
          * the current one, or is more general (i.e. entails) the current one
          *)
          if List.exists ( fun abs -> 
                             (** Already accepted *)
                             if abs = abstraction then true
                             else 
                               (** Entails ? *)
                               try not(front_end#accepts abs abstraction)
                               with e -> self#handle_exception e; true
          ) visited_abstractions then false 
          else 
          begin
            Hashtbl.add visited_sids sid 
              ( abstraction :: (Hashtbl.find visited_sids sid) ) ;
            true
          end
      end

    (** The main "loop" of this module, this function builds the current eCFG
      * starting from the root, and browsing every accepted successors,
      * recursively *)
    method _build_node_list 
      ( statement : stmt ) abstraction 
      guardCounter 
      front_end =
        if not (Hashtbl.mem visited_nodes (statement.sid, abstraction)) then
        begin
          self#add_visited_node statement.sid abstraction;
          let subEdges = Hashtbl.create 12 in
          let _ = List.map 
            ( fun succ -> 
            try
             let abstractions_and_labels = 
              front_end#next abstraction guardCounter succ.skind in

             List.map 
              ( fun (succ_abs, succ_lbl) ->
                if self#is_accepted succ.sid succ_abs front_end then
                  let edgeUID = 
                  self#_build_node_list succ succ_abs succ_lbl front_end in
                    Hashtbl.add subEdges edgeUID succ_lbl
                else
                  List.iter 
                    ( fun entailed_abs ->
                      let edgeUID = (self#get_uid succ.sid entailed_abs) in
                        Hashtbl.add subEdges edgeUID succ_lbl
                     ) (Hashtbl.find visited_sids succ.sid) 
               ) abstractions_and_labels;

            with e -> self#handle_exception e; []
            ) statement.succs in

            let currentUID = self#get_uid statement.sid abstraction in
              Hashtbl.add current_ecfg currentUID 
                (Node ( Semantic ( statement, abstraction ), subEdges));
                currentUID
        end
        else (self#get_uid statement.sid abstraction)

    (** This method is only the accessor to _build_node_list *)
    method build_node_list ( funInfo : Cil_types.fundec ) front_end =
      Hashtbl.clear current_ecfg;
      prepareCFG funInfo; computeCFGInfo funInfo true;
      let rootStmt = (List.hd funInfo.sallstmts) in
      let _ = self#_build_node_list rootStmt
                (front_end#get_entry_point_abstraction ())
                (front_end#get_empty_transition_label ()) front_end in
        Hashtbl.copy current_ecfg

    method vglob_aux (g :Cil_types.global ) =
      match (g, _front_end) with 
        | ( GFun ( funInfo, _ ), Some ( front_end ) ) -> 
            Hashtbl.add ecfgs funInfo.svar.vname 
              (self#build_node_list funInfo front_end); 
            DoChildren
        | _ -> DoChildren

    method handle_exception e =
      match e with 
        | Flatac_exception (_, 1, message) -> Self.warning "%s" message; () 
        | Flatac_exception (_, 0, message) -> Self.fatal "%s" message; ()
        | _ -> raise e; ()

    (* Some getters & setters *)
    method set_front_end front_end = _front_end <- Some ( front_end ) 
    method get_ecfgs = ecfgs
    method get_node_count = node_count
  end

  (** Compute the ecfg and fill the structures. *)
  let compute_ecfgs ( prj : Project.t ) ( ast : Cil_types.file ) 
        ( front_end : (semantic_abstraction, counter_expression)
            sem_and_logic_front_end ) = 
    let cfg_visitorInst = new cfg_visitor ( prj ) in	
      cfg_visitorInst#set_front_end front_end; 
      visitFramacFile ( cfg_visitorInst :> frama_c_copy ) ast;
      cfg_visitorInst#get_ecfgs 

  (** An ecfg visitor with a callback function. You must use this method if you 
    want to gather ecfg datas. *)
  let visit (ecfgs : (string, ecfg) Hashtbl.t) 
        pre_callback post_callback callback =
    Hashtbl.iter( fun fname func_ecfg ->
                    pre_callback fname;
                    Hashtbl.iter ( callback fname ) func_ecfg;
                    post_callback fname;
    ) ecfgs

end;;
