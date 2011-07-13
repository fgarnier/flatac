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

open Format
open Buffer

(** 
  This module contains every structures and algorithms
  relatives to ecfg. It's parametrized by the type of the 
  abstract interpretation. 
  Obviously, this type must match with the front-end inherited
  type.
  *)
module Ecfg = 
  functor ( A : sig type abstract_type type label_type end ) ->
struct
  (** A.t represents the data type of the abstraction. *)
  type semantic_abstraction = A.abstract_type
  type counter_expression = A.label_type

  type semantic = Semantic of stmt * semantic_abstraction

  type ecfg_node_id = int
  type ecfg_edge = counter_expression
  type ecfg_node = Node of semantic * (ecfg_node_id, ecfg_edge) Hashtbl.t

  type ecfg = (ecfg_node_id, ecfg_node) Hashtbl.t

  (** This visitor handle global function and trigger the build 
    of a new Cfg for each function. *)
  class cfg_visitor ( prj : Project.t ) = object(self)
    inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
    val mutable is_computed = false
    val mutable _front_end : ( ( (semantic_abstraction, counter_expression)
                                   sem_and_logic_front_end) option ) = None

    val ecfgs : (string, ecfg) Hashtbl.t = Hashtbl.create 12

    val current_ecfg : ecfg = Hashtbl.create 97
    val visited_nodes : ((ecfg_node_id * semantic_abstraction), int) Hashtbl.t =
      Hashtbl.create 97
    val visited_sids : (ecfg_node_id, semantic_abstraction list)
        Hashtbl.t = Hashtbl.create 97

    val mutable nodeCount = 0
    val mutable transitionCount = 0

    method get_uid sid abstraction = 
      if Hashtbl.mem visited_nodes (sid, abstraction) 
      then Hashtbl.find visited_nodes (sid, abstraction)
      else
        begin
          nodeCount <- nodeCount + 1;
          Hashtbl.add visited_nodes (sid, abstraction) 
            ((Hashtbl.length visited_nodes) + 1);
          Hashtbl.length visited_nodes 
        end

    method add_visited_node sid abstraction = 
      let _ = self#get_uid sid abstraction in ()

    method is_accepted  (sid : ecfg_node_id) 
                        (abstraction : semantic_abstraction) 
                        (front_end : 
                           (semantic_abstraction, counter_expression) 
                           sem_and_logic_front_end)  =
      if not (Hashtbl.mem visited_sids sid)
      then begin Hashtbl.add visited_sids sid [abstraction]; true end 
      else begin 
        let visited_abstractions = Hashtbl.find visited_sids sid in
        let entailed = List.exists ( fun abs -> 
                               if abs = abstraction then true
                               else front_end#entails abs abstraction
          ) visited_abstractions in
          if entailed then begin
            Self.feedback ~level:0 "ENTAILED !";
            false
              end else begin
            Hashtbl.add visited_sids sid 
                 ( abstraction :: Hashtbl.find visited_sids sid ) ;
            true
          end
      end

    method _build_node_list ( statement : stmt ) abstraction 
             guardCounter front_end =
      if not (Hashtbl.mem visited_nodes (statement.sid, abstraction)) then
        begin
          self#add_visited_node statement.sid abstraction;
          let subEdges = Hashtbl.create 12 in
          let _ = List.map ( fun succ -> 
                               let abstractions_and_labels = 
                                 front_end#next abstraction
                                   guardCounter succ.skind in
                                 List.map ( fun (succ_abs, succ_lbl) ->
                                  if self#is_accepted statement.sid abstraction front_end then
                                              let edgeUID = 
                                                self#_build_node_list succ 
                                                  succ_abs succ_lbl front_end in
                                                Hashtbl.add subEdges edgeUID
                                                  succ_lbl
                                 ) abstractions_and_labels;
          ) statement.succs in
          let currentUID = self#get_uid statement.sid abstraction in
            Hashtbl.add current_ecfg currentUID 
              (Node ( Semantic ( statement, abstraction ), subEdges));
            currentUID
        end
      else (self#get_uid statement.sid abstraction)

    method build_node_list ( funInfo : Cil_types.fundec ) front_end =
      Hashtbl.clear current_ecfg;
      let rootStmt = (List.hd funInfo.sallstmts) in
      let _ = self#_build_node_list rootStmt
                (front_end#get_entry_point_abstraction ())
                (front_end#get_empty_transition_label ()) front_end in
        Hashtbl.copy current_ecfg

    method vglob_aux (g :Cil_types.global ) =
      is_computed <- true;
      match (g, _front_end) with 
        | ( GFun ( funInfo, _ ), Some ( front_end ) ) -> 
            Hashtbl.add ecfgs funInfo.svar.vname 
              (self#build_node_list funInfo front_end); 
            DoChildren
        | _ -> DoChildren

    method set_front_end front_end = _front_end <- Some ( front_end ) 
    method get_ecfgs = ecfgs
    method get_node_count = nodeCount
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
  let visite_ecfgs (ecfgs : (string, ecfg) Hashtbl.t) 
        pre_callback post_callback callback =
    Hashtbl.iter( fun fname func_ecfg ->
                    (* Self.feedback ~level:0 "Export of %s (%d nodes)" fname
                     * (Hashtbl.length func_ecfg) ; *)
                    pre_callback fname;
                    Hashtbl.iter ( callback fname ) func_ecfg;
                    post_callback fname;
    ) ecfgs

  let stmt_to_string stmt =
    Buffer.reset stdbuf;
    match stmt.skind with
      | Instr ( Set ( (Var ( lvalueInfo ), _) , expression, _ ) ) ->
          let vname = lvalueInfo.vname in
            add_string stdbuf vname; add_string stdbuf " = ";
            Cil.printExp Cil.defaultCilPrinter str_formatter expression; 
            String.escaped (flush_str_formatter ())
      | If ( expression, _, _, _) -> 
          add_string stdbuf " IF ";
          Cil.printExp Cil.defaultCilPrinter str_formatter expression; 
          String.escaped (flush_str_formatter ())
      | Loop ( _, _, _, _, _ ) ->
          add_string stdbuf " WHILE ( 1 )";
          String.escaped (flush_str_formatter ())
      | Return ( Some ( expression ), _ ) ->
          add_string stdbuf " RETURN ";
          Cil.printExp Cil.defaultCilPrinter str_formatter expression; 
          String.escaped (flush_str_formatter ())
      | Return ( None, _ ) ->
          add_string stdbuf " RETURN ;";
          String.escaped (flush_str_formatter ())
      | UnspecifiedSequence _ ->
          add_string stdbuf " UNSPECIFIED :  ";
          Cil.printStmt Cil.defaultCilPrinter str_formatter stmt; 
          String.escaped (flush_str_formatter ())
      | Block (_) ->
          add_string stdbuf " BLOCK ";
          String.escaped (flush_str_formatter ())
      | _ -> 
          Cil.printStmt Cil.defaultCilPrinter str_formatter stmt; 
          String.escaped (flush_str_formatter ())

  let print_dot foc front_end _ uid node =
    match node with
      | Node (Semantic ( statement, abstraction ), listOfEdges) -> 
          Format.fprintf foc 
            "\t\t%d [label=\"%d/%d\\nCode : %s\\nAbstraction : %s\"]\n"
            uid uid statement.sid (stmt_to_string statement) 
            (front_end#pretty abstraction);
          Hashtbl.iter ( fun toUid counterValue  -> 
                           Format.fprintf foc 
                             "\t\t%d -> %d [label=\"%s\"]\n\n" uid toUid
                             (front_end#pretty_label counterValue)
          ) listOfEdges

  (** This function export the given ecfg in dot format into the given file.
    If the file does not exist, it is created. It is overwritten otherwise. *)
  let export_dot ecfgs filename front_end= 
    let oc = open_out filename in
    let foc = formatter_of_out_channel( oc ) in
      Format.fprintf foc "digraph G {\n";
      visite_ecfgs ecfgs 
        ( fun fname -> Format.fprintf foc 
                         "\tsubgraph cluster_%s {\n\t\tnode \
                         [style=filled,color=white]; \n\t\tstyle=filled; \
                          \n\t\tcolor=lightgray; \n\t\tlabel = \"%s\"; \
                          \n\t\tfontsize=40; \n\n" fname fname )
        ( fun _ -> Format.fprintf foc "\t}\n" ) 
        (print_dot foc front_end);
      Format.fprintf foc "\n}";
      Self.feedback ~level:0 "Graph exported!"
end;;
