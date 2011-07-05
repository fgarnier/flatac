(**
	This module contains the definition of an ecfg and implements a generic algorithm to
	fill it with the correct Abstract Interpretation and counter automata label.

	Maxime Gaudin - VERIMAG 2011 
	** THIS MODULE IS A PART OF FLATA-C, DEVELOPED AT VERIMAG (2011)
	For any question mail us to  maxime.gaudin [AT] imag [DOT] fr or florent.garnier [AT] imag [DOT] fr
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
module Ecfg = functor ( A : sig type t end ) ->
struct
  (** A.t represents the data type of the abstraction. *)
  type semantic_abstraction = A.t
  type semantic = Semantic of stmt * semantic_abstraction
      
  type ecfg_edge = Edge of int * counter_expression
  type ecfg_node = Node of int * semantic * (ecfg_edge list)
  type ecfg = 
    | CGraph of string * (ecfg_node list)
    | EmptyGraph
	    
	(** This visitor handle global function and trigger the build 
	    of a new Cfg for each function. *)
  class cfg_visitor ( prj : Project.t ) 
    = object(self)
      inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
      val mutable is_computed = false
      val mutable _front_end : ( (A.t sem_and_logic_front_end) option ) = None
      val mutable ecfgs : ( ecfg list ) = []
	
      val mutable current_ecfg : ( ecfg_node list ) = []
      val mutable visited_nodes : (int * semantic_abstraction) list = []

      method _get_uid sid abstraction visitedList counter =
	match visitedList with
	  | [] -> -1 
	  | head::tail -> if head = (sid, abstraction) then counter
	    else self#_get_uid sid abstraction tail counter + 1
      method get_uid sid abstraction = 
	self#_get_uid sid abstraction visited_nodes 0
	
      method _build_node_list ( statement : stmt ) abstraction guardCounter front_end =
	let nodeSemantic = (statement.sid, abstraction) in
	if not (List.exists ( fun e -> e = nodeSemantic ) visited_nodes ) then 
	  begin
	    visited_nodes <- visited_nodes @ [nodeSemantic];
	    (* Volontary @ to keep list order *)
	    let subEdges = List.map ( fun e -> 
	      let newAbstraction, newGuardCounter = 
		front_end#next abstraction guardCounter e.skind 
	      in
	      let edgeUID = self#_build_node_list e newAbstraction newGuardCounter front_end in
	      Edge (edgeUID, newGuardCounter) 
	    ) statement.succs in
	    let currentUID = self#get_uid statement.sid abstraction in
	    current_ecfg <-  Node ( currentUID, Semantic ( statement, abstraction ), subEdges ) :: current_ecfg;
	    currentUID
	  end
	else (self#get_uid statement.sid abstraction)
	  
      method build_node_list ( funInfo : Cil_types.fundec ) front_end =
	current_ecfg <- [];
	let rootStmt = (List.hd funInfo.sallstmts) in
				(* Let is there to get rid of the warning *)
	let _ = self#_build_node_list rootStmt
        (front_end#get_entry_point_abstraction ()) (front_end#get_entry_point_precondition ()) front_end in
	CGraph (funInfo.svar.vname, current_ecfg)
	  
      method vglob_aux (g :Cil_types.global ) =
	is_computed <- true;
	match (g, _front_end) with 
	  | ( GFun ( funInfo, _ ), Some ( front_end ) ) -> ecfgs <- (self#build_node_list funInfo front_end) :: ecfgs; DoChildren
	  | _ -> DoChildren

      method set_front_end front_end = _front_end <- Some ( front_end ) 
      method get_ecfgs = ecfgs
    end


(** Compute the ecfg and fill the structures. *)
  let compute_ecfgs ( prj : Project.t ) ( ast : Cil_types.file ) ( front_end :
          A.t sem_and_logic_front_end ) = 
    let cfg_visitorInst = new cfg_visitor ( prj ) in	
    cfg_visitorInst#set_front_end front_end; 
    visitFramacFile ( cfg_visitorInst :> frama_c_copy ) ast;
    cfg_visitorInst#get_ecfgs 


(** An ecfg visitor with a callback function. You must use this method if you 
	    want to gather ecfg datas. *)
  let visite_ecfgs ecfgs pre_callback post_callback callback =
    List.iter 	( fun e ->
		  match e with
		    | CGraph ( fname, listOfNodes ) -> 
		      pre_callback fname;
		      List.iter ( callback fname ) listOfNodes;
		      post_callback fname;
		    | _ -> ()
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
	  
  let print_dot foc front_end _ node =
    match node with
      | Node (uid, Semantic ( statement, abstraction ), listOfEdges) -> 
	Format.fprintf foc "\t\t%d [label=\"%d/%d\\nCode : %s\\nAbstraction : %s\"]\n" 
	  uid uid statement.sid (stmt_to_string statement) (front_end#pretty abstraction);
	List.iter ( fun (Edge(toUid, counterValue))  -> 
	  Format.fprintf foc "\t\t%d -> %d [label=\"%s\"]\n\n" uid toUid counterValue
	) listOfEdges

(** This function export the given ecfg in dot format into the given file.
If the file does not exist, it is created. It is overwritten otherwise. *)
  let export_dot ecfgs filename front_end= 
    let oc = open_out filename in
    let foc = formatter_of_out_channel( oc ) in
    Format.fprintf foc "digraph G {\n";
    visite_ecfgs ecfgs 
      ( fun fname -> Format.fprintf foc "\tsubgraph cluster_%s {\n\t\tnode [style=filled,color=white]; \n\t\tstyle=filled; \n\t\tcolor=lightgray; \n\t\tlabel = \"%s\"; \n\t\tfontsize=40; \n\n" fname fname ) 
      ( fun _ -> Format.fprintf foc "\t}\n" ) 
      (print_dot foc front_end);
    Format.fprintf foc "\n}";
    Self.feedback ~level:0 "Graph exported!"
end;;
