(*
	Maxime Gaudin - VERIMAG 2011 

	** THIS MODULE IS A PART OF FLATA-C, DEVELOPED AT VERIMAG (2011)

	This module contains the definition of an eCFG and implements a generic algorithm to
	fill it with the correct Abstract Interpretation and counter automata label.

	For any question mail us to :
	- maxime.gaudin@imag.fr
	- florent.garnier@imag.fr
*)

open Self
open Cil
open Cil_types
open Cfg
open Visitor
open SemAndLogicFrontEnd

open Format
open Buffer

(** 
	 This module contains every structures and algorithms
	 relatives to eCFG. It's parametrized by the type of the 
	 abstract interpretation. 
	 Obviously, this type must match with the front-end inherited
	 type.
 *)
module Ecfg = functor ( A : sig type t end ) ->
struct
	type semanticAbstraction = A.t
	type semantic = Semantic of stmt * semanticAbstraction

	type eCFGEdge = Edge of int * counterExpression
	type eCFGNode = Node of int * semantic * (eCFGEdge list)
	type eCFG = 
	| CGraph of string * (eCFGNode list)
	| EmptyGraph

	(** This visitor visits global function and trigger the build 
	 of a new Cfg for each function *)
	class cfgVisitor ( prj : Project.t ) 
	= object(self)
	inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
		val mutable is_computed = false
		val mutable _frontEnd : ( (A.t semAndLogicFrontEnd) option ) = None
		val mutable _eCFGs : ( eCFG list ) = []

		val mutable _currentECFG : ( eCFGNode list ) = []
		val mutable _visitedNodes : (int * semanticAbstraction) list = []

		method _getUID sid abstraction visitedList counter =
			match visitedList with
			| [] -> -1 
			| head::tail -> if head = (sid, abstraction) then counter
					else self#_getUID sid abstraction tail counter + 1
		method getUID sid abstraction = self#_getUID sid abstraction _visitedNodes 0
	
		method _buildNodeList ( statement : stmt ) abstraction guardCounter frontEnd =
			let nodeSemantic = (statement.sid, abstraction) in
				if not (List.exists ( fun e -> e = nodeSemantic ) _visitedNodes ) then 
				begin
					_visitedNodes <- _visitedNodes @ [nodeSemantic]; (* Volontary @ to keep list order *)
					let subEdges = List.map ( fun e -> 
									let newAbstraction, newGuardCounter = frontEnd#next abstraction guardCounter e.skind in
										let edgeUID = self#_buildNodeList e newAbstraction newGuardCounter frontEnd in
											Edge (edgeUID, newGuardCounter) 
								) statement.succs in
						let currentUID = self#getUID statement.sid abstraction in
							_currentECFG <-  Node ( currentUID, Semantic ( statement, abstraction ), subEdges ) :: _currentECFG;
							currentUID
				end
				else (self#getUID statement.sid abstraction)
				
		method buildNodeList ( funInfo : fundec ) frontEnd =
			_currentECFG <- [];
			let rootStmt = (List.hd funInfo.sallstmts) in
				(* Let is there to get rid of the warning *)
				let _ = self#_buildNodeList rootStmt (frontEnd#getEntryPointAbstraction ()) (frontEnd#getEntryPointPrecondition ()) frontEnd in
					CGraph (funInfo.svar.vname, _currentECFG)

		method vglob_aux g =
			is_computed <- true;
			match (g, _frontEnd) with 
			| ( GFun ( funInfo, _ ), Some ( frontEnd ) ) -> _eCFGs <- (self#buildNodeList funInfo frontEnd) :: _eCFGs; DoChildren
			| _ -> DoChildren

		method setFrontEnd frontEnd = _frontEnd <- Some ( frontEnd ) 
		method getECFGs = _eCFGs
	end

	(** Compute the eCFG and fill the structures *)
	let computeECFGs ( prj : Project.t ) ( ast : Cil_types.file ) ( frontEnd : A.t semAndLogicFrontEnd ) = 
		let cfgVisitorInst = new cfgVisitor ( prj ) in	
			cfgVisitorInst#setFrontEnd frontEnd; 
			visitFramacFile ( cfgVisitorInst :> frama_c_copy ) ast;
			cfgVisitorInst#getECFGs 

	let visiteCFGs eCFGs preCallback postCallback callback =
		List.iter 	( fun e ->
					match e with
					| CGraph ( fname, listOfNodes ) -> 
						preCallback fname;
						List.iter ( callback fname ) listOfNodes;
						postCallback fname;
					| _ -> ()
				) eCFGs

	let stmtToString stmt =
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

	let printDot foc frontEnd _ node =
		match node with
		| Node (uid, Semantic ( statement, abstraction ), listOfEdges) -> 
			Format.fprintf foc "\t\t%d [label=\"%d/%d\\nCode : %s\\nAbstraction : %s\"]\n" 
				uid uid statement.sid (stmtToString statement) (frontEnd#pretty abstraction);
			List.iter ( fun (Edge(toUid, counterValue))  -> 
				  	Format.fprintf foc "\t\t%d -> %d [label=\"Counter : %s\"]\n\n" uid toUid counterValue
				  ) listOfEdges
	
	let exportDot eCFGs frontEnd= 
		let oc = open_out "output.dot" in
		let foc = formatter_of_out_channel( oc ) in
			Format.fprintf foc "digraph G {\n";
				visiteCFGs eCFGs 
					( fun fname -> Format.fprintf foc "\tsubgraph cluster_%s {\n\t\tnode [style=filled,color=white]; \n\t\tstyle=filled; \n\t\tcolor=lightgray; \n\t\tlabel = \"%s\"; \n\t\tfontsize=40; \n\n" fname fname ) 
					( fun _ -> Format.fprintf foc "\t}\n" ) 
					(printDot foc frontEnd);
			Format.fprintf foc "\n}";
			Self.feedback ~level:0 "Graph exported!"
end;;
