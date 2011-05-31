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
	 of a new Cfg each one *)
	class cfgVisitor ( prj : Project.t ) 
	= object(self)
	inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
		val mutable is_computed = false
		val mutable _frontEnd : ( (A.t semAndLogicFrontEnd) option ) = None
		val mutable _eCFGs : ( eCFG list ) = []

		val mutable _currentECFG : ( eCFGNode list ) = []
		val mutable _visitedNodes : (int * semanticAbstraction) list = []

		method _buildNodeList ( statement : stmt ) abstraction guardCounter frontEnd =
			let nodeSemantic = (statement.sid, abstraction) in
			if not (List.exists ( fun e -> e = nodeSemantic ) _visitedNodes ) then 
			begin
				_visitedNodes <- nodeSemantic :: _visitedNodes;
				let subEdges = List.map ( fun e -> 
								let newAbstraction, newGuardCounter = frontEnd#next abstraction guardCounter e.skind in
									self#_buildNodeList e newAbstraction newGuardCounter frontEnd;
									Edge (e.sid, newGuardCounter) 
							) statement.succs in
					_currentECFG <- Node ( statement.sid, Semantic ( statement, abstraction ), subEdges ) :: _currentECFG
			end
				
		method buildNodeList ( funInfo : fundec ) frontEnd =
			_currentECFG <- [];
			let rootStmt = (List.hd funInfo.sallstmts) in
				self#_buildNodeList rootStmt (frontEnd#getEntryPointAbstraction ()) (frontEnd#getEntryPointPrecondition ()) frontEnd;
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

	let visiteCFGs eCFGs callback =
		List.iter 	( fun e ->
					match e with
					| CGraph ( _, listOfNodes ) -> List.iter ( callback ) listOfNodes
					| _ -> ()
				) eCFGs

	let stmtToString stmt =
		Buffer.reset stdbuf;
		match stmt.skind with
		| Instr ( Set ( (Var ( lvalueInfo ), _) , expression, _ ) ) ->
			let vname = lvalueInfo.vname in
				add_string stdbuf vname; add_string stdbuf " = ";
				Cil.printExp Cil.defaultCilPrinter str_formatter expression; 
				String.escaped (Buffer.contents stdbuf)
		| _ -> 
			Cil.printStmt Cil.defaultCilPrinter str_formatter stmt; 
			String.escaped (Buffer.contents stdbuf)

	let printDot foc node =
		match node with
		| Node (uid, Semantic ( statement, _), listOfEdges) -> 
			Format.fprintf foc "%d [label=\"%d - %s\"]\n" uid uid (stmtToString statement);
			List.iter 	
				( fun (Edge(toUid, counterValue))  -> Format.fprintf foc "%d -> %d [label=\"Counter : %s\"]\n\n" uid toUid counterValue) listOfEdges
		| _ -> ()
	
	let exportDot eCFGs = 
		let oc = open_out "output.dot" in
		let foc = formatter_of_out_channel( oc ) in
			Format.fprintf foc "digraph G {\n";
				visiteCFGs eCFGs (printDot foc);
			Format.fprintf foc "\n}";
			Self.feedback ~level:0 "Graph exported!"

(*
	let prettyNode eCFGNode =
	match eCFGNode with
	| Leaf (sid, Op (stmtData), _ ) -> sprintf "%d / %s / [ ]" sid (stmtToString stmtData)
	| Node (sid, Op (stmtData), _, _, l) ->
		let sidList = "" in
			List.iter 
			( fun n ->
				match node with
				| Leaf ( succId, _, _ ) -> let sidList = sprintf "%s; %d" sidList succId
			) l
*)

end;;
