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
 	This HashTable is defined to avoid 
	looping through CFG 
*)
module HashInt =
struct
	type t = int
	let equal i j = ( i = j )
	let hash k = k
end;;
module IntHashtbl = Hashtbl.Make ( HashInt )
(******************************************)

(** 
	 This module contains every structures and algorithms
	 relatives to eCFG. It's parametrized by the type of the 
	 abstract interpretation. 
	 Obviously, this type must match with the front-end inherited
	 type.
 *)
module Ecfg = functor ( A : sig type t end ) ->
struct
	(* Node *)
	type stmtId = int
	type cOperation = 
	| Op of Cil_types.stmt
	| EntryPoint
	type semanticValue = A.t
	(***********************)

	(* Transition *)
	type counterExpression = string
	type cfgTransition = Transition of counterExpression
	(******************************************************************)

	type cfg = 
	| Node of stmtId * cOperation * semanticValue * cfgTransition * (cfg list) 
	| Leaf of stmtId * cOperation * semanticValue
	| Empty

	(* Container class of eCFG *)
	class eCFG fName (root : cfg)
	= object
		val mutable _fName = ""
		val mutable _root : cfg = Empty

		initializer 
			_fName <- fName;
			_root <- root;
			match _root with
			| Node ( sid, _, _, _, _ ) -> Self.debug ~level:1 "New eCFG node built : %s ( %d )" _fName sid
			| Leaf ( sid, _, _) -> Self.debug ~level:1 "New eCFG leaf built : %s ( %d )" _fName sid
			| _ -> ()

		method getFunctionName () = _fName
		method getRoot () = _root
	end

	let rec _buildCfg ( stmtData : Cil_types.stmt ) ( newSemanticValue : semanticValue ) ( frontEnd : A.t semAndLogicFrontEnd ) visited = 
		Self.debug ~level:1 "Visiting %d..." stmtData.sid;
		IntHashtbl.add visited stmtData.sid ( Leaf ( stmtData.sid, Op (stmtData), newSemanticValue ) );

		let children = ref [] in
			if (List.length stmtData.succs) = 0 then
				let newNode = Leaf ( stmtData.sid, Op (stmtData), newSemanticValue ) in
					Self.debug ~level:0 "New node built : %d (%s) with %d succs" stmtData.sid (frontEnd#pretty newSemanticValue) (List.length !children);
					IntHashtbl.replace visited stmtData.sid newNode; newNode
			else
			begin
					(** For each successors ... *)
					List.iter 	( fun stmt -> 
							 	(* Already visited *) 
								try 
									let s = IntHashtbl.find visited stmt.sid in children := s :: !children 
 								(** Never visited *)
								with _ ->
									Self.debug ~level:0 "%d not visited yet." stmt.sid;
									(* While waiting for computing th real value *)
									let abs, newCounter = frontEnd#next newSemanticValue "" stmtData.skind in
										children := (_buildCfg stmt abs frontEnd visited) :: !children 
							) stmtData.succs;
					Self.debug ~level:0 "New node built : %d (%s) with %d succs" stmtData.sid (frontEnd#pretty newSemanticValue) (List.length !children);
					let newNode = Node ( stmtData.sid, Op ( stmtData ), newSemanticValue, Transition ( "" ) , !children ) in
						(* Here it is ! *) 
						Self.debug ~level:0 "Removing last reference...";
						IntHashtbl.replace visited stmtData.sid newNode;
						newNode
			end	

	(** Private method called by the CilCFG Visitor at each function *)
	let buildCfg ( frontEnd : A.t semAndLogicFrontEnd ) ( funInfo : fundec ) = 
		let ep in 
			ep = Node ( 
		new eCFG funInfo.svar.vname (_buildCfg (List.hd funInfo.sallstmts) (frontEnd#getEntryPointAbstraction ()) (frontEnd) (IntHashtbl.create 1))

	(** eCFGs accessor. *)
	let eCFGs : ( (eCFG list) Pervasives.ref ) = ref []

	(** This visitor visits global function and trigger the build 
	 of a new Cfg each one *)
	class cfgVisitor ( prj : Project.t ) 
	= object
	inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
		val mutable is_computed = false
		val mutable _frontEnd : ( (A.t semAndLogicFrontEnd) option ) = None

		method vglob_aux g =
			is_computed <- true;
			match (g, _frontEnd) with 
			| ( GFun ( funInfo, _ ), Some ( frontEnd ) ) -> 
				eCFGs := (buildCfg frontEnd funInfo) :: !eCFGs; 
				Self.debug ~level:0 "Building eCFG #%d..." (List.length !eCFGs);
				DoChildren
			| _ -> DoChildren

		method setFrontEnd frontEnd = _frontEnd <- Some ( frontEnd ) 
		method getComputationState = is_computed
	end

	(** Compute the eCFG and fill the structures *)
	let computeECFGs ( prj : Project.t ) ( ast : Cil_types.file ) ( frontEnd : A.t semAndLogicFrontEnd ) = 
		let cfgVisitorInst = new cfgVisitor ( prj ) in	
			cfgVisitorInst#setFrontEnd frontEnd; 
			visitFramacFile ( cfgVisitorInst :> frama_c_copy ) ast;
			Self.debug ~level:0 "Computed %d eCFGs... " (List.length !eCFGs)

	let rec _visiteCFGs root callback =
		callback root;
		match root with
		| Node (sid, _, _, _, l) -> 
			Self.debug ~level:0 "Visiting node %d..." sid;
			List.iter ( fun newRoot -> _visiteCFGs newRoot callback ) l
		| Leaf (sid, _, _ ) -> 
			Self.debug ~level:0 "Visiting leaf %d..." sid
		| _ -> Self.debug ~level:0 "Visiting empty ..."

	let visiteCFGs callback =
		List.iter ( fun g -> _visiteCFGs (g#getRoot ()) callback ) !eCFGs

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

	let stmtToString stmt =
		Buffer.reset stdbuf;
		Cil.printStmt Cil.defaultCilPrinter str_formatter stmt;
		String.escaped (Buffer.contents stdbuf)
		

	let printDot foc node =
		match node with
		| Leaf (sid, Op (stmtData), _ ) -> ()
		| Node (sid, Op (stmtData), _, _, l) -> 
							List.iter 	
							( fun e -> 
								match e with
								| Leaf (childSid, Op ( childStmtData ), _) -> 
									Format.fprintf foc "%d [label=\"%d - %s\"]\n" sid sid (stmtToString stmtData);
									Format.fprintf foc "%d [label=\"%d - %s\"]\n" childSid childSid (stmtToString childStmtData);
									Format.fprintf foc "%d -> %d\n\n" sid childSid
								| Node (childSid, Op ( childStmtData ), _ , _, _) -> 
									Format.fprintf foc "%d [label=\"%d - %s\"]\n" sid sid (stmtToString stmtData);
									Format.fprintf foc "%d [label=\"%d - %s\"]\n" childSid childSid (stmtToString childStmtData);
									Format.fprintf foc "%d -> %d\n\n" sid childSid;
								| _ -> ()
							) l
		| _ -> ()
	
	let exportDot () = 
		let oc = open_out "output.dot" in
		let foc = formatter_of_out_channel( oc ) in
			Format.fprintf foc "digraph G {\n";
				visiteCFGs (printDot foc);
			Format.fprintf foc "\n}";
			Self.feedback ~level:0 "Graph exported!"
end;;
