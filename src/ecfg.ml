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

	type cfgNode = 
	| Node of int * stmtId * cOperation * semanticValue * ((cfgNode * counterExpression) list)
	| Empty

	(* Container class of eCFG *)
	class eCFG fName stmtData ( frontEnd : A.t semAndLogicFrontEnd ) 
	= object(self)
		val mutable _fName = ""
		val mutable _root = Empty
		val mutable _maxUID = 0
		val mutable _visited = IntHashtbl.create 100

		initializer 
			_fName <- fName;
			let epAbstraction = frontEnd#getEntryPointAbstraction () in let epCounterValue = frontEnd#getEntryPointPrecondition () in 
				_root <- self#buildNode epAbstraction epCounterValue stmtData frontEnd
		
		method buildNode currentAbstraction counterValue stmtData ( frontEnd : A.t semAndLogicFrontEnd ) =
			let bindings = IntHashtbl.find_all _visited stmtData.sid in
			if not ( List.exists ( fun v -> v = currentAbstraction ) bindings ) then 
			begin
				IntHashtbl.add _visited stmtData.sid currentAbstraction;
				_maxUID <- _maxUID + 1;
				let deref = _maxUID in
				Self.debug ~level:0 "Visite du noeud %d, stmt = %d" _maxUID stmtData.sid;
				Node 	( deref, stmtData.sid, (Op stmtData), currentAbstraction, 
						List.map 	( fun subStmt -> 
									let subAbs, newCounter = frontEnd#next currentAbstraction counterValue subStmt.skind in  
										self#buildNode subAbs counterValue subStmt frontEnd, newCounter
								) stmtData.succs
					)
			end
			else Empty

		method getFunctionName () = _fName
		method getRoot () = _root
	end

	let buildCfg ( frontEnd : A.t semAndLogicFrontEnd ) ( funInfo : fundec ) = new eCFG funInfo.svar.vname (List.hd funInfo.sallstmts) frontEnd

	(* Node stmtData.sid stmtData currentAbstraction *) 

	(** eCFGs accessor. *)
	let eCFGs : ( (eCFG list) ref ) = ref []

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
			| ( GFun ( funInfo, _ ), Some ( frontEnd ) ) -> eCFGs := (buildCfg frontEnd funInfo) :: !eCFGs; DoChildren
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
		| Node (_, _, _, _, children ) -> List.iter ( fun (newRoot, _) -> _visiteCFGs newRoot callback ) children
		| Empty -> () 

	let visiteCFGs callback =
		List.iter ( fun flowGraph -> _visiteCFGs (flowGraph#getRoot ()) callback ) !eCFGs

	let stmtToString stmt =
		Buffer.reset stdbuf;
		Cil.printStmt Cil.defaultCilPrinter str_formatter stmt;
		String.escaped (Buffer.contents stdbuf)


	let printDot foc node =
		match node with
		| Node (uid, sid, Op (stmtData), _, l) -> 
			List.iter 	
			( fun (node, _) -> 
				match node with
				| Node (childUID, childSID, Op ( childStmtData ), _, _) -> 
					Format.fprintf foc "%d [label=\"%d - %s\"]\n" uid sid (stmtToString stmtData);
					Format.fprintf foc "%d [label=\"%d - %s\"]\n" childUID childSID (stmtToString childStmtData);
					Format.fprintf foc "%d -> %d\n\n" uid childUID;
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
