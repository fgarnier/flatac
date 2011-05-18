open Self
open Cil
open Cil_types
open Cfg
open Visitor
open SemAndLogicFrontEnd

(*
	Maxime Gaudin - VERIMAG 2011 

	** THIS MODULE IS A PART OF FLATA-C, DEVELOPED AT VERIMAG (2011)

	This module contains the definition of an eCFG and implements a generic algorithm to
	fill it with the correct Abstract Interpretation and counter automata label

	For any question mail us to :
	- maxime.gaudin@imag.fr
	- florent.garnier@imag.fr
*)

(** This HashTable is defined to avoid looping through CFG *)
module HashInt =
struct
	type t = int
	let equal i j = ( i = j )
	let hash k = k
end;;
module IntHashtbl = Hashtbl.Make ( HashInt )

module Ecfg = functor ( A : sig type t end ) ->
struct
	(* Node label *)
	type stmtId = int
	type semanticValue = A.t
	(*******************)

	(* Transition *)
	type counterExpression = string
	type transitionOp = 
	| Op of Cil_types.stmtkind
	| EntryPoint
	type cfgTransition = Transition of transitionOp * counterExpression
	(******************************************************************)

	type cfg = 
	| Node of stmtId * semanticValue * cfgTransition * (cfg list) 
	| Leaf of stmtId * semanticValue
	| Empty

	(* Container class of eCFG *)
	class eCFG fName (root : cfg)
	= object
		val mutable _fName = ""
		val mutable _root : cfg = Empty

		initializer 
			_fName <- fName;
			_root <- root;
			Self.debug ~level:1 "New eCFG built : %s" _fName

		method getFunctionName () = _fName
		method getRoot () = _root
	end

	let rec _buildCfg ( stmtData : Cil_types.stmt ) ( newSemanticValue : semanticValue ) ( frontEnd : A.t semAndLogicFrontEnd ) visited = 
		let children = ref [] in
			if (List.length stmtData.succs) = 0 then
				Leaf ( stmtData.sid, newSemanticValue )
			else
				(List.iter 	( fun stmt -> 
							try (** Already visited *)
								if IntHashtbl.find visited stmt.sid then () 		
								(** Dirty exception hacking to handle the "Already visited" case *)
							with _ -> (** Never visited *)
								IntHashtbl.add visited stmt.sid true;
								let abs, newCounter = frontEnd#next newSemanticValue "" stmtData.skind in
									children := (_buildCfg stmt abs frontEnd visited) :: !children 
						) stmtData.succs;
				Node ( stmtData.sid, newSemanticValue, Transition ( EntryPoint, "" ) , !children ))

	(** Private method called by the CilCFG Visitor at each function *)
	let buildCfg ( frontEnd : A.t semAndLogicFrontEnd ) ( funInfo : fundec ) = 
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
			| ( GFun ( funInfo, _ ), Some ( frontEnd ) ) -> eCFGs := (buildCfg frontEnd funInfo) :: !eCFGs; DoChildren
			| _ -> DoChildren

		method setFrontEnd frontEnd = _frontEnd <- Some ( frontEnd ) 
	end

	(** Compute the eCFG and fill the structures *)
	let computeECFGs ( prj : Project.t ) ( ast : Cil_types.file ) ( frontEnd : A.t semAndLogicFrontEnd ) = 
		let cfgVisitorInst = new cfgVisitor ( prj ) in	
			cfgVisitorInst#setFrontEnd frontEnd; 
			visitFramacFile ( cfgVisitorInst :> frama_c_copy ) ast
end;;
