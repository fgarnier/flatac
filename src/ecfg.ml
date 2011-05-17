open Self
open Cil
open Cil_types
open Cfg
open Visitor

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
	type stmtId = int

	type transitionOp = Op of Cil_types.stmtkind
	| EntryPoint

	type semanticValue = ASem of A.t 
	| None

	type cfg = Node of stmtId * transitionOp * semanticValue * (cfg list) 
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

	let rec _buildCfg ( stmtData : Cil_types.stmt ) visited = 
		let children = ref [] in
			List.iter 	( fun stmt -> 
						try (** Already visited *)
							if IntHashtbl.find visited stmt.sid then () 		
							(** Dirty exception hacking to handle the "Already visited" case *)
						with _ -> (** Never visited *)
							IntHashtbl.add visited stmt.sid true;
							children := (_buildCfg stmt visited) :: !children 
					) stmtData.succs;
			Node ( stmtData.sid, EntryPoint, None , !children ) 

	(** Private method called by the CilCFG Visitor at each function *)
	let buildCfg ( funInfo : fundec ) = new eCFG funInfo.svar.vname (_buildCfg (List.hd funInfo.sallstmts) (IntHashtbl.create 1))

	(** eCFGs accessor. *)
	let eCFGs : ( (eCFG list) Pervasives.ref ) = ref []

	(** This visitor visits global function and trigger the build 
	 of a new Cfg each one *)
	class cfgVisitor ( prj : Project.t ) 
	= object
	inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
		val mutable is_computed = false
	
		method vglob_aux g =
			is_computed <- true;
			match g with 
			| GFun ( funInfo, _ ) -> eCFGs := (buildCfg funInfo) :: !eCFGs; DoChildren
			| _ -> DoChildren
	end

	(** Compute the eCFG and fill the structures *)
	let computeECFGs ( prj : Project.t ) ( ast : Cil_types.file ) = 
		let cfgVisitorInst = new cfgVisitor ( prj ) in	
			visitFramacFile ( cfgVisitorInst :> frama_c_copy ) ast;
end;;
