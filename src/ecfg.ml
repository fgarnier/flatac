open Cil
open Cil_types
open Cfg
open Visitor

module Ecfg = functor ( A : sig type t end ) ->
struct
	type cfgLeaf = Leaf of int * A.t
	type cfg = Node of cfgLeaf * (cfgLeaf list)
	| Empty

	(* Container class of eCFG *)
	class eCFG fName (root : cfg)
	= object(self)
		val mutable _fName = ""
		val mutable _root : cfg = Empty

		initializer 
			_fName <- fName;
			_root <- root;

		method getFunctionName () = _fName
		method getRoot () = _root
	end

	(* eCFG accessor. One eCFG exists for each global function *)
	let eCFGs : ( (eCFG list) Pervasives.ref ) = ref []

	(* Private method called by the CilCFG Visitor at each function *)
	let buildCfg ( funInfo : fundec ) = new eCFG funInfo.svar.vname Empty

	(* This CfgVisitor visits each global function and trigger the build 
	 of a new Cfg *)
	class cfgVisitor ( prj : Project.t ) 
	= object(self)
	inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
		val mutable is_computed = false
	
		method vglob_aux g =
			is_computed <- true;
			match g with 
			| GFun ( funInfo, _ ) -> eCFGs := (buildCfg funInfo) :: !eCFGs; DoChildren
			| _ -> DoChildren
	end

	let computeECFGs ( prj : Project.t ) ( ast : Cil_types.file ) = 
		let cfgVisitorInst = new cfgVisitor ( prj ) in	
			visitFramacFile ( cfgVisitorInst :> frama_c_copy ) ast;
end;;
