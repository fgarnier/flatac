open Cil
open Cil_types
open Visitor

module ECFG = functor ( A : sig type t end ) ->
struct

	type cfgLeaf = Leaf of int * A.t
	type cfg = Node of cfgLeaf * (cfgLeaf list)

	class eCFG ( prj : Project.t ) 
	= object(self)
	inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
		val mutable is_computed = false
	
		method vglob_aux g =
			is_computed <- true;
			match g with 
			| GFun ( funInfo, _ ) -> DoChildren
			| _ -> DoChildren
	
	end
end;;
