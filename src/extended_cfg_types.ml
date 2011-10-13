open Cil
open Cil_types

module Extended_cfg_types = 
  functor ( A : sig type abstract_type type label_type end ) ->
struct
  
  type abs_dom_val = A.abstract_type
  type trans_label_val = A.label_type

  type ecfg_vertex = { id : int;
		       statement : Cil_types.stmt;
		       abstract_val : abs_dom_val;
		     }

  let init_hashtbl_size = 97   
(*  type ecfg_node_id = int
  type ecfg_edge = counter_expression
  type ecfg_node = Node of semantic * (ecfg_node_id, ecfg_edge) Hashtbl.t

  type ecfg = (ecfg_node_id, ecfg_node) Hashtbl.t *)
end;;
