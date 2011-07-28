module Ecfg_types = 
  functor ( A : sig type abstract_type type label_type end ) ->
struct
  type semantic_abstraction = A.abstract_type
  type counter_expression = A.label_type

  type semantic = Semantic of stmt * semantic_abstraction

  type ecfg_node_id = int
  type ecfg_edge = counter_expression
  type ecfg_node = Node of semantic * (ecfg_node_id, ecfg_edge) Hashtbl.t

  type ecfg = (ecfg_node_id, ecfg_node) Hashtbl.t
end;;
