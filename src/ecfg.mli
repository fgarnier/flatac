open Cil_types
open Sem_and_logic_front_end

module Ecfg : 
        functor ( A : sig 
                        type abstract_type 
                        type label_type
                      end ) ->
        sig
  type semantic_abstraction = A.abstract_type
  type counter_expression = A.label_type

  type semantic = Semantic of stmt * semantic_abstraction

  type ecfg_node_id = int
  type ecfg_edge = counter_expression
  type ecfg_node = Node of semantic * (ecfg_node_id, ecfg_edge) Hashtbl.t

  type ecfg = (ecfg_node_id, ecfg_node) Hashtbl.t

                val compute_ecfgs : Project.t -> Cil_types.file 
                -> (semantic_abstraction, counter_expression) sem_and_logic_front_end -> (string, ecfg) Hashtbl.t
  
                val visite_ecfgs : (string, ecfg) Hashtbl.t ->
                (string -> 'a) -> (string -> unit) ->
                (string -> int -> ecfg_node -> unit)
                -> unit

                val export_dot : (string, ecfg) Hashtbl.t -> string ->
                (semantic_abstraction, counter_expression) sem_and_logic_front_end -> unit 
        end

  
