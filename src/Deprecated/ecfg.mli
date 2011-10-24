open Cil_types
open Sem_and_logic_front_end

open Ecfg_types 

module Ecfg : 
  functor ( A : sig 
              type abstract_type 
              type label_type
            end ) ->
sig
  module P_ecfg_types :
  sig
    type semantic_abstraction = A.abstract_type
    type counter_expression = A.label_type
    type semantic =
        Ecfg_types(A).semantic =
          Semantic of Cil_types.stmt * semantic_abstraction
    type ecfg_node_id = int
    type ecfg_edge = counter_expression
    type ecfg_node =
        Ecfg_types(A).ecfg_node =
          Node of semantic * (ecfg_node_id, ecfg_edge) Hashtbl.t
    type ecfg = (ecfg_node_id, ecfg_node) Hashtbl.t
  end
  
  val compute_ecfgs : Project.t -> Cil_types.file 
  -> (P_ecfg_types.semantic_abstraction, P_ecfg_types.counter_expression) sem_and_logic_front_end -> (string, P_ecfg_types.ecfg) Hashtbl.t

  val visit : (string, P_ecfg_types.ecfg) Hashtbl.t ->
    (string -> 'a) -> (string -> unit) ->
    (string -> int -> P_ecfg_types.ecfg_node -> unit)
  -> unit
end


