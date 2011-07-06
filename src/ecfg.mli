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
      
                type ecfg_edge = Edge of int * counter_expression
                type ecfg_node = Node of int * semantic * (ecfg_edge list)
                type ecfg = 
                | CGraph of string * (ecfg_node list)
                | EmptyGraph

                val compute_ecfgs : Project.t -> Cil_types.file 
                -> (semantic_abstraction, counter_expression) sem_and_logic_front_end -> ecfg list
  
                val visite_ecfgs : ecfg list ->
                (string -> 'a) -> (string -> unit) ->
                (string -> ecfg_node -> unit)
                -> unit

                val export_dot : ecfg list -> string ->
                (semantic_abstraction, counter_expression) sem_and_logic_front_end -> unit 
        end

  
