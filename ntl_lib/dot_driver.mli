module Make :
  functor (Param : Nts_functor.NTS_PARAM) ->
    sig
      module NFParam :
        sig
          type anotations = Nts_functor.Make(Param).anotations
          type control = Nts_functor.Make(Param).control
          type nts_automaton =
            Nts_functor.Make(Param).nts_automaton 

	  type nts_system =
            Nts_functor.Make(Param).nts_system 
	end
      

      type anotations = NFParam.anotations
      type control = NFParam.control
      type nts_automaton = NFParam.nts_automaton
      type nts_system = NFParam.nts_system
      
      (** Exports a GraphViz .dot format descritpion of a NTS
	counter automaton.*)
      val dot_of_cautomaton :
        ?standalone_graph:bool -> NFParam.nts_automaton -> string
     
      (** 
 	Exports the collection of all automata of a NTS hyerachical
	system into a .dot file.
      *)
      val dot_of_nts : NFParam.nts_system -> string
    end
