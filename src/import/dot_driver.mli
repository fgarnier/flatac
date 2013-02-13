(**
 This file defines a functorial interface that allows to plot
 Numerical Transition System using the Graphviz dot Format.

 See Dot_driver.Make functor in order to browse both types and
 available functions.

 This file is released under the terms of the GNU LGPL v2.1 Licence.

 Developped in the Verimag Laboratory, by Florent Garnier, 
 contact me at florent dot garnier at gmail dot com for question and/or
 comments.

 Febuary 2013.
  
*)
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

      (** Prints a trance upon the hierachical transition system*)
      val dot_of_trace_upon_nts :
        NFParam.nts_system -> Trace_types.trace -> string
      
      (** *)
      val dot_of_subcfg_of_nts :
	NFParam.nts_system -> Trace_types.trace -> string
	
      (** Computes the Graphviz representation of hyerarchical numerical
      transition system.*)
      val dot_of_all_subsystem_of_nts :
	NFParam.nts_system -> string
	
    end
