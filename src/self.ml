(*
   One need to add FRAMAC- ORIGINAL COPIRIGHT NOTICE HERE, with LGPL2
  stuff and consorts !


	Maxime Gaudin - VERIMAG 2011 

	** THIS MODULE IS A PART OF FLATA-C, DEVELOPED AT VERIMAG (2011)

	This module declares the plugin into Frama-C and allow
 other plugin to access to the logging system.

	For any question mail us to :
	- maxime.gaudin@imag.fr
	- florent.garnier@imag.fr
*)
module Self =
	Plugin.Register
		(struct
			let name = "FlataC"
			let shortname = "FlataC"
			let help= "Extracts flat counter automata based
	model of C program for memory faults detection."
		end)
