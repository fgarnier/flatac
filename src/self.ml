(*
	Maxime Gaudin - VERIMAG 2011 

	** THIS MODULE IS A PART OF FLATA-C, DEVELOPED AT VERIMAG (2011)

	This module declares the plugin into Frama-C.

	For any question mail us to :
	- maxime.gaudin@imag.fr
	- florent.garnier@imag.fr
*)

module Self =
	Plugin.Register
		(struct
			let name = "FlataC"
			let shortname = "FlataC"
			let help= "This plugin demonstrate the work of Radu Iosif, Florent Ganier, Marius Bozga \
			and Maxime Gaudin about the automatic verification on critical embedded softwares."
		end)
