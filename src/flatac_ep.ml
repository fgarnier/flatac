open Logging

module Self =
	Plugin.Register
		(struct
			let name = "FlataC"
			let shortname = "FlataC"
			let help= "This plugin demonstrate the work of Radu Iosif, Florent Ganier, Marius Bozga \
			and Maxime Gaudin about the automatic verification on critical embedded softwares."
		end)

module Enabled =
	Self.False
		(struct
	 		let option_name= "-flatac"
			let help= ""
			let kind= `Correctness
		 end)

let print () = 
	Self.result "Hello world!";
	Logging.registerMsg "Salut les louloutes !" Logging.TRACE

let run () = if Enabled.get () then print ()
let () = Db.Main.extend run
