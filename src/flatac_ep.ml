open Ecfg
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

module CfgExtension =
	struct
		type t = int
	end;;

module IntCFG = Ecfg ( CfgExtension ) 

let print () = 
	IntCFG.computeECFGs (Project.current()) (Ast.get());
	Self.result "Hello world!";
	List.iter ( fun e -> print_string (e#getFunctionName ()); print_newline () ) (!IntCFG.eCFGs)

let run () = if Enabled.get () then print ()
let () = Db.Main.extend run
