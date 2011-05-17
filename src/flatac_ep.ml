open Self
open Ecfg

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
	Self.feedback~level:0 "Welcome to Flata-C !";
	IntCFG.computeECFGs (Project.current()) (Ast.get())
	(* List.iter ( fun e -> Self.debug ~level:0 "%s\n" (e#getFunctionName ())) (!IntCFG.eCFGs) *)

let run () = if Enabled.get () then print ()
let () = Db.Main.extend run
