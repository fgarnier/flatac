(*
	Maxime Gaudin - VERIMAG 2011 

	** THIS MODULE IS A PART OF FLATA-C, DEVELOPED AT VERIMAG (2011)

	Entry point of the plugin.

	For any question mail us to :
	- maxime.gaudin@imag.fr
	- florent.garnier@imag.fr
*)
open Self
open Ecfg
open SemAndLogicFrontEnd
open TrueLogicFrontEnd

module Enabled =
	Self.False
		(struct
	 		let option_name= "-flatac"
			let help= ""
			let kind= `Correctness
		 end)

module CfgExtension = struct type t = bool end;;
module BoolCFG = Ecfg ( CfgExtension ) 

let print () = 
	Self.feedback ~level:0 "Welcome to Flata-C !";
	let frontEnd = new trueLogicFrontEnd in
	let eCFGs = BoolCFG.computeECFGs (Project.current()) (Ast.get()) ( frontEnd ) in
		BoolCFG.exportDot eCFGs "output.dot" frontEnd

let run () = if Enabled.get () then print ()
let () = Db.Main.extend run