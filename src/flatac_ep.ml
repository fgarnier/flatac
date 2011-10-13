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
open Sem_and_logic_front_end
open Extended_cfg_types
open Extended_cfg

(* open True_logic_front_end *) 
(* open Trivalue_logic_front_end *)

open Ssl_types
open Ssl
open SSL_lex
open Flatac_ssl_front_end
open Dot_exporter
open List
 (* Working on the cartesian prouct with validity pre-processing.*)
open Nts_types
open Ssl_valid_abs_dom_types

module Enabled =
	Self.False
		(struct
	 		let option_name= "-flatac"
			let help= ""
			let kind= `Correctness
		 end)

(** This module is used as a parameter of the Ecfg functor *)
module CfgExtension = 
        struct 
                type abstract_type = Ssl_valid_abs_dom_types.ssl_validity_absdom
                type label_type = Nts_types.cnt_trans_label list
        end;;

(** Creation of an Ecfg which abstract domain is {true,false}*)
module Parametrized_cfg = Ecfg ( CfgExtension ) 
module Parametrized_dot_exporter = Dot_exporter ( CfgExtension )

let print () = 
	Self.feedback ~level:0 "Welcome to Flata-C !";
	let frontEnd = new ssl_flatac_front_end in
	let eCFGs = Parametrized_cfg.compute_ecfgs (Project.current()) (Ast.get()) ( frontEnd ) in
	  Parametrized_dot_exporter.export_dot eCFGs "output.dot" frontEnd 

let run () = if Enabled.get () then print ()
let () = Db.Main.extend run
