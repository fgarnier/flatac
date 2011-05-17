open Format
open Sys

module Logging = 
struct
	type logLevel =
	TRACE
	| INFO
	| WARNING
	| ERROR

	let registerMsg ( msg : string ) ( level : logLevel ) =
		let oc = open_out_gen [Open_text; Open_creat; Open_append] 0o774 "log" in
		let foc = formatter_of_out_channel( oc ) in
			match level with
			| TRACE -> Format.fprintf foc "%f - ERROR : %s\n" (time ()) msg;
			| INFO -> Format.fprintf foc "%f - ERROR : %s\n" (time ()) msg;
			| WARNING -> Format.fprintf foc "%f - ERROR : %s\n" (time ()) msg;
			| ERROR -> Format.fprintf foc "%f - ERROR : %s\n" (time ()) msg;

			close_out oc
end;;
