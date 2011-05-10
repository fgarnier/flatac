open Format

module Logging =
	struct
		(** This function register and display on output channel oc 
		 a failure described by information message 'm' **)
		let register_failure ( oc : Format.formatter ) ( m : string ) =
    			Format.formatter.fprintf "Failure : %s \n" m
		
	end;;
