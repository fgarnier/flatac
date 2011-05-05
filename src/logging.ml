module Logging
	struct
		(** This function register and display on output channel oc 
		 a failure described by information message 'm' **)
		let register_failure ( oc : out_channel ) ( m : string ) =
    			Printf.printf "Failure"
		;;
	end;;
