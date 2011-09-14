(*open Validity*)
open Map


type var_valid = TrueValid 
		 | FalseValid
		 | DKValid

module  Validvarmap = Map.Make ( struct 
				   type t = string
				   let compare = String.compare 
				 end )

open Validvarmap

type validity_loc_map = 
    {
      parameter :  string t ;
      locals :  string t ;
      globals :  string t ;
    }
