(*open Validity*)
open Map


type var_valid = TrueValid 
		 | FalseValid
		 | DKValid

type var_cathegory = LocalVar
		     | ParameterVar
		     | GlobalVar

type var_valid_entry = { validity : var_valid ;  location : var_cathegory ;}

module  Validvarmap = Map.Make ( struct 
				   type t = string
				   let compare = String.compare 
				 end )

open Validvarmap

type validity_loc_map = Validlocmap of string t 
   
