(** 
Type definition of validity expression for intermediate language
expression when associated with some sslf formula.
*)

type valid_counter = PtValid of string
		     | IntValid of string
		     | NotValid of valid_counter
		     | AndValid of valid_counter * valid_counter
		     | OrValid of valid_counter * valid_counter
		     | TrueValid
		     | FalseValid
