val curr_lexbuf : string ref
type loc = { file : string; line : int; start : int; stop : int; }
val loc_of_pos : Lexing.lexbuf -> loc
val error : loc -> 'a -> string
