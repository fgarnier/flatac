type token =
  | IDENT of (string)
  | EOF
  | COMMA
  | LBRACE
  | RBRACE
  | COLON
  | TRACEDECL
  | SEMICOLON

val gettrace :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Trace_types.trace
