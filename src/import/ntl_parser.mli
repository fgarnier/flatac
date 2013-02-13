type token =
  | INT of (Big_int.big_int)
  | FLOAT of (float)
  | IDENT of (string)
  | DOTTEDIDENT of (string)
  | REAL of (float)
  | PRIMEDVAR of (string)
  | TIMES
  | PLUS
  | MINUS
  | UMINUS
  | DIV
  | MOD
  | LT
  | GT
  | EQ
  | NEQ
  | LEQ
  | GEQ
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | OBRACK
  | CBRACK
  | COLON
  | SEMICOLON
  | COMMA
  | ARROW
  | PRIME
  | BTRUE
  | BFALSE
  | BAND
  | BOR
  | BNOT
  | EOF
  | NTSDECL
  | INTDECL
  | NATDECL
  | REALDECL
  | BOOLDECL
  | INITSTATE
  | FINALSTATE
  | ERRORSTATE
  | INPUTVARLIST
  | OUTPUTVARLIST
  | LOCALVARLIST
  | HAVOC
  | EXISTS
  | FORALL
  | IMPLY
  | EQUIV
  | LRARROW
  | DOT
  | INSTANCES
  | INIT

val ntldescr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ntsint.Nts_int.nts_system
