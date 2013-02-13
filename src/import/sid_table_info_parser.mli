type token =
  | INT of (int)
  | IDENT of (string)
  | ANNOT of (string)
  | MAPESIDTOSID
  | OPENGROUP
  | CLOSEGROUP
  | CODEBLOCKOPEN
  | CODEBLOCKCLOSE
  | ENDLINE
  | EQ
  | EOF
  | CCODE
  | SID
  | COLON
  | SEMICOLON
  | FUNDECL
  | DECLMAPESIDTOSID
  | DECLARECODEMAP
  | LEXINFOS
  | POSINFOS

val mapextract :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Trace_types.tr_subsystem_table
