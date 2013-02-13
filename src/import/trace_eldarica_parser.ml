type token =
  | IDENT of (string)
  | EOF
  | COMMA
  | LBRACE
  | RBRACE
  | COLON
  | TRACEDECL
  | SEMICOLON

open Parsing;;
# 2 "trace_eldarica_parser.mly"

  open Lexing
  open Nts_types
  open Nts_functor

  open Ntsint.Nts_int
  open Trace_types

  exception Fail_to_extract_Sysname_Statename_from of string


  let starts_with_underscore s =
    if s.[0]='_' then true
    else false

  let state_of_str s =
    Format.printf "Separting state and system from %s \n %!" s;
    let slen = String.length s in
    let index_beg = ref 0 in
    let index_end = ref 0 in
    let starts_with_underscore = ref (starts_with_underscore s) 
    in 
    let is_state_part = ref false in
    let sysname = ref "" in
    let state_name = ref "" in
    let finish = ref false in
    
    while !index_end < slen && (not !finish) do
      if ( (not !is_state_part) && !starts_with_underscore 
	   && s.[(!index_end)] != '_') 
      then
	begin
	  index_end := (!index_end) +1 ;
	  starts_with_underscore := false 
	end
      else if ( (not !is_state_part) && !starts_with_underscore 
		&& s.[!index_end] = '_') 
      then
	begin
	  index_end := !index_end +1 ;
	end
	  
      else if ( (not !starts_with_underscore) && 
		  (not !is_state_part) && 
		  s.[!index_end]='_')
      then
	begin
	  sysname := String.sub s !index_beg !index_end ;
	  index_beg := !index_end+1;
	  index_end := !index_end+1;
	  is_state_part := true;
	end
      else if (!is_state_part && (s.[!index_end]='_')) 
      then
	begin
	  state_name := (String.sub s !index_beg (!index_end -1));
	  finish :=true
	end
      else
	index_end:= !index_end + 1;
    done;
    if (not !is_state_part) then
      raise (Fail_to_extract_Sysname_Statename_from s)
    else
      begin
	Format.printf "sysname = %s; control is = %s \n %!" !sysname !state_name;
	Trace_types.Sys_control(!sysname,!state_name)
      end
       
	  
      
	



# 89 "trace_eldarica_parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  258 (* COMMA *);
  259 (* LBRACE *);
  260 (* RBRACE *);
  261 (* COLON *);
  262 (* TRACEDECL *);
  263 (* SEMICOLON *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\000\000"

let yylen = "\002\000\
\005\000\003\000\001\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\002\000\000\000\004\000"

let yydgoto = "\002\000\
\004\000\007\000\008\000"

let yysindex = "\255\255\
\251\254\000\000\255\254\000\000\000\255\003\255\001\255\004\255\
\002\255\007\000\000\255\007\255\000\000\000\000\006\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\002\000\000\000"

let yytablesize = 13
let yytable = "\001\000\
\003\000\005\000\006\000\009\000\010\000\011\000\013\000\015\000\
\012\000\016\000\000\000\003\000\014\000"

let yycheck = "\001\000\
\006\001\003\001\003\001\001\001\004\001\002\001\000\000\001\001\
\007\001\004\001\255\255\004\001\011\000"

let yynames_const = "\
  EOF\000\
  COMMA\000\
  LBRACE\000\
  RBRACE\000\
  COLON\000\
  TRACEDECL\000\
  SEMICOLON\000\
  "

let yynames_block = "\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'statelist) in
    Obj.repr(
# 95 "trace_eldarica_parser.mly"
                                                 (
  let parsedlist = _3 in
   parsedlist
)
# 161 "trace_eldarica_parser.ml"
               : Trace_types.trace))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'state) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'statelist) in
    Obj.repr(
# 100 "trace_eldarica_parser.mly"
                                  ( _1 :: _3 )
# 169 "trace_eldarica_parser.ml"
               : 'statelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 101 "trace_eldarica_parser.mly"
             ([_1])
# 176 "trace_eldarica_parser.ml"
               : 'statelist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 103 "trace_eldarica_parser.mly"
                                            (Trace_types.Sys_control(_2,_4))
# 184 "trace_eldarica_parser.ml"
               : 'state))
(* Entry gettrace *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let gettrace (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Trace_types.trace)
