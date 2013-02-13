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

open Parsing;;
# 2 "sid_table_info_parser.mly"
  open Lexing
  open Trace_types 
  

  exception ParseLexinfosexception of string * string
  
  let esid_sid_tbl_builder tbl ((l,r) : (esid * sid)) =
    Hashtbl.add tbl l r
      
  let sid_to_code_tbl_builder tbl ((l,r,s) : (sid * string * (Lexing.position * Lexing.position ) option )) =
    Hashtbl.add tbl l (r,s)
      
      
  let tr_subsystem_builder_iterator tbl 
      ((l,r) : (string *  map_2_fcinfos)) =
    Hashtbl.add tbl l r
      
# 43 "sid_table_info_parser.ml"
let yytransl_const = [|
  260 (* MAPESIDTOSID *);
  261 (* OPENGROUP *);
  262 (* CLOSEGROUP *);
  263 (* CODEBLOCKOPEN *);
  264 (* CODEBLOCKCLOSE *);
  265 (* ENDLINE *);
  266 (* EQ *);
    0 (* EOF *);
  267 (* CCODE *);
  268 (* SID *);
  269 (* COLON *);
  270 (* SEMICOLON *);
  271 (* FUNDECL *);
  272 (* DECLMAPESIDTOSID *);
  273 (* DECLARECODEMAP *);
  274 (* LEXINFOS *);
  275 (* POSINFOS *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* IDENT *);
  259 (* ANNOT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\006\000\006\000\007\000\
\005\000\008\000\008\000\009\000\009\000\010\000\011\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\010\000\004\000\001\000\002\000\003\000\
\004\000\001\000\002\000\006\000\010\000\002\000\019\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\016\000\000\000\000\000\000\000\001\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\000\007\000\
\000\000\000\000\008\000\000\000\000\000\000\000\004\000\000\000\
\009\000\011\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\014\000\021\000\018\000\019\000\029\000\
\030\000\042\000\043\000"

let yysindex = "\001\000\
\252\254\000\000\244\254\000\000\004\000\252\254\251\254\000\000\
\000\000\005\255\255\254\249\254\006\255\001\255\011\255\253\254\
\009\255\010\255\011\255\012\255\013\255\014\255\000\000\000\000\
\007\255\015\255\000\000\016\255\017\255\007\255\000\000\019\255\
\000\000\000\000\004\255\020\255\021\255\008\255\023\255\018\255\
\025\255\026\255\018\255\024\255\000\000\000\000\027\255\031\255\
\028\255\032\255\029\255\034\255\030\255\036\255\033\255\039\255\
\035\255\043\255\037\255\040\255\038\255\042\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\044\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\045\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\250\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\040\000\000\000\000\000\000\000\034\000\000\000\024\000\
\000\000\000\000\012\000"

let yytablesize = 55
let yytable = "\012\000\
\003\000\001\000\007\000\008\000\010\000\012\000\011\000\012\000\
\013\000\016\000\015\000\017\000\022\000\020\000\027\000\023\000\
\025\000\036\000\028\000\035\000\031\000\026\000\033\000\038\000\
\002\000\047\000\039\000\040\000\032\000\044\000\037\000\045\000\
\049\000\051\000\053\000\041\000\048\000\055\000\052\000\057\000\
\061\000\050\000\056\000\054\000\059\000\009\000\060\000\063\000\
\058\000\006\000\010\000\062\000\024\000\034\000\046\000"

let yycheck = "\006\001\
\005\001\001\000\015\001\000\000\010\001\012\001\002\001\009\001\
\016\001\009\001\005\001\001\001\004\001\017\001\001\001\006\001\
\005\001\014\001\012\001\001\001\006\001\009\001\006\001\003\001\
\000\000\002\001\019\001\005\001\013\001\005\001\011\001\006\001\
\002\001\002\001\001\001\018\001\010\001\002\001\010\001\001\001\
\001\001\014\001\010\001\014\001\002\001\006\000\010\001\006\001\
\014\001\006\001\006\001\014\001\019\000\030\000\043\000"

let yynames_const = "\
  MAPESIDTOSID\000\
  OPENGROUP\000\
  CLOSEGROUP\000\
  CODEBLOCKOPEN\000\
  CODEBLOCKCLOSE\000\
  ENDLINE\000\
  EQ\000\
  EOF\000\
  CCODE\000\
  SID\000\
  COLON\000\
  SEMICOLON\000\
  FUNDECL\000\
  DECLMAPESIDTOSID\000\
  DECLARECODEMAP\000\
  LEXINFOS\000\
  POSINFOS\000\
  "

let yynames_block = "\
  INT\000\
  IDENT\000\
  ANNOT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nts_map_list) in
    Obj.repr(
# 33 "sid_table_info_parser.mly"
                              (
  let tbl = Hashtbl.create 97 in
  List.iter ( tr_subsystem_builder_iterator tbl ) _1;
  tbl
    
)
# 173 "sid_table_info_parser.ml"
               : Trace_types.tr_subsystem_table))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'extract_subsystable_map) in
    Obj.repr(
# 41 "sid_table_info_parser.mly"
                                       ([_1])
# 180 "sid_table_info_parser.ml"
               : 'nts_map_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'extract_subsystable_map) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'nts_map_list) in
    Obj.repr(
# 42 "sid_table_info_parser.mly"
                                        (_1 :: _2 )
# 188 "sid_table_info_parser.ml"
               : 'nts_map_list))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'extract_esid_sid_map) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'extract_sid_code_map) in
    Obj.repr(
# 45 "sid_table_info_parser.mly"
                                                                                                                                  (

  let sysname = _4 in
  let mp = {
    tr_sysname=sysname;
    esid_to_sid_map = _6;
    esid_to_statement_infos=_8;
  } 
  in
  (sysname,mp)

)
# 208 "sid_table_info_parser.ml"
               : 'extract_subsystable_map))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'esidlist) in
    Obj.repr(
# 60 "sid_table_info_parser.mly"
(
  let esidsidtbl = Hashtbl.create 97 in
  List.iter (fun s -> esid_sid_tbl_builder esidsidtbl s) _3;
  esidsidtbl
)
# 219 "sid_table_info_parser.ml"
               : 'extract_esid_sid_map))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'esidtosidrel) in
    Obj.repr(
# 67 "sid_table_info_parser.mly"
                        ([_1])
# 226 "sid_table_info_parser.ml"
               : 'esidlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'esidtosidrel) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'esidlist) in
    Obj.repr(
# 68 "sid_table_info_parser.mly"
                         (_1::_2)
# 234 "sid_table_info_parser.ml"
               : 'esidlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 71 "sid_table_info_parser.mly"
                                    ((Trace_types.ESID(_1), Trace_types.SID(_3)))
# 242 "sid_table_info_parser.ml"
               : 'esidtosidrel))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'sidtocodelist) in
    Obj.repr(
# 75 "sid_table_info_parser.mly"
  (
    let tbl = Hashtbl.create 97 in
    List.iter (fun s -> sid_to_code_tbl_builder tbl s) _3;
    tbl
  )
# 253 "sid_table_info_parser.ml"
               : 'extract_sid_code_map))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sidtocoderel) in
    Obj.repr(
# 83 "sid_table_info_parser.mly"
                             ( [_1] )
# 260 "sid_table_info_parser.ml"
               : 'sidtocodelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sidtocoderel) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sidtocodelist) in
    Obj.repr(
# 84 "sid_table_info_parser.mly"
                             (_1::_2)
# 268 "sid_table_info_parser.ml"
               : 'sidtocodelist))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "sid_table_info_parser.mly"
                                                    (
  let sid = _3 in
  let code = _6 in
  (Trace_types.SID(sid),code,None)						 
)
# 280 "sid_table_info_parser.ml"
               : 'sidtocoderel))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 7 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'get_posinfos) in
    Obj.repr(
# 92 "sid_table_info_parser.mly"
 (

  let sid = _3 in
  let code = _6 in
  let posinfos = _9 in
  (Trace_types.SID(sid),code,Some(posinfos))
 )
# 295 "sid_table_info_parser.ml"
               : 'sidtocoderel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'get_lexinfos) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'get_lexinfos) in
    Obj.repr(
# 103 "sid_table_info_parser.mly"
                                         ( (_1,_2) )
# 303 "sid_table_info_parser.ml"
               : 'get_posinfos))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 16 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 14 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 12 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 10 : int) in
    let _11 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _13 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _15 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _17 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 106 "sid_table_info_parser.mly"
                                                                                                                                            (
 
  if (String.compare "pos_fname" _3)<> 0 then 
     raise (ParseLexinfosexception("pos_fname = excepted, instead of",_3))
  else () ;
  if (String.compare "pos_lnum" _7) <> 0 then
     raise (ParseLexinfosexception("pos_lnum = excepted, instead of",_7))
  else () ;
  if (String.compare  "pos_bol" _11 ) <> 0 then
   raise (ParseLexinfosexception("pos_bol = excepted, instead of",_11))
  else () ;
  if ( String.compare "pos_cnum" _15 )<> 0 then
    raise  (ParseLexinfosexception("pos_bol = excepted, instead of",_15))
  else ();
  
  let filename = _5 in
  let begin_line_number = _9 in
  let begin_of_line = _13 in
  let col_number = _17 in
 
  {
    pos_fname = filename;
    pos_lnum = begin_line_number;
    pos_bol = begin_of_line ;
    pos_cnum = col_number;
  }
    
)
# 344 "sid_table_info_parser.ml"
               : 'get_lexinfos))
(* Entry mapextract *)
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
let mapextract (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Trace_types.tr_subsystem_table)
