{
  
  open Ntl_parser
  open Lexing
  

  exception UndefinedLexem of string

  module KWD: sig val register_kwd : string -> token -> unit val _KWD_or_IDENT : string -> token end =
  struct
  let kwds = Hashtbl.create 17
    
  let register_kwd k sort = Hashtbl.add kwds k sort
    
  let _KWD_or_IDENT str = try Hashtbl.find kwds str with Not_found -> IDENT(str)
  end;;
  
  open KWD;;
  register_kwd "nts" NTSDECL;;
  register_kwd "int" INTDECL;;
  register_kwd "nat" NATDECL;;
  register_kwd "real" REALDECL;;
  register_kwd "initial" INITSTATE;;
  register_kwd "final" FINALSTATE;;
  register_kwd "error" ERRORSTATE;;
  register_kwd "in" INPUTVARLIST;;
  register_kwd "out" OUTPUTVARLIST;;
  register_kwd "true" BTRUE;;
  register_kwd "false" BFALSE;;
  register_kwd "havoc" HAVOC;;
 
  
  

  let new_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum }
      
}


let lletter = ['a' - 'z']
let uletter = ['A' - 'Z']
let number =  ['0' - '9']
let intval = number +
let quote = ['\'']
let identifier = ( '_' | uletter | lletter)+ ( uletter | lletter | '_' | number )*
let primed_var = (identifier quote)
    
 rule token = parse
  | ['\n'] {Lexing.new_line lexbuf; token lexbuf}
  | [' ' '\t' '\r' '\000'] {token lexbuf}
  | "*" {TIMES}
  | "+" {PLUS}
  | "-" {MINUS}
  | "/" {DIV}
  | "%" {MOD}
  | "<" {LT}
  | ">" {GT}
  | "<=" {LEQ}
  | ">=" {GEQ}
  | "(" {LBRACE}
  | ")" {RBRACE}
  | "{" {LBRACK}
  | "}" {RBRACK}
  | ":" {COLON}
  | ";" {SEMICOLON}
  | "," {COMMA}
  | "->" {ARROW}
  | "=" {EQ}
  | "!=" {NEQ}
  | "'" {PRIME}
  | "and" {BAND}
  | "&&" {BAND}
  | "or" {BOR}
  | "||" {BOR}
  | "not" {BNOT}
  | "!" {BNOT}
  | number {
    let num =  int_of_string( 
    Lexing.lexeme lexbuf) in
    INT(num)
  }
  | identifier  { 
    KWD._KWD_or_IDENT (Lexing.lexeme lexbuf)
  }
  | primed_var {PRIMEDVAR ( Lexing.lexeme lexbuf )}
  | eof {EOF}
  | _ { 
    let error_msg = Lexing.lexeme lexbuf in 
    raise (UndefinedLexem(error_msg))
  }


{

}
