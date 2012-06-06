{
  
  open Parser
  open Lexing
  
  module KWD: sig val register_kwd : string -> token -> unit val _KWD_or_LIDENT : string -> token end =
  struct
  let kwds = Hashtbl.create 17
    
  let register_kwd = Hashtbl.add kwds
    
  let _KWD_or_LIDENT str = try Hashtbl.find kwds str with Not_found -> LIDENT str
  end;;
  
  open KWD;;
  register_kwd "nts" NTSDECL;;
  register_kwd "int" INTDECL;;
  register_kwd "nat" NATDECL;;
  register_kwd "real" REALDECL;;
  register_kwd "init" INITSTATE;;
  register_kwd "final" FINALSTATE;;
  register_kwd "error" ERRORSTATE;;
  register_kwd "in" INPUTVARSLIST;;
  register_kwd "out" OUTPUTVARSLIST;;
  register_kwd "true" BTRUE;;
  register_kwd "false" BFALSE;;
  register_kwd "havoc" HAVOC
  (* register_kwd "not" BOP_NOT;;
     register_kwd "and" BOP_AND;;
     register_kwd "or" BOP_OR;;
  register_kwd "&&" BOP_AND;;
     register_kwd "||" BOP_OR;;
     register_kwd "!" BOP_NOT;;
  *)
  

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
let quote = ["'"]
let identifier = uletter ( uletter | '_' | number )*
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
  | "=" {EQUAL}
  | "'" {PRIME}
  | "and" {BAND}
  | "&&" {BAND}
  | "or" {BOR}
  | "||" {BOR}
  | "not" {BNOT}
  | "!" {BNOT}
  | primed_var {PRIMEDVAR}
  | eof {EOF}



{

}
