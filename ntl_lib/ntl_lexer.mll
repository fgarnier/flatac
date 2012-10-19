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
  register_kwd "bool" BOOLDECL;;
  register_kwd "real" REALDECL;;
  register_kwd "initial" INITSTATE;;
  register_kwd "final" FINALSTATE;;
  register_kwd "error" ERRORSTATE;;
  register_kwd "in" INPUTVARLIST;;
  register_kwd "out" OUTPUTVARLIST;;
  register_kwd "true" BTRUE;;
  register_kwd "false" BFALSE;;
  register_kwd "havoc" HAVOC;;
  register_kwd "exists" EXISTS;;
  register_kwd "forall" FORALL;;
  register_kwd "imply" IMPLY;;
  register_kwd "equiv" EQUIV;;
  register_kwd "init" INIT;;
  register_kwd "instances" INSTANCES;; 
  
  
  

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
let floatval = ( number+ '.' number+)
let quote = ['\'']
let identifier = ( '_' | uletter | lletter)+ ( uletter | lletter | '_' | number )*
let dotted_identifier = (  '_' | uletter | lletter)+ ( '/' |'.' | '_' | uletter | lletter | number )+ (uletter | lletter |number)+

let primed_var = (identifier quote)
    
 rule token = parse
  | "/*" { comments 0 lexbuf }
  | ['\n'] {Lexing.new_line lexbuf; token lexbuf}
  | [' ' '\t' '\r' '\000'] {token lexbuf}
  | "*" {TIMES}
  | "." {DOT}
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
  | "[" {OBRACK}
  | "]" {CBRACK}
  | ";" {SEMICOLON}
  | "," {COMMA}
  | "<->" {LRARROW}
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
  | "//" {comment_line lexbuf}

  | intval {
    let num =  Big_int.big_int_of_string( 
    Lexing.lexeme lexbuf) in
    INT(num)
  }

  | floatval{
    let f=Scanf.sscanf (Lexing.lexeme lexbuf) "%f" (fun s-> s)  in
    FLOAT(f)
  }
  | identifier  { 
    KWD._KWD_or_IDENT (Lexing.lexeme lexbuf)
  }
  | dotted_identifier {
    DOTTEDIDENT((Lexing.lexeme lexbuf))
  }

  | primed_var {PRIMEDVAR ( Lexing.lexeme lexbuf )}
  | eof {EOF}
  | _ { 
    let error_msg = Lexing.lexeme lexbuf in 
    raise (UndefinedLexem(error_msg))
  }
 
 and comments level = parse
   | "*/" {
     if level = 0 then token lexbuf
     else comments (level-1) lexbuf
   }
   | "/*" {
     comments (level+1) lexbuf
   }
   | "\n" {
     Lexing.new_line lexbuf;comments level lexbuf
   }
   | _ {
     comments level lexbuf
   }
   | eof {
     begin
       Format.printf " EOF reached within unclosed comments %d \n" (level+1);
       exit 1
     end
   }

 and comment_line  = parse
   | ['\n'] {
     token lexbuf
   }
 
   | _ {
     comment_line lexbuf
   }




{

}
