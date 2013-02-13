{
  
  open Lexing
  open Sid_table_info_parser
  
  exception UndefinedLexem of string
  exception EofWhileInAComment 

      
  module KWD: sig val register_kwd : string -> token -> unit val _KWD_or_IDENT : string -> token end =
  struct
    let kwds = Hashtbl.create 17
    
    let register_kwd k sort = Hashtbl.add kwds k sort
      
    let _KWD_or_IDENT str = try Hashtbl.find kwds str with Not_found -> IDENT(str)
  end;;


  open KWD;;


  register_kwd "FUNCTION" FUNDECL;;
  register_kwd "ESID_TO_SID_MAP" DECLMAPESIDTOSID;;
  register_kwd "SID_TO_CODE_MAP" DECLARECODEMAP;;
  register_kwd "SOURCE_LEXINFOS" LEXINFOS;;
  register_kwd "POSINFOS" POSINFOS


  let new_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum }

}

let lletter = ['a' - 'z']
let uletter = ['A' - 'Z']
let number =  ['0' - '9']
let intval = '-'(number +) | (number +)
let identifier = ( '_' | uletter | lletter)+ ( uletter | lletter | '_' | number )*
let dotted_identifier = (  '_' | uletter | lletter)+ ( '/' |'.' | '_' | uletter | lletter | number )+ (uletter | lletter |number)+

    
  rule token = parse
  
  | ">>" {MAPESIDTOSID}
  | "{{" {OPENGROUP}
  | "}}" {CLOSEGROUP}
  | "@{{@" { annot "" lexbuf}

  | "@}}@" {CODEBLOCKCLOSE}

  | ";;" {ENDLINE}
  | ";" {SEMICOLON}
  | ":" {COLON}
  | "sid" {SID}
  | "=" {EQ}
  | "C-Code" {CCODE}
  | eof {EOF}



  | ['\n'] {Lexing.new_line lexbuf; token lexbuf}
  | [' ' '\t' '\r' '\000'] {token lexbuf}

  | intval { 
    let num =  int_of_string( 
    Lexing.lexeme lexbuf) in
    INT(num)
  }

  | identifier {
    let str = Lexing.lexeme lexbuf in
    _KWD_or_IDENT str 
  }

  | _ { 
    let error_msg = Lexing.lexeme lexbuf in 
    raise (UndefinedLexem(error_msg))
  }

and annot prefix = parse
    | "@}}@" {ANNOT(prefix)}
    | eof { raise EofWhileInAComment}
    | _ {
      let suffix=prefix^(Lexing.lexeme lexbuf) in
      annot suffix lexbuf
    }
      
{}
