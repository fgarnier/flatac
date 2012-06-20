(*Most of this material comes from Pierre Corbineau's work*)

open Format
open Lexing

let curr_lexbuf = ref ""

type loc = 
    {
      file: string;
      line: int;
      start: int;
      stop: int;
    }
      
let loc_of_pos buf =
  let spos = lexeme_start_p buf in
  let endp = lexeme_end buf in
  {
    file=spos.pos_fname;
    line=spos.pos_lnum;
    start=spos.pos_cnum-spos.pos_bol;
    stop=endp-spos.pos_bol
  }
    


let error loc err =
  let msg = sprintf "File %S, line %d, characters %d-%d:\n"
    loc.file loc.line loc.start loc.stop in
  msg
    
    

