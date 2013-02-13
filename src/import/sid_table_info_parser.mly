%{
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
      
%}


%type <Trace_types.tr_subsystem_table> mapextract 
%token <int> INT
%token <string> IDENT
%token <string> ANNOT
%token MAPESIDTOSID OPENGROUP CLOSEGROUP CODEBLOCKOPEN CODEBLOCKCLOSE
%token ENDLINE EQ EOF CCODE SID COLON SEMICOLON FUNDECL DECLMAPESIDTOSID
%token DECLARECODEMAP LEXINFOS POSINFOS
%start mapextract
%%


mapextract : nts_map_list EOF {
  let tbl = Hashtbl.create 97 in
  List.iter ( tr_subsystem_builder_iterator tbl ) $1;
  tbl
    
};


nts_map_list : extract_subsystable_map {[$1]}
 | extract_subsystable_map nts_map_list {$1 :: $2 };


extract_subsystable_map : OPENGROUP FUNDECL EQ IDENT ENDLINE extract_esid_sid_map ENDLINE extract_sid_code_map ENDLINE CLOSEGROUP {

  let sysname = $4 in
  let mp = {
    tr_sysname=sysname;
    esid_to_sid_map = $6;
    esid_to_statement_infos=$8;
  } 
  in
  (sysname,mp)

};


extract_esid_sid_map : DECLMAPESIDTOSID OPENGROUP esidlist CLOSEGROUP 
{
  let esidsidtbl = Hashtbl.create 97 in
  List.iter (fun s -> esid_sid_tbl_builder esidsidtbl s) $3;
  esidsidtbl
};


esidlist : esidtosidrel {[$1]}
| esidtosidrel  esidlist {$1::$2};


esidtosidrel : INT MAPESIDTOSID INT {(Trace_types.ESID($1), Trace_types.SID($3))};


extract_sid_code_map : DECLARECODEMAP OPENGROUP sidtocodelist CLOSEGROUP 
  {
    let tbl = Hashtbl.create 97 in
    List.iter (fun s -> sid_to_code_tbl_builder tbl s) $3;
    tbl
  };



sidtocodelist : sidtocoderel { [$1] }
| sidtocoderel sidtocodelist {$1::$2};

sidtocoderel :  SID COLON INT SEMICOLON CCODE ANNOT {
  let sid = $3 in
  let code = $6 in
  (Trace_types.SID(sid),code,None)						 
}
|  SID COLON INT SEMICOLON CCODE ANNOT POSINFOS OPENGROUP get_posinfos CLOSEGROUP
 {

  let sid = $3 in
  let code = $6 in
  let posinfos = $9 in
  (Trace_types.SID(sid),code,Some(posinfos))
 }

;


get_posinfos : get_lexinfos get_lexinfos { ($1,$2) };


get_lexinfos : LEXINFOS OPENGROUP IDENT EQ IDENT SEMICOLON IDENT EQ INT SEMICOLON IDENT EQ INT SEMICOLON IDENT EQ INT SEMICOLON  CLOSEGROUP {
 
  if (String.compare "pos_fname" $3)<> 0 then 
     raise (ParseLexinfosexception("pos_fname = excepted, instead of",$3))
  else () ;
  if (String.compare "pos_lnum" $7) <> 0 then
     raise (ParseLexinfosexception("pos_lnum = excepted, instead of",$7))
  else () ;
  if (String.compare  "pos_bol" $11 ) <> 0 then
   raise (ParseLexinfosexception("pos_bol = excepted, instead of",$11))
  else () ;
  if ( String.compare "pos_cnum" $15 )<> 0 then
    raise  (ParseLexinfosexception("pos_bol = excepted, instead of",$15))
  else ();
  
  let filename = $5 in
  let begin_line_number = $9 in
  let begin_of_line = $13 in
  let col_number = $17 in
 
  {
    pos_fname = filename;
    pos_lnum = begin_line_number;
    pos_bol = begin_of_line ;
    pos_cnum = col_number;
  }
    
}
