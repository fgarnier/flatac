%{
  open Lexing
  open Nts_types
  open Nts_functor
  open Ntsint.Nts_int
  open Parsing_error 
  

  exception UnBoundVariable of string * Parsing_error.loc option
  type varsort = Nat | Int | Real 

  open Ntsint (* This module contains the definition of the moduel
	      Nts_int, which is an "instance of the functor 
		 Nts_functor"*)
  



module Parse_machine  
    = 
  struct
    let ntsinstance = (Ntsint.Nts_int.create_nts_system "")
    let current_cautomaton = ref (Ntsint.Nts_int.create_nts_automaton "")
  end

  (* *)
  let add_input_vars_iterator (vsort : varsort) (c : nts_automaton) s =
    match vsort with 
	Int ->
	  c.input_vars <- (c.input_vars@(NtsIVar(s)::[]))
      | Real -> 
	  c.input_vars <- (c.input_vars@(NtsRVar(s)::[]))
      (* One need to define Nat implementation*)
  
  let add_output_vars_iterator (vsort : varsort) (c : nts_automaton) s =
    match vsort with 
	Int ->
	  c.output_vars <- c.input_vars@(NtsIVar(s)::[])
      | Real -> 
	c.output_vars <- c.output_vars@(NtsRVar(s)::[])

  let add_local_vars_iterator (vsort : varsort) (c : nts_automaton) s =
    match vsort with 
	Int ->
	  c.local_vars <- c.input_vars@(NtsIVar(s)::[])
      | Real -> 
	c.local_vars <- c.local_vars@(NtsRVar(s)::[])


  let get_varname_of_primedvarname pvname =
    String.sub pvname 0 ((String.length pvname)-1)



  let get_vinfo vname =
    let vinfo = Nts_int.get_varinfo Parse_machine.ntsinstance (Some(!Parse_machine.current_cautomaton.nts_automata_name)) vname in
    match vinfo with
      None -> 
	begin
	  (*raise (UnBoundVariable (vname, (loc_of_pos lexbuf) ))*)
	  raise (UnBoundVariable (vname, None ))
	end
      | Some(v) ->
	v (* The nts var is here*)

	  

%}


%token <int>  INT
%token <string> IDENT
%token <float> REAL
%token <string> PRIMEDVAR 
%type <Ntsint.Nts_int.nts_system> ntldescr 
%type <Nts_types.cnt_arithm_exp> arithm_expr

%token TIMES PLUS MINUS UMINUS DIV MOD LT GT MOD LEQ GEQ EQUAL LBRACE
%token RBRACE LBRACK RBRACK COLON SEMICOLON COMMA ARROW
%token  PRIME BTRUE BFALSE BAND BOR BNOT EOF
%token NTSDECL INTDECL NATDECL REALDECL INITSTATE FINALSTATE ERRORSTATE
%token INPUTVARLIST OUTPUTVARLIST LOCALVARLIST HAVOC

%nonassoc AFFRULERED
%nonassoc PRIMEDEXPR EQ LBRACK RBRACK
%nonassoc UMINUS 
%nonassoc NTS_TRANS


%left BOR
%left PLUS MINUS 
%left TIMES DIV MOD BAND
%right BNOT 
%start ntldescr
%%



ntldescr : NTSDECL IDENT SEMICOLON decl { 
  Nts_int.rename_nts_system Parse_machine.ntsinstance   $2; 
  add_cautomata_to_nts !Parse_machine.current_cautomaton Parse_machine.ntsinstance;
  Format.printf "Parsing automaton %s \n" $2;  Parse_machine.ntsinstance }
;

ident_list : IDENT {[$1]}
| IDENT COMMA ident_list {$1::$3}
;

gvars_decl : ident_list INTDECL SEMICOLON { Nts_int.add_nts_int_vars_to_nts_system Parse_machine.ntsinstance $1}
| ident_list REALDECL SEMICOLON { Nts_int.add_nts_real_vars_to_nts_system Parse_machine.ntsinstance $1 } 
;

decl :  gvars_decl  {}
| IDENT LBRACK cautomaton_decl_sequence RBRACK decl { 
  Parse_machine.current_cautomaton :=  ( Nts_int.create_nts_automaton $1);
  Nts_int.add_cautomata_to_nts  !Parse_machine.current_cautomaton Parse_machine.ntsinstance
}
| EOF {}
;


cautomaton_decl_sequence : cautomaton_decl cautomaton_decl_sequence {}
| cautomaton_decl {}

cautomaton_decl :
 INPUTVARLIST ident_list COLON INTDECL SEMICOLON {
  List.iter (  add_input_vars_iterator Int !Parse_machine.current_cautomaton) $2  }
| INPUTVARLIST ident_list COLON REALDECL SEMICOLON {
  List.iter (  add_input_vars_iterator Real !Parse_machine.current_cautomaton) $2  }
| OUTPUTVARLIST ident_list COLON INTDECL SEMICOLON {
  List.iter (  add_output_vars_iterator Int !Parse_machine.current_cautomaton) $2  }
| OUTPUTVARLIST ident_list COLON REALDECL SEMICOLON {
  List.iter (  add_output_vars_iterator Real !Parse_machine.current_cautomaton) $2  }
| ident_list COLON INTDECL SEMICOLON {
  List.iter (  add_local_vars_iterator Int !Parse_machine.current_cautomaton) $1  }
| ident_list COLON REALDECL SEMICOLON {
  List.iter (  add_local_vars_iterator Real !Parse_machine.current_cautomaton) $1  }

| INITSTATE ident_list SEMICOLON  {
  List.iter ( fun s -> 
		add_init_state !Parse_machine.current_cautomaton (Nts_int.control_of_id_param s)  
	    ) $2
  }

| FINALSTATE ident_list SEMICOLON {
    List.iter ( fun s -> 
		  add_final_state !Parse_machine.current_cautomaton (Nts_int.control_of_id_param s)  
	      ) $2

}
| ERRORSTATE ident_list SEMICOLON {
    List.iter ( fun s -> 
		  add_final_state !Parse_machine.current_cautomaton (Nts_int.control_of_id_param s)  
	      ) $2
}

| IDENT ARROW IDENT LBRACK nts_trans_split RBRACK {
  let control_org = control_of_id_param $1 in
  let control_dest= control_of_id_param $3 in
  let transit = $5 in
  Nts_int.add_transition !Parse_machine.current_cautomaton control_org control_dest transit
}  

| IDENT ARROW IDENT LBRACK  RBRACK {
  let control_org = control_of_id_param $1 in
  let control_dest= control_of_id_param $3 in
  let transit = [] in
  Nts_int.add_transition !Parse_machine.current_cautomaton control_org control_dest transit
}  

;


nts_trans_split : nts_trans BAND nts_trans_split { $1 :: $3}
| nts_trans {[$1]}


nts_trans :  pressburg_bool %prec NTS_TRANS {CntGuard ( $1 )}
| affect {$1}
| havocise {$1}
| callaffect {$1}



primed_var_list : primed_express COMMA primed_var_list {$1::$3}
| primed_express {[$1]}

callaffect : primed_var_list EQ IDENT LBRACE arithm_expr_list RBRACE
{CntCall($3,Some($1),$5)}
| IDENT LBRACE arithm_expr_list RBRACE  {CntCall($1,None,$3)}

pressburg_bool : BTRUE { CntBTrue }
| BFALSE {CntBFalse}
| pressburg_bool BOR pressburg_bool  {CntBOr ( $1 , $3)}
| pressburg_bool BAND pressburg_bool {CntBAnd ( $1 , $3)}
| BNOT pressburg_bool {CntNot($2)}
| arithm_expr GT arithm_expr {CntBool(CntGt,$1,$3)}
| arithm_expr LT arithm_expr {CntBool(CntLt,$1,$3)}
| arithm_expr GEQ arithm_expr {CntBool(CntGeq,$1,$3)}
| arithm_expr LEQ arithm_expr {CntBool(CntLeq,$1,$3)}
| arithm_expr EQ arithm_expr {CntBool(CntEq,$1,$3)}
;

primed_express : PRIMEDVAR %prec PRIMEDEXPR { 
  let varname = get_varname_of_primedvarname $1 in
  let vinfo = Nts_int.get_varinfo Parse_machine.ntsinstance (Some((!Parse_machine.current_cautomaton).nts_automata_name)) varname in
  match vinfo with
      Some(v) -> v
    | None -> raise (UnboundVarName ( varname ))
}
;

 
arithm_expr_list : arithm_expr {$1::[]}
| arithm_expr COMMA arithm_expr_list {$1::$3} 

arithm_expr : INT { let  cst = Big_int.big_int_of_int $1 in 
		   CntCst(cst)}

| IDENT { let vname = $1 in
	  let vinfo = Nts_int.get_varinfo Parse_machine.ntsinstance 
	    (Some(!Parse_machine.current_cautomaton).nts_automata_name) 
	    vname in
	  let var =
	    match vinfo with
		Some(v) -> v
	      | None ->  raise (UnboundVarName ( vname ))
	  in
	  CntVar(var)
	}
| LBRACE arithm_expr RBRACE {$2}
| MINUS arithm_expr %prec UMINUS { CntUnMin($2) }
| arithm_expr PLUS arithm_expr { CntSum($1,$3) }
| arithm_expr MINUS arithm_expr { CntMinus($1,$3) }
| arithm_expr DIV arithm_expr { CntDiv($1,$3) }
| arithm_expr MOD arithm_expr { CntMod($1,$3) }
| arithm_expr TIMES arithm_expr { CntProd($1,$3) }
;


havocise : HAVOC LBRACE ident_list RBRACE {
  let ntvarlist = List.map get_vinfo $3 in 
  CntHavoc(ntvarlist)
}
;

affect : PRIMEDVAR EQ arithm_expr %prec AFFRULERED {
  let vname = get_varname_of_primedvarname $1 in
  let vinfo = get_vinfo vname in
  CntAffect(vinfo,$3)
}
;




