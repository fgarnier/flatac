%{
  open Lexing
  open Error
  open Nts_types
  open Nts_functor
  

  exception UnBoundVariable of string * Lexing.position
  type varsort = Nat | Int | Real 

  module P =
  struct
    type t = string
    type anot_type = ()
    let make_anot () = ()
    let pprint s = s 
  end
    
  module Nts_int = Nts_functor.Make(P)
  (*open Nts_int*)
  
  module Parse_machine = struct
    let ntsinstance = Nts_int.create_nts ()
    let current_cautomaton = (ref Nts_int.create_nts_cautomata ())
  end

  (* *)
  let add_input_vars_iterator (vsort : varsort) (c : Nts_int.nts_automaton) s =
    match varsort with 
	Int ->
	  c.input_vars <- c.input_vars@(NtsIVar(s))
      | Real -> 
	c.intput_vars@(NtsRVar(s))
      (* One need to define Nat implementation*)
  
  let add_output_vars_iterator (vsort : varsort) (c : Nts_int.nts_automaton) s =
    match varsort with 
	Int ->
	  c.output_vars <- c.input_vars@(NtsIVar(s))
      | Real -> 
	c.output_vars@(NtsRVar(s))

  let add_local_vars_iterator (vsort : varsort) (c : Nts_int.nts_automaton) s =
    match varsort with 
	Int ->
	  c.local_vars <- c.input_vars@(NtsIVar(s))
      | Real -> 
	c.local_vars@(NtsRVar(s))


  let get_varname_of_primedvarname pvname =
    String.sub pvname 0 ((String.length pvname)-1)

  let get_vinfo vname =
    let vinfo = Nts_int.get_var_info Parse_machine.nts_instance Some((!Parse_machine.current_instance).name) vname in
    match vinfo
      None -> (raise UnBoundVarName (vname, lexbuf.lex_curr_p ))
    
      | Some(v) ->
	v (* The nts var is here*)

  

%}


%token <int>  INT
%token <string> IDENT
%token <float> REAL
%token <string> PRIMEDVAR 
%type <Nts_int.nts_system> ntldescr 
%token TIMES PLUS MINUS UMINUS DIV MOD LT GT MOD LEQ GEQ EQUAL LBRACE
%token RBRACE LBRACK RBRACK COLON SEMICOLON COMMA ARROW
%token  PRIME BTRUE BFALSE BAND BOR BNOT EOF
%token NTSDECL INTDECL NATDECL REALDECL INITSTATE FINALSTATE ERRORSTATE
%token INPUTVARLIST OUTPUTVARLIST LOCALVARLIST HAVOC

%nonassoc AFFRULERED
%nonassoc EQ LBRACK RBRACK
%nonassoc UMINUS 

%left BOR
%left PLUS MINUS 
%left TIMES DIV MOD BAND 
%start ntldescr
%%




ntldescr : NTSDECL IDENT COLON decl {  Nts_int.rename_nts_cautomaton  $2 }
;


ident_list : IDENT COLON {[$1]}
| IDENT COMMA ident_list {$1::$3}
;

gvars_decl : ident_list INTDECL SEMICOLON { Nts_int.add_nts_int_vars_to_nts_system $1}
| ident_list REALDECL SEMICOLON { Nts_int.add_nts_real_vars_to_nts_system $1 } 
;


decl :  gvars_decl  {}
| IDENT LBRACK cautomaton_decl RBRACK decl { 
  Parse_machine.current_cautomata := ref (Nts_int.create_nts_cautomata ()) in
  rename_nts_automaton !Parse_machine.current_cautomaton $1;
  Nts_int.add_cautomata_to_nts nts_instance !Parse_machine.current_cautomaton 
}
| EOF {}
;

cautomaton_decl :
 INPUTVARLIST ident_list INTDECL SEMICOLON {
  List.iter (  add_input_var_iterator Int !Parse_machine.current_instance) $2  }
| INPUTVARLIST ident_list REALDECL SEMICOLON {
  List.iter (  add_input_var_iterator REAL !Parse_machine.current_instance) $2  }
| OUTPUTVARLIST ident_list INTDECL SEMICOLON {
  List.iter (  add_output_var_iterator Int !Parse_machine.current_instance) $2  }
| OUTPUTVARLIST ident_list REALDECL SEMICOLON {
  List.iter (  add_output_var_iterator REAL !Parse_machine.current_instance) $2  }
| LOCALVARLIST ident_list INTDECL SEMICOLON {
  List.iter (  add_local_var_iterator Int !Parse_machine.current_instance) $2  }
| LOCALVARLIST ident_list REALDECL SEMICOLON {
  List.iter (  add_local_var_iterator REAL !Parse_machine.current_instance) $2  }

| INITSTATE ident_list SEMICOLON  {
  List.iter ( fun s -> 
		add_init_state !Parse_machine.current_instance (Nts_int.control_of_id s)  
	    ) $2
  }

| FINALSTATE ident_list SEMICOLON {
    List.iter ( fun s -> 
		  add_final_state !Parse_machine.current_instance (Nts_int.control_of_id s)  
	      ) $2

}
| ERRORSTATE ident_list SEMICOLON {
    List.iter ( fun s -> 
		  add_final_state !Parse_machine.current_instance (Nts_int.control_of_id s)  
	      ) $2
}

| IDENT ARROW IDENT LBRACK nts_trans_split RBRACK {
  let control_org = control_of $1 in
  let control_dest= control_of $3 in
  let transit = $3 in
  Nts_int.add_transition !Parse_machine.current_instance control_org control_dest transit
}  
;



nts_trans_split : nts_trans BAND nts_trans_split { $1 :: $3}
| nts_trans {[$1]}


nts_trans :  pressburg_bool {CntGuard ( $1 )}
| affect {$1}
| havocise {$1)}
| callaffect {$1}



primed_var_list : primed_express COMMA primed_var_list {$1::$3}
| primed_express {[$1]}

callaffect : primed_var_list EQ IDENT LBRACE arithm_expr_list RBRACE
{CntCall($3,Some($1),$5)}
| IDENT LBRACE arithm_expr RBRACE  {CntCall($1,None,$3)}

pressburg_bool : BTRUE { CntBTrue }
| BFALSE {CntBFalse}
| pressburg_bool BOR pressburg_bool  {CntBOr ( $1 , $3)}
| pressburg_bool BAND pressburg_bool {CntBAnd ( $1 , $3)}
| BNOT pressburg_bool {CntBNot($2)}
| arithm_expr GT arithm_expr {CntBool(CntGt,$1,$3)}
| arithm_expr LT arithm_expr {CntBool(CntLt,$1,$3)}
| arithm_expr GEQ arithm_expr {CntBool(CntGeq,$1,$3)}
| arithm_expr LEQ arithm_expr {CntBool(CntLeq,$1,$3)}
| arithm_expr EQ arithm_expr {CntBool(CntEq,$1,$3)}
;

primed_express : PRIMEDVAR { 
  let varname = get_varname_of_primedvarname $1 in
  let vinfo = Nts_int.get_var_info Parse_machine.nts_instance Some((!Parse_machine.current_instance).name) varname in
  vinfo
}
;

 
arithm_expr_list : arithm_expr {$1}
| arithm_expr COMMA arithm_expr_list {$1::$3} 

arithm_expr : INT { let  cst = My_bigint.of_string $1 in 
		   CntCst(cst)}

| IDENT { let vname = $i in
	  get_vinfo vname
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
  let ntvarlist = List.map getvinfo $3 in 
  CntHavoc(ntvarlist)
}
;

affect : PRIMEDVAR EQ arithm_expr %prec AFFRULERED {
  let vname = get_varname_of_primedvarname $1 in
  let vinfo = get_vinfo vname in
  CntAffect(vinfo,$3)
}
;




