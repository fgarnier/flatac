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
  
  let ntsinstance = Nts_int.create_nts ();;
  let current_cautomata = (ref Nts_int.create_nts_cautomata ());;

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

%}



%token <int>  INT
%token <string> IDENT
%token <float> REAL 
%token TIMES PLUS MINUS DIV MOD LT GT MOD LEQ GEQ LBRACE
%token RBRACE LBRACK RBRACK COLON SEMICOLON COMMA ARROW
%token EQUAL PRIME BAND BOR BNOT EOF
%token NTSDECL INTDECL NATDECL REALDECL INITSTATE FINALSTATE ERRORSTATE
%token INPUTVARSLIST OUTPUTVARSLIST LOCALVARLIST PRIMEDVAR

%type 


%start ntldescr



%ntldescr : IDENT hsystemname COLON decl {  Nts_int.rename_nts_cautomaton  $2 } 



%ident_list : IDENT COLON {[$1]}
| IDENT COMMA ident_list {$1::$3}

%gvars_decl : ident_list INTDECL SEMICOLON { Nts_int.add_nts_int_vars_to_nts_system $1}
| ident_list REALDECL SEMICOLON { Nts_int.add_nts_real_vars_to_nts_system $1 } 


%decl :  gvars_decl decl {}
| INDENT LBRACK cautomaton_decl RBRACK decl { 
  current_cautomata := ref (Nts_int.create_nts_cautomata ()) in
  rename_nts_automaton !current_cautomaton $1;
  Nts_int.add_cautomata_to_nts nts_instance !current_cautomaton 
}
| EOF {}



%cautomaton_decl : INPUTVARLIST ident_list INTDECL SEMICOLON 
{Nts_int.add_inputvar_left !current_instance $2  }
| INPUTVARLIST ident_list INTDECL SEMICOLON {
  List.iter (  add_input_var_iterator Int !current_instance) $2  }
| INPUTVARLIST ident_list REALDECL SEMICOLON {
  List.iter (  add_input_var_iterator REAL !current_instance) $2  }
| OUTPUTVARLIST ident_list INTDECL SEMICOLON {
  List.iter (  add_output_var_iterator Int !current_instance) $2  }
| OUTPUTVARLIST ident_list REALDECL SEMICOLON {
  List.iter (  add_output_var_iterator REAL !current_instance) $2  }
| LOCALVARLIST ident_list INTDECL SEMICOLON {
  List.iter (  add_local_var_iterator Int !current_instance) $2  }
| LOCALVARLIST ident_list REALDECL SEMICOLON {
  List.iter (  add_local_var_iterator REAL !current_instance) $2  }

| INITSTATE ident_list SEMICOLON  {
  List.iter ( fun s -> 
		add_init_state !current_instance (Nts_int.control_of_id s)  
	    ) $2
  }

| FINALSTATE ident_list SEMICOLON {
    List.iter ( fun s -> 
		  add_final_state !current_instance (Nts_int.control_of_id s)  
	      ) $2

}
| ERRORSTATE ident_list SEMICOLON {
    List.iter ( fun s -> 
		  add_final_state !current_instance (Nts_int.control_of_id s)  
	      ) $2
}

| IDENT ARROW IDENT LBRACK nts_transit RBRACK {
  let control_org = control_of $1 in
  let control_dest= control_of $2 in
  let transit = $3 in
  Nts_int.add_transition !current_instance control_org control_dest transit
}  




%nts_ntrans_split : trans_elem { [$1] }
| trans_elem BAND trans



%pressburg_bool : BTRUE
| BFALSE
| pressburg_bool BOR pressburg_bool
| pressburg_bool BAND pressburg_bool




%rel_decl : 

%arithm_expr : INT { let  cst = My_bigint.of_string $1 in 
		   CntCst(cst)}
| IDENT { let vname = $i in
	  let vinfo = Nts_int.get_var_info nts_instance Some((!current_instance).name) vname in
	  match 
	    None -> (raise UnBoundVarName (vname, lexbuf.lex_curr_p ))
	    
	    | Some(v) ->
	      v (* The nts var is here*)
	}
| arithm_expr PLUS arithm_expr { CntSum($1,$3)}
| arithm_expr MINUS arithm_expr {CntMinus($1,$3)}
| UNMIN arithm_expr {CntUnMin($2)}
| arithm_expr DIV arithm_expr {CntDiv($1,$3)}
| arithm_expr MOD arithm_expr {CntMod($1,$3)}
| arithm_expr TIMES arithm_expr {CntProd($1,$3)}








