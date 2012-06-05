%{
  open Lexing
  open Error
  open Nts_types
  open Nts_functor
  

  type varsort = Nat | Int | Real (**)

  module P =
  struct
    type t = string
    type anot_type = ()
    let make_anot () = ()
    let pprint s = s 
  end
    
  module Nts_int = Nts_functor.Make(P)
  open Nts_int
  
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
%token INPUTVARSLIST OUTPUTVARSLIST LOCALVARLIST

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
  List.iter 
}
| FINALSTATE ident_list SEMICOLON {}
| ERRORSTATE ident_list SEMICOLON {}

|  











%cautomata_vars : 
