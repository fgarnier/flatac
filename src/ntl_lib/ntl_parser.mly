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
  let add_input_vars_iterator (vsort : varsort) (c : nts_automaton ref) s =
    match vsort with 
	Int ->
	  (!c).input_vars <- ((!c).input_vars@(NtsIVar(s)::[]))
      | Real -> 
	  (!c).input_vars <- ((!c).input_vars@(NtsRVar(s)::[]))
      (* One need to define Nat implementation*)
  
  let add_output_vars_iterator (vsort : varsort) (c : nts_automaton ref) s =
    match vsort with 
	Int ->
	  (!c).output_vars <- (!c).output_vars@(NtsIVar(s)::[])
      | Real -> 
	(!c).output_vars <- (!c).output_vars@(NtsRVar(s)::[])

  let add_local_vars_iterator (vsort : varsort) (c : nts_automaton ref) s =
    match vsort with 
	Int ->
	  (!c).local_vars <- (!c).local_vars@(NtsIVar(s)::[])
      | Real -> 
	(!c).local_vars <- (!c).local_vars@(NtsRVar(s)::[])


  let get_varname_of_primedvarname pvname =
    String.sub pvname 0 ((String.length pvname)-1)



  let get_vinfo vname =
    let vinfo = Nts_int.get_varinfo_by_optcautomaton Parse_machine.ntsinstance (Some(!Parse_machine.current_cautomaton)) vname in
    match vinfo with
      None -> 
	begin
	  (*raise (UnBoundVariable (vname, (loc_of_pos lexbuf) ))*)
	  Format.printf ("Current context cautomata has the following variables \n");
	  Format.printf ("Input vars : %s \n") (pprint_inputvars !Parse_machine.current_cautomaton );
	  Format.printf ("Output vars : %s \n") (pprint_outputvars !Parse_machine.current_cautomaton );
	  Format.printf ("Local vars : %s \n") (pprint_localvars !Parse_machine.current_cautomaton  );
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

%token TIMES PLUS MINUS UMINUS DIV MOD LT GT MOD EQ LEQ GEQ LBRACE
%token RBRACE LBRACK RBRACK COLON SEMICOLON COMMA ARROW
%token  PRIME BTRUE BFALSE BAND BOR BNOT EOF
%token NTSDECL INTDECL NATDECL REALDECL INITSTATE FINALSTATE ERRORSTATE
%token INPUTVARLIST OUTPUTVARLIST LOCALVARLIST HAVOC

%nonassoc PRIMEVARLIST 
%nonassoc PRIMEDEXPR EQ LBRACK RBRACK
%nonassoc PRESSEVAL

%nonassoc UMINUS 



%left BOR
%left PLUS MINUS 
%left TIMES DIV MOD BAND
%right BNOT 

%nonassoc NTS_TRANS
%start ntldescr
%%



ntldescr : NTSDECL IDENT SEMICOLON decl_sequence { 
  Nts_int.rename_nts_system Parse_machine.ntsinstance   $2; 
 (* add_cautomata_to_nts !Parse_machine.current_cautomaton Parse_machine.ntsinstance;*)
  Format.printf "Parsing automaton %s \n" $2;  Parse_machine.ntsinstance }
;

ident_list : IDENT {[$1]}
| IDENT COMMA ident_list {$1::$3}
;

gvars_decl : ident_list INTDECL SEMICOLON { Nts_int.add_nts_int_vars_to_nts_system Parse_machine.ntsinstance $1}
| ident_list REALDECL SEMICOLON { Nts_int.add_nts_real_vars_to_nts_system Parse_machine.ntsinstance $1 } 
;

decl_sequence : decl_automata decl_sequence {}
| decl_automata {}
| EOF {}
;


decl_automata :  IDENT LBRACK cautomaton_decl RBRACK { 
  Parse_machine.current_cautomaton :=  ( Nts_int.create_nts_automaton $1);
  Format.printf "!!!!!##### Creating a new counter automaton %s \n" $1;
  Nts_int.add_cautomata_to_nts  Parse_machine.current_cautomaton Parse_machine.ntsinstance;
  Format.printf "DEBUG :%s\n" (pprint_to_nts !Parse_machine.current_cautomaton);
 Format.printf "Debug : Transitions in Parsem_machine.current_cautomaton  are :%s \n" (Nts_int.pprint_transitions "" !Parse_machine.current_cautomaton )
}

;


cautomaton_decl : cautomaton_decl_sections cautomaton_decl {}
| cautomaton_decl_sections {Format.printf "End of an automaton declaration \n"}

cautomaton_decl_sections :
 INPUTVARLIST ident_list COLON INTDECL SEMICOLON {
  List.iter (  add_input_vars_iterator Int Parse_machine.current_cautomaton) $2 ; Format.printf "Adding inpuvariables : %s \n" (pprint_inputvars !Parse_machine.current_cautomaton) }
| INPUTVARLIST ident_list COLON REALDECL SEMICOLON {
  List.iter (  add_input_vars_iterator Real Parse_machine.current_cautomaton) $2  }
| OUTPUTVARLIST ident_list COLON INTDECL SEMICOLON {
  List.iter (  add_output_vars_iterator Int Parse_machine.current_cautomaton) $2  }
| OUTPUTVARLIST ident_list COLON REALDECL SEMICOLON {
  List.iter (  add_output_vars_iterator Real Parse_machine.current_cautomaton) $2  }
| ident_list COLON INTDECL SEMICOLON {
  List.iter (  add_local_vars_iterator Int Parse_machine.current_cautomaton) $1  }
| ident_list COLON REALDECL SEMICOLON {
  List.iter (  add_local_vars_iterator Real Parse_machine.current_cautomaton) $1  }

| INITSTATE ident_list SEMICOLON  {
  List.iter ( fun s -> 
		add_init_state Parse_machine.current_cautomaton (Nts_int.control_of_id_param s)  
	    ) $2;

  Format.printf "Adds an initial state \n"
  }

| FINALSTATE ident_list SEMICOLON {
    List.iter ( fun s -> 
		  add_final_state Parse_machine.current_cautomaton (Nts_int.control_of_id_param s)  
	      ) $2;
  Format.printf "Adds a final state \n"

}
| ERRORSTATE ident_list SEMICOLON {
    List.iter ( fun s -> 
		  add_final_state Parse_machine.current_cautomaton (Nts_int.control_of_id_param s)  
	      ) $2;

    Format.printf "Adds an error state \n"
}

| IDENT ARROW IDENT LBRACK nts_trans_split RBRACK {
  let control_org = control_of_id_param $1 in
  let control_dest= control_of_id_param $3 in
  let transit = $5 in
  Nts_int.add_transition Parse_machine.current_cautomaton control_org control_dest transit; Format.printf "Adding a transition  "
}  


/* | IDENT  ARROW IDENT LBRACK nts_trans RBRACK {
  let control_org = control_of_id_param $1 in
  let control_dest= control_of_id_param $3 in
  let transit = ($5::[]) in
  Nts_int.add_transition Parse_machine.current_cautomaton control_org control_dest transit
}*/ 

| IDENT ARROW IDENT LBRACK  RBRACK {
  let control_org = control_of_id_param $1 in
  let control_dest= control_of_id_param $3 in
  let transit = [] in
  Nts_int.add_transition Parse_machine.current_cautomaton control_org control_dest transit;
  Format.printf "Add an empty transition \n"
}  

;


/*nts_trans_split : nts_trans BAND nts_trans_split { $1 :: $3} 
| nts_trans BAND nts_trans {$1 :: [$3]}*/

nts_trans_split : pressburg_tree_guards BAND gen_affect {$1 :: $3 :: []}


| havocise {$1 :: []}
| gen_affect BAND havocise  {$1 :: $3::[]}
| pressburg_atomic_bool BAND gen_affect {$1 :: $3:: []}
| pressburg_atomic_bool BAND havocise {$1 :: $3 ::[]}
| pressburg_tree_guards BAND havocise {$1 :: $3 ::[]}
| pressburg_atomic_bool BAND gen_affect {$1 :: $3 :: []}
| pressburg_tree_guards BAND gen_affect {$1 :: $3 :: []}
| pressburg_tree_guards BAND gen_affect {$1 :: $3 :: []}
| pressburg_atomic_bool BAND gen_affect BAND havocise {$1::$3::$5::[]}
| pressburg_tree_guards BAND gen_affect BAND havocise {$1::$3::$5::[]}

/*
nts_trans :  pressburg_tree_guards %prec NTS_TRANS { $1 }
| gen_affect {$1}
| havocise {$1}
*/

primed_var_list : primed_express COMMA primed_var_list %prec PRIMEVARLIST {$1::$3}
| primed_express COMMA primed_express {$1::[$3]}
;

gen_affect : PRIMEDVAR EQ arithm_expr   {
  let vname = get_varname_of_primedvarname $1 in
  let vinfo = get_vinfo vname in
  CntAffect(vinfo,$3)
}

/*| primed_express EQ IDENT LBRACE arithm_expr_list RBRACE  
 {
   CntCall($3,Some([$1]),$5)
 }*/

| primed_var_list EQ IDENT LBRACE arithm_expr_list RBRACE {
  CntCall($3,Some($1),$5)
}
| IDENT LBRACE arithm_expr_list RBRACE  {CntCall($1,None,$3)}
;


pressburg_tree_guards : LBRACE pressburg_atomic_bool RBRACE 
  {$2}
| LBRACE pressburg_tree_guards RBRACE {$2}

| pressburg_atomic_bool BAND pressburg_atomic_bool {
  match $1,$3 with
      CntGuard(a),CntGuard(b) -> CntGuard(CntBAnd(a,b))
}

| pressburg_tree_guards BAND  pressburg_atomic_bool {
  match $1,$3 with
      CntGuard(a),CntGuard(b) -> CntGuard(CntBAnd(a,b))
}

| pressburg_atomic_bool BAND  pressburg_tree_guards {
  match $1,$3 with
      CntGuard(a),CntGuard(b) -> CntGuard(CntBAnd(a,b))
}


| pressburg_atomic_bool BOR pressburg_atomic_bool {
  match $1,$3 with
      CntGuard(a),CntGuard(b) -> CntGuard(CntBOr(a,b))
}  


| pressburg_tree_guards BOR  pressburg_atomic_bool {
  match $1,$3 with
      CntGuard(a),CntGuard(b) -> CntGuard(CntBAnd(a,b))
}


|  pressburg_atomic_bool BOR pressburg_tree_guards {
  match $1,$3 with
      CntGuard(a),CntGuard(b) -> CntGuard(CntBAnd(a,b))
}

|  BNOT pressburg_tree_guards {
  match $2 with
  CntGuard(a) -> CntGuard(CntNot(a))
} 
|  BNOT pressburg_atomic_bool {
  match $2 with
  CntGuard(a) -> CntGuard(CntNot(a))
}


pressburg_atomic_bool : BTRUE { CntGuard(CntBTrue) } 
| BFALSE {CntGuard(CntBFalse)} %prec PRESSEVAL

| arithm_expr GT arithm_expr {CntGuard(CntBool(CntGt,$1,$3))} 
| arithm_expr LT arithm_expr {CntGuard(CntBool(CntLt,$1,$3))} 
| arithm_expr GEQ arithm_expr {CntGuard(CntBool(CntGeq,$1,$3))} 
| arithm_expr LEQ arithm_expr {CntGuard(CntBool(CntLeq,$1,$3))} 
| arithm_expr EQ arithm_expr {CntGuard(CntBool(CntEq,$1,$3))} 
;

primed_express : PRIMEDVAR %prec PRIMEDEXPR { 
  let varname = get_varname_of_primedvarname $1 in
  Format.printf "Primed var string is %s \n %!" $1;
  Format.printf "Primed var has name %s \n %! " varname ;

  Format.printf "Looking for var name %s in the current cautomaton whose name is [%s] \n %!" varname !Parse_machine.current_cautomaton.nts_automata_name  ;

  try
    let vinfo = Nts_int.get_varinfo_by_optcautomaton Parse_machine.ntsinstance (Some(!Parse_machine.current_cautomaton)) varname in
    match vinfo with
	Some(v) -> v
      | None -> raise (UnboundVarName ( varname ))
  with
      (Nts_functor.No_such_counter_automata_in_nts_system(a,b))->
	begin
	  Format.printf ("Current context cautomata has the following variables \n");
	  Format.printf ("Input vars : %s \n") (pprint_inputvars !Parse_machine.current_cautomaton );
	  Format.printf ("Output vars : %s \n") (pprint_outputvars !Parse_machine.current_cautomaton );
	  Format.printf ("Local vars : %s \n") (pprint_localvars !Parse_machine.current_cautomaton  );
	  raise  ( No_such_counter_automata_in_nts_system(a,b) )
	end

}
;

 
arithm_expr_list : arithm_expr {$1::[]}
| arithm_expr COMMA arithm_expr_list {$1::$3} 

arithm_expr : INT { let  cst = Big_int.big_int_of_int $1 in 
		   CntCst(cst)}

| IDENT { let vname = $1 in
	  let vinfo = Nts_int.get_varinfo_by_optcautomaton 
	    Parse_machine.ntsinstance 
	    (Some(!Parse_machine.current_cautomaton)) 
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

|  HAVOC LBRACE  RBRACE {
  
  CntHavoc([])
}
;






