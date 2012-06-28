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
  
  
  

 (* module Parse_machine  
    = 
  struct
    let ntsinstance = (Ntsint.Nts_int.create_nts_system "")
    let current_cautomaton = ref (Ntsint.Nts_int.create_nts_automaton "")
  end
 *)
  (* *)
    
  type vloctype = Cautomaton_local 
		  | Cautomaton_input 
		  | Cautomaton_output
		      
  type stateloctype = Cautomaton_start
		      | Cautomaton_final
		      | Cautomaton_error
		      | Cautomaton_common



let build_local_var_list_mapper (vsort : varsort ) vloctype  s =
    match vsort with 
	Int ->
	  (NtsIVar(s),vloctype)
      | Real -> 
	(NtsRVar(s),vloctype)
	  
let build_cautomata_states_mapper (state_type : stateloctype ) s =
  let sin = Nts_int.control_of_id_param s in
  (sin,state_type)


(* Used to get either start states, final or error states from
the list of all states returne by the parser production rule.*)

let get_states_by_locality_type_of_states_list (sloctype : stateloctype ) l =
  let retp = List.filter ( 
    fun (_,b) -> 
      match b,sloctype 
      with
	  Cautomaton_start,Cautomaton_start -> true
	| Cautomaton_final,Cautomaton_final -> true
	| Cautomaton_error,Cautomaton_error -> true
	| Cautomaton_common,Cautomaton_common -> true
	| (_,_) -> false
  )  l in
 List.map (fun(a,_)->a) retp
   
(* Used to get either local, input or output, vars from
the list of all declared variables returned by the parser production rule.*)
let get_vars_by_locality_type_of_vars_list vloctype l =
  let retp = List.filter ( 
    fun (_,b) -> 
      match b,vloctype 
      with
	  Cautomaton_local,Cautomaton_local ->true
	| Cautomaton_input,Cautomaton_input ->true
	| Cautomaton_output,Cautomaton_output ->true
	| (_,_) -> false
  )  l in
  List.map (fun(a,_)->a) retp
   


let cautomata_hashtbl_of_cautomata_list (l : Nts_int.nts_automaton list) =
  let ret_hash = Hashtbl.create 97 in
  List.iter ( fun c -> Hashtbl.add ret_hash c.nts_automata_name c ) l;
  ret_hash

let state_hashtbl_of_statelist ( l : Nts_int.control list) =
  let ret_hash = Hashtbl.create 97 in
  List.iter ( fun s -> Hashtbl.add ret_hash s () ) l;
  ret_hash
    

let get_varname_of_primedvarname pvname =
  String.sub pvname 0 ((String.length pvname)-1)

(* Converts the (control * control * tlist) list 
   into
   (control, (control, tlist) Hashtbl.t ) Hastbl.t
*)

let trans_hashtbl_of_trans_list tlist =
  let ret_hash = Hashtbl.create 97 in
  let build_iterator (c1,c2, tlabel ) =
    if not (Hashtbl.mem ret_hash c1) then
      begin
	let new_rel_hash = Hashtbl.create 97 in
	Hashtbl.add new_rel_hash c2 tlabel;
	Hashtbl.add ret_hash c1 new_rel_hash
      end
    else
      begin
	let inner_relation = Hashtbl.find ret_hash c1 in
	Hashtbl.add inner_relation c2 tlabel
      end
  in
  List.iter build_iterator tlist; ret_hash

(*
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

*)	  

%}


%token <int>  INT
%token <string> IDENT
%token <float> REAL
%token <string> PRIMEDVAR 
%type <Ntsint.Nts_int.nts_system> ntldescr 
%type <Nts_types.cnt_arithm_exp> arithm_expr

%token TIMES PLUS MINUS UMINUS DIV MOD LT GT MOD EQ NEQ LEQ GEQ LBRACE
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
  let nts_name = $2 in
  {
    nts_system_name = nts_name;
    nts_global_vars = [] ;
    nts_automata = cautomata_hashtbl_of_cautomata_list $4 

  }
}
 
|  NTSDECL IDENT SEMICOLON gvars_decl decl_sequence {
  
  { 
    nts_system_name = $2 ;
    nts_global_vars = $4 ;
    nts_automata = cautomata_hashtbl_of_cautomata_list $5 ;
  }
}

|  NTSDECL IDENT SEMICOLON gvars_list_decl decl_sequence {
  
  { 
    nts_system_name = $2 ;
    nts_global_vars = $4 ;
    nts_automata = cautomata_hashtbl_of_cautomata_list $5 ;
  }
}

;

ident_list : IDENT {[$1]}
| IDENT COMMA ident_list {$1::$3}
;

gvars_list_decl : gvars_decl gvars_list_decl {$1 @ $2}
;

gvars_decl : ident_list INTDECL SEMICOLON { 
  List.map (fun s-> NtsIVar(s)) $1
}

| ident_list REALDECL SEMICOLON {
List.map (fun s-> NtsRVar(s)) $1 

} 
;

decl_sequence : decl_automata decl_sequence {$1::$2}
/*| decl_automata {$1}*/
| EOF {[]}
;




decl_automata : IDENT LBRACK vars_automaton states_list transitions_list RBRACK
{
  Format.printf "!!!!!##### Creating a new counter automaton %s \n" $1;
  let cautomata_name = $1 in
  let varlocs = $3 in
  let trans_list = $5 in
  let states_list = $4 in
  {
    nts_automata_name = cautomata_name;
    anot = Nts_int.anot_parser () ;
    init_states =  state_hashtbl_of_statelist( (get_states_by_locality_type_of_states_list Cautomaton_start) states_list );
    final_states = state_hashtbl_of_statelist( 
	get_states_by_locality_type_of_states_list Cautomaton_final states_list );
    error_states =  state_hashtbl_of_statelist( 
      get_states_by_locality_type_of_states_list Cautomaton_error states_list );
    
    input_vars = get_vars_by_locality_type_of_vars_list Cautomaton_input varlocs;
    output_vars = get_vars_by_locality_type_of_vars_list Cautomaton_output varlocs;
    local_vars = get_vars_by_locality_type_of_vars_list Cautomaton_local varlocs;
    transitions = trans_hashtbl_of_trans_list trans_list;
  }
  
}

vars_automaton : vars_loc vars_automaton { $1 @ $2 }
| vars_loc {$1}
;

vars_loc : INPUTVARLIST ident_list COLON INTDECL SEMICOLON  {
  List.map (  build_local_var_list_mapper Int Cautomaton_input) $2  }

| INPUTVARLIST ident_list COLON REALDECL SEMICOLON {
  List.map ( build_local_var_list_mapper Real Cautomaton_input ) $2  }

| OUTPUTVARLIST ident_list COLON INTDECL SEMICOLON {
  List.map ( build_local_var_list_mapper Int Cautomaton_output) $2  }

| OUTPUTVARLIST ident_list COLON REALDECL SEMICOLON {
  List.map ( build_local_var_list_mapper Real Cautomaton_output) $2 } 

| ident_list COLON INTDECL SEMICOLON {
  List.map ( build_local_var_list_mapper Int Cautomaton_local ) $1  }

| ident_list COLON REALDECL SEMICOLON {
  List.map ( build_local_var_list_mapper Real Cautomaton_local ) $1  }

;

transitions_list : transitions transitions_list {$1::$2}
| transitions { $1 :: []}
;

transitions :  IDENT ARROW IDENT LBRACK  RBRACK {
  let control_org = control_of_id_param $1 in
  let control_dest= control_of_id_param $3 in
  let transit = [] in
  (control_org, control_dest, transit )
 
}

| IDENT ARROW IDENT LBRACK nts_trans_split RBRACK {
  let control_org = control_of_id_param $1 in
  let control_dest= control_of_id_param $3 in
  let transit = $5 in
  (control_org, control_dest, transit)
}
;  
  

states_list  : cautomaton_state states_list {$1 @ $2}
| cautomaton_state {$1}
;

cautomaton_state :  INITSTATE ident_list SEMICOLON  {
  Format.printf "Adds an initial state \n"; 
  List.map (build_cautomata_states_mapper Cautomaton_start 
	    ) $2 
  
  }

| FINALSTATE ident_list SEMICOLON {
  Format.printf "Adds a final state \n";
  List.map ( build_cautomata_states_mapper Cautomaton_final
  ) $2
}

| ERRORSTATE ident_list SEMICOLON {
  Format.printf "Adds an error state \n";
  List.map ( build_cautomata_states_mapper Cautomaton_error  
  ) $2
    
}
;




/* cautomaton_decl_sections :
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
  (control_org,control_dest, transit)
}  


/* | IDENT  ARROW IDENT LBRACK nts_trans RBRACK {
  let control_org = control_of_id_param $1 in
  let control_dest= control_of_id_param $3 in
  let transit = ($5::[]) in
  Nts_int.add_transition Parse_machine.current_cautomaton control_org control_dest transit
}*/ 

/*
| IDENT ARROW IDENT LBRACK  RBRACK {
  let control_org = control_of_id_param $1 in
  let control_dest= control_of_id_param $3 in
  let transit = [] in
 
  Format.printf "Add an empty transition \n"
}  

;
*/



nts_trans_split : havocise {$1 :: []}
| gen_affect BAND havocise  {$1 :: $3::[]}
| pressburg_atomic_bool BAND gen_affect {$1 :: $3:: []}
| pressburg_atomic_bool BAND havocise {$1 :: $3 ::[]}
| pressburg_tree_guards BAND havocise {$1 :: $3 ::[]}
| pressburg_tree_guards BAND gen_affect {$1 :: $3 :: []}
| pressburg_atomic_bool BAND gen_affect BAND havocise {$1::$3::$5::[]}
| pressburg_tree_guards BAND gen_affect BAND havocise {$1::$3::$5::[]}



primed_var_list : primed_express COMMA primed_var_list %prec PRIMEVARLIST {$1::$3}
| primed_express COMMA primed_express {$1::[$3]}
;

gen_affect : PRIMEDVAR EQ arithm_expr   {
  let vname = get_varname_of_primedvarname $1 in
  let vinfo = (*get_vinfo*) NtsMiscType(vname) in (* We need to
						  get the type of the
						  variable a posteriori
						     --Sementical verification
						     phase
						  *)
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
| arithm_expr NEQ arithm_expr {CntGuard(CntBool(CntNeq,$1,$3))}
;

primed_express : PRIMEDVAR %prec PRIMEDEXPR { 
  let varname = get_varname_of_primedvarname $1 in
  Format.printf "Primed var string is %s \n %!" $1;
  Format.printf "Primed var has name %s \n %! " varname ;
  NtsMiscType(varname)
 (*
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
 *)
}
;

 
arithm_expr_list : arithm_expr {$1::[]}
| arithm_expr COMMA arithm_expr_list {$1::$3} 

arithm_expr : INT { let  cst = Big_int.big_int_of_int $1 in 
		   CntCst(cst)}

| IDENT { let vname = $1 in
	  (*let vinfo = Nts_int.get_varinfo_by_optcautomaton 
	    Parse_machine.ntsinstance 
	    (Some(!Parse_machine.current_cautomaton)) 
	    vname in
	  let var =
	    match vinfo with
		Some(v) -> v
	      | None ->  raise (UnboundVarName ( vname ))
	  in*)
	  CntVar(NtsMiscType(vname))
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
  let ntvarlist = (*List.map get_vinfo $3*) List.map (fun s -> NtsMiscType(s)) $3 in 
  CntHavoc(ntvarlist)
}

|  HAVOC LBRACE  RBRACE {
  
  CntHavoc([])
}
;






