%{
  open Lexing
  open Nts_types
  open Nts_functor
  open Ntsint.Nts_int
  open Parsing_error 
  

  exception UnBoundVariable of string * Parsing_error.loc option
  type varsort = Nat | Int | Real | Bool

  open Ntsint (* This module contains the definition of the moduel
	      Nts_int, which is an "instance of the functor 
		 Nts_functor"*)
  
  
 
    
  type vloctype = Cautomaton_local 
		  | Cautomaton_input 
		  | Cautomaton_output
		      
  type stateloctype = Cautomaton_start
		      | Cautomaton_final
		      | Cautomaton_error
		      | Cautomaton_common



let build_local_var_list_mapper vloctype  s =
  (s,vloctype)
      
	  
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
We rebuild the transitions using the list provided by
 nts_trans_split_prec rule, as one need a contextual analysis
to properly type which AND are boolean conjuction or separators
between guards, havocs and funcalls.
*)

let rebuild_trans_guards nts_trans_split_prec_list =
  let nts_guards_of_parsed_info (cnt_curr_bool, bool_stack ) parsed_elem =
    match parsed_elem with
	`Trans_new_guard -> 
	  begin
	    match bool_stack with
		None -> (CntGenTrue,Some(cnt_curr_bool))
	      | Some(prev_bool) -> 
		(CntGenTrue, Some(CntGenRelComp(CntGenBOr,cnt_curr_bool,prev_bool)))
	  end
      | `Trans_tree_bool(parsed_bool) -> 
	let curr_bool = CntGenRelComp(CntGenBAnd,cnt_curr_bool,parsed_bool) in
	(curr_bool,bool_stack )
      | `Trans_atom_bool(parsed_bool) -> 
	let curr_bool = CntGenRelComp(CntGenBAnd,cnt_curr_bool,parsed_bool) in
	(curr_bool,bool_stack )
      | `Trans_atom_neg(parsed_bool) ->
	let curr_bool = CntGenRelComp(CntGenBAnd,cnt_curr_bool,CntGenNot(parsed_bool))
	in
	(curr_bool,bool_stack )
      | `Qformula(parsed_bool) ->
	let curr_bool = CntGenRelComp(CntGenBAnd,cnt_curr_bool,parsed_bool) in
	(curr_bool,bool_stack )
      | _ -> (cnt_curr_bool,bool_stack ) 
  in
  let (c,s) =
    List.fold_left nts_guards_of_parsed_info (CntGenTrue, None) nts_trans_split_prec_list in
  match s with 
      None -> let c = Nts_generic.simplify_gen_rel c 
	      in CntGenGuard(c)
    | Some(stack) -> 
      let res_rel = CntGenRelComp(CntGenBOr,c,stack) in
      let res_rel = Nts_generic.simplify_gen_rel res_rel in
      CntGenGuard(res_rel)


let rebuild_non_guard_trans list_res = 
  let non_guard_folder lres parsed_elem =
    match parsed_elem with
	`Trans_gen_affect(p)
      | `Trans_havoc (p) -> lres@(p::[])
      | _ -> lres
  in
  List.fold_left non_guard_folder [] list_res
      
      
%}


%token <Big_int.big_int>  INT
%token <string> IDENT
%token <float> REAL
%token <string> PRIMEDVAR 
%type <Ntsint.Nts_int.nts_system> ntldescr 
%type <Nts_types.nts_genrel_arithm_exp> arithm_expr

%token TIMES PLUS MINUS UMINUS DIV MOD LT GT MOD EQ NEQ LEQ GEQ LBRACE
%token RBRACE LBRACK RBRACK OBRACK CBRACK COLON SEMICOLON COMMA ARROW
%token  PRIME BTRUE BFALSE BAND BOR BNOT EOF
%token NTSDECL INTDECL NATDECL REALDECL BOOLDECL INITSTATE FINALSTATE ERRORSTATE
%token INPUTVARLIST OUTPUTVARLIST LOCALVARLIST HAVOC
%token EXISTS FORALL IMPLY EQUIV LRARROW DOT 

%nonassoc PRIMEVARLIST 
%nonassoc PRIMEDEXPR 
%nonassoc EQ LBRACK RBRACK
%nonassoc PRESSEVAL
%nonassoc UMINUS 



%left BOR
%left PLUS MINUS 
%left TIMES DIV MOD BAND
%right BNOT 

%nonassoc NTS_TRANS
%start ntldescr
%%



ntldescr : NTSDECL IDENT SEMICOLON gvars_list_decl decl_sequence {
  
  { 
    nts_system_name = $2 ;
    nts_global_vars = $4 ;
    nts_automata = cautomata_hashtbl_of_cautomata_list $5 ;
  }
}




| NTSDECL IDENT SEMICOLON decl_sequence { 
  let nts_name = $2 in
  {
    nts_system_name = nts_name;
    nts_global_vars = [] ;
    nts_automata = cautomata_hashtbl_of_cautomata_list $4 

  }
}
 
;

ident_list : IDENT {[$1]}
| IDENT COMMA ident_list {$1::$3}
;

gvars_list_decl : gvars_decl COMMA gvars_list_decl {$1 @ $3}
| gvars_decl SEMICOLON {$1}
;

gvars_decl : ident_list COLON INTDECL { 
  List.map (fun s-> NtsGenVar(NtsIVar(s),NtsUnPrimed)) $1
}

| ident_list COLON REALDECL  {
List.map (fun s->NtsGenVar( NtsRVar(s),NtsUnPrimed)) $1 
} 

| ident_list COLON NATDECL {
List.map  (fun s->NtsGenVar( NtsNVar(s),NtsUnPrimed)) $1 
}

| ident_list COLON BOOLDECL {
List.map  (fun s->NtsGenVar( NtsBVar(s),NtsUnPrimed)) $1 
}
;

decl_sequence : decl_automata decl_sequence {$1::$2}
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

vars_automaton : vars_loc  vars_automaton { $1 @ $2 }
| vars_loc {$1}
;

vars_loc : INPUTVARLIST typed_id_list_list SEMICOLON  {
  List.map (  build_local_var_list_mapper  Cautomaton_input) $2  }

| OUTPUTVARLIST typed_id_list_list  SEMICOLON {
  List.map ( build_local_var_list_mapper  Cautomaton_output) $2  }

| typed_id_list_list SEMICOLON {
  List.map ( build_local_var_list_mapper  Cautomaton_local ) $1  }


;


typed_id_list_list : typed_id_list COMMA typed_id_list_list {$1 @ $3} 
| typed_id_list {$1}

typed_id_list : ident_list COLON INTDECL {
List.map (fun s -> NtsGenVar(NtsIVar(s),NtsUnPrimed)) $1
}
| ident_list COLON NATDECL{
List.map (fun s -> NtsGenVar(NtsNVar(s),NtsUnPrimed)) $1
}
| ident_list COLON REALDECL{
List.map (fun s -> NtsGenVar(NtsRVar(s),NtsUnPrimed)) $1
}
| ident_list COLON BOOLDECL{
List.map (fun s -> NtsGenVar(NtsBVar(s),NtsUnPrimed)) $1
}
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

| IDENT COLON IDENT ARROW IDENT LBRACK nts_trans_split RBRACK {
  let control_org = control_of_id_param $3 in
  let control_dest= control_of_id_param $5 in
  let transit = $7 in
  (control_org, control_dest, transit )
 


}

| IDENT COLON IDENT ARROW IDENT LBRACK  RBRACK {
  let control_org = control_of_id_param $3 in
  let control_dest= control_of_id_param $5 in
  let transit = [] in
  (control_org, control_dest, transit )
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



nts_trans_elem :/* BNOT {`Trans_neg_of_guard} */  /* Negation until the
*/ 
| BNOT pressburg_atomic_bool {`Trans_atom_neg($2)}
| BOR { `Trans_new_guard } /* Marks a new disjoint guard*/
| BNOT LBRACE pressburg_atomic_bool  RBRACE {`Trans_atom_neg($3)}
| BNOT LBRACE pressburg_tree_guards  RBRACE {`Trans_atom_neg($3)}
| LBRACE pressburg_atomic_bool RBRACE { `Trans_atom_bool($2) }
| LBRACE pressburg_tree_guards RBRACE { `Trans_tree_bool($2) }
| pressburg_atomic_bool { `Trans_atom_bool($1) }
| gen_affect {`Trans_gen_affect($1)}
| havocise {`Trans_havoc ($1)}
| qformula {`Qformula($1)}
| LBRACE qformula RBRACE {`Qformula($2)}


qformula : EXISTS ident_list COLON INTDECL DOT pressburg_atomic_bool {
  let var_list = List.map (fun s -> NtsGenVar(NtsIVar(s),NtsUnPrimed)) $2
  in 
  CntQVarsGenRel(var_list,NtsExists,$6)
 } 

| EXISTS ident_list COLON INTDECL DOT LBRACE pressburg_tree_guards RBRACE {
  let var_list = List.map (fun s -> NtsGenVar(NtsIVar(s),NtsUnPrimed)) $2
  in 
  CntQVarsGenRel(var_list,NtsExists,$7)
 }

| EXISTS ident_list COLON INTDECL DOT LBRACE qformula RBRACE {
  let var_list = List.map (fun s -> NtsGenVar(NtsIVar(s),NtsUnPrimed)) $2
  in 
  CntQVarsGenRel(var_list,NtsExists,$7)
 }
  
|  FORALL ident_list COLON INTDECL DOT pressburg_atomic_bool {
  let var_list = List.map (fun s -> NtsGenVar(NtsIVar(s),NtsUnPrimed)) $2
  in 
  CntQVarsGenRel(var_list,NtsForall,$6)
 } 

| FORALL ident_list COLON INTDECL DOT LBRACE pressburg_tree_guards RBRACE {
  let var_list = List.map (fun s -> NtsGenVar(NtsIVar(s),NtsUnPrimed)) $2
  in 
  CntQVarsGenRel(var_list,NtsForall,$7)
 }

| FORALL ident_list COLON INTDECL DOT LBRACE qformula RBRACE {
  let var_list = List.map (fun s -> NtsGenVar(NtsIVar(s),NtsUnPrimed)) $2
  in 
  CntQVarsGenRel(var_list,NtsForall,$7)
}



nts_trans_split_prec :  nts_trans_elem BOR nts_trans_split_prec 
{
$1 :: `Trans_new_guard :: $3
}
| nts_trans_elem BAND nts_trans_split_prec { $1 :: $3 }
| nts_trans_elem { $1 :: [] }
;


nts_trans_split : nts_trans_split_prec {
  let list_res = $1 in
  let guard = rebuild_trans_guards list_res in
  let affects_n_havocs = rebuild_non_guard_trans list_res in
  guard::affects_n_havocs

}



primed_var_list : primed_express COMMA primed_var_list %prec PRIMEVARLIST {$1::$3}
| primed_express COMMA primed_express {$1::[$3]}
;

gen_affect : PRIMEDVAR EQ IDENT  LBRACE arithm_expr_list RBRACE {
  let vname = get_varname_of_primedvarname $1 in
  let vinfolist =  NtsGenVar(NtsMiscType(vname),NtsPrimed)::[] 
  in 
  CntGenCall($3,Some(vinfolist),$5)

}


| PRIMEDVAR EQ arithm_expr   {
  let vname = get_varname_of_primedvarname $1 in
  let vinfo = (*get_vinfo*) NtsGenVar(NtsMiscType(vname),NtsPrimed) 
  in 
  (* We need to
     get the type of the
     variable a posteriori
     --Sementical verification
     phase
  *)
  CntGenGuard(CntGenRel(CntEq,CntGenVar(vinfo),$3))
}


| LBRACE primed_var_list RBRACE EQ IDENT LBRACE arithm_expr_list RBRACE {
  CntGenCall($5,Some($2),$7)
}
| LBRACE primed_var_list RBRACE EQ IDENT LBRACE RBRACE {
  CntGenCall($5,Some($2),[])
}
| IDENT LBRACE arithm_expr_list RBRACE  {CntGenCall($1,None,$3)}
| IDENT LBRACE RBRACE {CntGenCall($1,None,[])}

;


pressburg_tree_guards : LBRACE pressburg_atomic_bool RBRACE 
  {$2}
| LBRACE pressburg_tree_guards RBRACE {$2}

| LBRACE qformula RBRACE {$2}

| pressburg_atomic_bool BAND pressburg_atomic_bool {
 
  CntGenRelComp(CntGenBAnd,$1,$3)
}


| pressburg_tree_guards BAND pressburg_tree_guards {
  CntGenRelComp(CntGenBAnd,$1,$3)
}

| pressburg_tree_guards BAND  pressburg_atomic_bool {
  CntGenRelComp(CntGenBAnd,$1,$3)
}

| pressburg_atomic_bool BAND  pressburg_tree_guards {
   CntGenRelComp(CntGenBAnd,$1,$3)
  
}


| qformula BAND  pressburg_tree_guards {
  CntGenRelComp(CntGenBAnd,$1,$3)
}

| qformula BAND pressburg_atomic_bool {
 CntGenRelComp(CntGenBAnd,$1,$3)
}


|  pressburg_tree_guards BAND qformula {
  CntGenRelComp(CntGenBAnd,$1,$3)
}

|  pressburg_atomic_bool BAND qformula {
 CntGenRelComp(CntGenBAnd,$1,$3)
}



| pressburg_atomic_bool BOR pressburg_atomic_bool {
   CntGenRelComp(CntGenBAnd,$1,$3)
}  


| pressburg_tree_guards BOR  pressburg_atomic_bool {
  CntGenRelComp(CntGenBAnd,$1,$3)

}


|  pressburg_atomic_bool BOR pressburg_tree_guards {
  CntGenRelComp(CntGenBOr,$1,$3)

}


| qformula BOR  pressburg_tree_guards {
  CntGenRelComp(CntGenBOr,$1,$3)
}

| qformula BOR pressburg_atomic_bool {
 CntGenRelComp(CntGenBOr,$1,$3)
}


|  pressburg_tree_guards BOR qformula {
  CntGenRelComp(CntGenBOr,$1,$3)
}

|  pressburg_atomic_bool BOR qformula {
 CntGenRelComp(CntGenBOr,$1,$3)
}



|  BNOT pressburg_tree_guards {
  CntGenNot($2)
  
} 

|  BNOT pressburg_atomic_bool {
  CntGenNot($2)

}


pressburg_atomic_bool : BTRUE { CntGenTrue } 
| BFALSE {CntGenFalse} %prec PRESSEVAL

| arithm_expr GT arithm_expr {CntGenRel(CntGt,$1,$3)} 
| arithm_expr LT arithm_expr {CntGenRel(CntLt,$1,$3)} 
| arithm_expr GEQ arithm_expr {CntGenRel(CntGeq,$1,$3)} 
| arithm_expr LEQ arithm_expr {CntGenRel(CntLeq,$1,$3)} 
| arithm_expr EQ arithm_expr {CntGenRel(CntEq,$1,$3)} 
| arithm_expr NEQ arithm_expr {CntGenRel(CntNeq,$1,$3)}
;

primed_express : PRIMEDVAR %prec PRIMEDEXPR { 
  let varname = get_varname_of_primedvarname $1 in
  Format.printf "Primed var string is %s \n %!" $1;
  Format.printf "Primed var has name %s \n %! " varname ;
  NtsGenVar(NtsMiscType(varname),NtsPrimed)
 
}
;

 
arithm_expr_list : arithm_expr {$1::[]}
| arithm_expr COMMA arithm_expr_list {$1::$3} 
;

arithm_expr : INT { let  cst =  $1 in 
		   CntGenCst(cst)}

| IDENT { let vname = $1 in
	  CntGenVar(NtsGenVar(NtsMiscType(vname),NtsUnPrimed))
	}

| primed_express {
  CntGenVar($1)
}
| LBRACE arithm_expr RBRACE {$2}
| MINUS arithm_expr %prec UMINUS { CntGenArithmUOp(CntGenUMinus,$2) }
| arithm_expr PLUS arithm_expr { CntGenArithmBOp(CntGenSum,$1,$3) }
| arithm_expr MINUS arithm_expr {CntGenArithmBOp( CntGenMinus,$1,$3) }
| arithm_expr DIV arithm_expr {CntGenArithmBOp( CntGenDiv,$1,$3) }
| arithm_expr MOD arithm_expr {CntGenArithmBOp( CntGenMod,$1,$3) }
| arithm_expr TIMES arithm_expr { CntGenArithmBOp( CntGenProd,$1,$3) }
;


havocise : HAVOC LBRACE ident_list RBRACE {
  let ntvarlist = (*List.map get_vinfo $3*) List.map (fun s -> NtsGenVar(NtsMiscType(s),NtsUnPrimed)) $3 in 
  CntGenHavoc(ntvarlist)
}

|  HAVOC LBRACE  RBRACE {
  
  CntGenHavoc([])
}
;






