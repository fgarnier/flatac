type token =
  | INT of (Big_int.big_int)
  | FLOAT of (float)
  | IDENT of (string)
  | DOTTEDIDENT of (string)
  | REAL of (float)
  | PRIMEDVAR of (string)
  | TIMES
  | PLUS
  | MINUS
  | UMINUS
  | DIV
  | MOD
  | LT
  | GT
  | EQ
  | NEQ
  | LEQ
  | GEQ
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | OBRACK
  | CBRACK
  | COLON
  | SEMICOLON
  | COMMA
  | ARROW
  | PRIME
  | BTRUE
  | BFALSE
  | BAND
  | BOR
  | BNOT
  | EOF
  | NTSDECL
  | INTDECL
  | NATDECL
  | REALDECL
  | BOOLDECL
  | INITSTATE
  | FINALSTATE
  | ERRORSTATE
  | INPUTVARLIST
  | OUTPUTVARLIST
  | LOCALVARLIST
  | HAVOC
  | EXISTS
  | FORALL
  | IMPLY
  | EQUIV
  | LRARROW
  | DOT
  | INSTANCES
  | INIT

open Parsing;;
# 2 "ntl_parser.mly"
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
  Ntsint.Nts_int.states_container_of_states_list l

(*let ret_hash = Hashtbl.create 97 in
  List.iter ( fun s -> Hashtbl.add ret_hash s () ) l;
  ret_hash*) 
    

let get_varname_of_primedvarname pvname =
  String.sub pvname 0 ((String.length pvname)-1)

let trans_hashtbl_of_trans_list tlist =
   Nts_int.transitions_container_of_trans_list  tlist

 
 (* 
We rebuild the transitions using the list provided by
 nts_trans_split_prec rule, as one need a contextual analysis
to properly type which AND are boolean conjuction or separators
between guards, havocs and funcalls.
*)

let rebuild_gen_relation nts_trans_split_prec_list =
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
	      in c
    | Some(stack) -> 
      let res_rel = CntGenRelComp(CntGenBOr,c,stack) in
      let res_rel = Nts_generic.simplify_gen_rel res_rel in
      res_rel


let rebuild_trans_guards nts_trans_split_prec_list =
  let gen_cond = rebuild_gen_relation nts_trans_split_prec_list in
  CntGenGuard(gen_cond)


let rebuild_non_guard_trans list_res = 
  let non_guard_folder lres parsed_elem =
    match parsed_elem with
	`Trans_gen_affect(p)
      | `Trans_havoc (p) -> lres@(p::[])
      | _ -> lres
  in
  List.fold_left non_guard_folder [] list_res


(** *)
let normalize_gvars_init_cond_parameter p = 
  match p with
      `GVarsInitCond(q) -> q
      


let append_to_opt_list optlist appendix =
  match optlist with
      Some(lst)-> lst@appendix
    | None -> appendix
      

let rebuild_top_level_infos nts_name toplevel_list =
  let rebuild_left_folder nts_param header_list_elem =
    match header_list_elem with
	`Decl_gvars(vlist) -> 
	  {
	    nts_system_name = nts_param.nts_system_name;
	    nts_global_vars = vlist@nts_param.nts_global_vars;
	    nts_automata = nts_param.nts_automata;
	    nts_gvars_init = nts_param.nts_gvars_init;
	    nts_system_threads = nts_param.nts_system_threads;
	    
	  }
      
      | `Decl_seq(nts_collection) ->
	  begin
	    {
	      nts_system_name = nts_param.nts_system_name;
	      nts_global_vars = nts_param.nts_global_vars;
	      nts_automata = nts_collection;
	      nts_gvars_init = nts_param.nts_gvars_init;
	      nts_system_threads = nts_param.nts_system_threads;
	    }
	  end

      | `Gvars_cond(nts_conditions) ->
	begin
	 
	  let condition = rebuild_gen_relation nts_conditions 
	    (* normalize_gvars_init_cond_parameter nts_conditions*)
	  in
	    {
	      nts_system_name = nts_param.nts_system_name;
	      nts_global_vars = nts_param.nts_global_vars;
	      nts_automata =  nts_param.nts_automata;
	      nts_gvars_init = Some ((append_to_opt_list 
			       nts_param.nts_gvars_init (condition::[])));

	      nts_system_threads = nts_param.nts_system_threads;
	    }
	end

      | `Thread_decl(threads_list_decl) ->
	begin
	    {
	      nts_system_name = nts_param.nts_system_name;
	      nts_global_vars = nts_param.nts_global_vars;
	      nts_automata = nts_param.nts_automata;
	      nts_gvars_init = nts_param.nts_gvars_init;
	      nts_system_threads = Some ( (append_to_opt_list 
		nts_param.nts_system_threads threads_list_decl));
	    }
	  end
  in
  let nts_init = 
    { 
      nts_system_name = nts_name ; 
      nts_global_vars = [] ; 
      nts_automata = Hashtbl.create 1;
      nts_gvars_init = None;
      nts_system_threads = None;
    } 
  in
    List.fold_left rebuild_left_folder nts_init toplevel_list


# 286 "ntl_parser.ml"
let yytransl_const = [|
  263 (* TIMES *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* UMINUS *);
  267 (* DIV *);
  268 (* MOD *);
  269 (* LT *);
  270 (* GT *);
  271 (* EQ *);
  272 (* NEQ *);
  273 (* LEQ *);
  274 (* GEQ *);
  275 (* LBRACE *);
  276 (* RBRACE *);
  277 (* LBRACK *);
  278 (* RBRACK *);
  279 (* OBRACK *);
  280 (* CBRACK *);
  281 (* COLON *);
  282 (* SEMICOLON *);
  283 (* COMMA *);
  284 (* ARROW *);
  285 (* PRIME *);
  286 (* BTRUE *);
  287 (* BFALSE *);
  288 (* BAND *);
  289 (* BOR *);
  290 (* BNOT *);
    0 (* EOF *);
  291 (* NTSDECL *);
  292 (* INTDECL *);
  293 (* NATDECL *);
  294 (* REALDECL *);
  295 (* BOOLDECL *);
  296 (* INITSTATE *);
  297 (* FINALSTATE *);
  298 (* ERRORSTATE *);
  299 (* INPUTVARLIST *);
  300 (* OUTPUTVARLIST *);
  301 (* LOCALVARLIST *);
  302 (* HAVOC *);
  303 (* EXISTS *);
  304 (* FORALL *);
  305 (* IMPLY *);
  306 (* EQUIV *);
  307 (* LRARROW *);
  308 (* DOT *);
  309 (* INSTANCES *);
  310 (* INIT *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* IDENT *);
  260 (* DOTTEDIDENT *);
  261 (* REAL *);
  262 (* PRIMEDVAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\006\000\009\000\009\000\007\000\010\000\010\000\
\011\000\004\000\004\000\012\000\012\000\013\000\013\000\016\000\
\016\000\016\000\005\000\005\000\017\000\017\000\017\000\015\000\
\015\000\018\000\018\000\018\000\018\000\014\000\014\000\020\000\
\020\000\020\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\026\000\026\000\026\000\
\026\000\026\000\026\000\008\000\008\000\008\000\019\000\027\000\
\027\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
\024\000\024\000\023\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\023\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\022\000\028\000\
\029\000\029\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\025\000\025\000\000\000"

let yylen = "\002\000\
\004\000\004\000\004\000\004\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\001\000\003\000\002\000\003\000\001\000\
\004\000\002\000\001\000\006\000\005\000\002\000\001\000\003\000\
\003\000\002\000\003\000\001\000\003\000\003\000\003\000\002\000\
\001\000\005\000\006\000\008\000\007\000\002\000\001\000\003\000\
\003\000\003\000\002\000\001\000\004\000\004\000\003\000\003\000\
\001\000\001\000\001\000\001\000\003\000\006\000\008\000\008\000\
\006\000\008\000\008\000\003\000\003\000\001\000\001\000\003\000\
\003\000\006\000\005\000\003\000\008\000\007\000\008\000\007\000\
\004\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\001\000\
\001\000\003\000\001\000\001\000\001\000\001\000\003\000\002\000\
\003\000\003\000\003\000\003\000\003\000\004\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\120\000\000\000\000\000\000\000\000\000\
\000\000\019\000\000\000\000\000\001\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\003\000\004\000\000\000\000\000\
\000\000\014\000\000\000\107\000\108\000\000\000\000\000\000\000\
\000\000\096\000\097\000\044\000\000\000\000\000\000\000\000\000\
\000\000\011\000\000\000\049\000\050\000\051\000\052\000\110\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\000\000\000\000\109\000\
\104\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\043\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\000\005\000\
\007\000\008\000\010\000\009\000\029\000\030\000\031\000\027\000\
\000\000\000\000\000\000\000\000\000\000\026\000\000\000\000\000\
\000\000\000\000\022\000\038\000\000\000\015\000\074\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\095\000\094\000\000\000\111\000\047\000\000\000\000\000\048\000\
\000\000\000\000\053\000\000\000\000\000\000\000\000\000\000\000\
\000\000\119\000\000\000\000\000\000\000\117\000\000\000\000\000\
\115\000\116\000\000\000\000\000\000\000\000\000\000\000\000\000\
\061\000\060\000\040\000\041\000\042\000\024\000\025\000\000\000\
\000\000\000\000\021\000\032\000\017\000\000\000\073\000\000\000\
\000\000\075\000\076\000\077\000\078\000\081\000\085\000\000\000\
\000\000\000\000\080\000\079\000\084\000\000\000\000\000\000\000\
\083\000\082\000\000\000\000\000\000\000\064\000\000\000\045\000\
\046\000\118\000\000\000\000\000\020\000\000\000\000\000\106\000\
\067\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\066\000\000\000\000\000\000\000\054\000\000\000\057\000\000\000\
\034\000\063\000\000\000\070\000\000\000\072\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\035\000\069\000\071\000\
\055\000\056\000\058\000\059\000\037\000\000\000\036\000"

let yydgoto = "\002\000\
\004\000\041\000\013\000\014\000\015\000\016\000\017\000\234\000\
\018\000\026\000\027\000\019\000\063\000\064\000\121\000\065\000\
\020\000\122\000\235\000\066\000\043\000\044\000\081\000\045\000\
\046\000\047\000\083\000\048\000\129\000"

let yysindex = "\032\000\
\236\254\000\000\181\255\000\000\017\255\026\255\001\000\001\000\
\129\255\000\000\057\255\207\255\000\000\000\000\062\255\070\255\
\087\255\107\255\002\000\115\255\000\000\000\000\146\255\151\255\
\150\255\000\000\154\255\000\000\000\000\192\255\209\255\075\000\
\233\255\000\000\000\000\000\000\020\000\203\255\151\255\151\255\
\004\001\000\000\016\000\000\000\000\000\000\000\000\000\000\000\
\001\000\001\000\001\000\176\255\212\255\000\000\151\255\218\255\
\151\255\151\255\151\255\151\255\151\255\204\255\247\255\248\255\
\048\255\247\255\000\000\008\000\057\255\020\255\203\000\000\000\
\000\000\075\000\040\001\238\255\011\000\011\000\212\000\014\255\
\103\255\119\255\246\255\025\000\011\000\000\000\004\255\003\000\
\013\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
\075\000\075\000\075\000\075\000\207\255\207\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\047\000\054\000\077\000\079\000\081\000\000\000\248\255\049\255\
\005\000\248\255\000\000\000\000\085\000\000\000\000\000\220\255\
\088\000\041\000\040\001\160\255\097\000\125\255\185\255\239\255\
\000\000\000\000\035\000\000\000\000\000\011\000\011\000\000\000\
\011\000\011\000\000\000\011\000\011\000\112\000\107\000\242\255\
\014\000\000\000\114\000\101\000\110\000\000\000\001\255\001\255\
\000\000\000\000\040\001\040\001\040\001\040\001\040\001\040\001\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\126\000\
\147\000\148\000\000\000\000\000\000\000\075\000\000\000\173\000\
\149\000\000\000\000\000\000\000\000\000\000\000\000\000\123\000\
\124\000\125\000\000\000\000\000\000\000\123\000\124\000\125\000\
\000\000\000\000\123\000\124\000\155\000\000\000\025\000\000\000\
\000\000\000\000\121\000\128\000\000\000\136\000\186\000\000\000\
\000\000\158\000\168\000\175\000\034\000\060\000\205\000\100\255\
\000\000\182\000\194\000\011\000\000\000\011\000\000\000\189\000\
\000\000\000\000\193\000\000\000\191\000\000\000\196\000\050\000\
\024\000\098\000\157\000\166\000\173\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\195\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\206\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\192\000\000\000\000\000\000\000\000\000\
\000\000\000\000\210\000\000\000\000\000\016\001\028\001\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\071\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\223\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\046\000\246\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\127\000\154\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\226\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\230\000\000\000\000\000\000\000\000\000\000\000\236\000\
\000\000\063\000\103\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\084\000\106\000\
\000\000\000\000\005\255\078\255\224\255\225\000\228\000\233\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\253\254\
\255\254\074\255\000\000\000\000\000\000\079\255\096\255\144\255\
\000\000\000\000\158\255\163\255\000\000\000\000\242\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\227\255\036\000\076\000\105\000\000\000\000\000\244\255\
\252\255\194\001\000\000\000\000\199\001\042\000\040\000\000\000\
\000\000\000\000\025\001\000\000\000\000\228\255\189\255\000\000\
\000\000\251\255\131\001\229\255\088\255"

let yytablesize = 564
let yytable = "\042\000\
\010\000\010\000\075\000\079\000\080\000\084\000\056\000\090\000\
\086\000\135\000\138\000\093\000\094\000\216\000\003\000\218\000\
\086\000\153\000\088\000\067\000\028\000\029\000\072\000\154\000\
\099\000\073\000\099\000\082\000\032\000\086\000\099\000\088\000\
\001\000\141\000\088\000\089\000\099\000\099\000\074\000\127\000\
\128\000\131\000\007\000\021\000\132\000\142\000\143\000\079\000\
\134\000\137\000\056\000\008\000\113\000\114\000\115\000\079\000\
\152\000\237\000\239\000\025\000\158\000\159\000\160\000\161\000\
\162\000\163\000\164\000\165\000\166\000\167\000\168\000\136\000\
\139\000\177\000\190\000\193\000\178\000\196\000\199\000\139\000\
\202\000\204\000\155\000\022\000\103\000\105\000\107\000\049\000\
\169\000\170\000\060\000\061\000\062\000\093\000\054\000\050\000\
\062\000\098\000\087\000\098\000\028\000\029\000\030\000\098\000\
\119\000\031\000\093\000\124\000\032\000\098\000\098\000\087\000\
\051\000\189\000\192\000\089\000\195\000\198\000\033\000\201\000\
\203\000\233\000\144\000\207\000\104\000\106\000\108\000\062\000\
\089\000\034\000\035\000\052\000\036\000\037\000\145\000\146\000\
\191\000\194\000\147\000\197\000\200\000\055\000\139\000\139\000\
\186\000\038\000\039\000\040\000\056\000\023\000\148\000\149\000\
\128\000\056\000\128\000\024\000\142\000\143\000\176\000\112\000\
\241\000\180\000\243\000\092\000\116\000\117\000\090\000\091\000\
\092\000\062\000\093\000\094\000\068\000\028\000\029\000\030\000\
\092\000\091\000\031\000\140\000\069\000\032\000\090\000\005\000\
\006\000\057\000\058\000\059\000\060\000\061\000\091\000\033\000\
\229\000\231\000\253\000\090\000\128\000\128\000\079\000\240\000\
\079\000\240\000\034\000\035\000\187\000\036\000\037\000\028\000\
\029\000\030\000\070\000\109\000\031\000\110\000\111\000\032\000\
\145\000\146\000\038\000\039\000\040\000\087\000\242\000\071\000\
\244\000\033\000\090\000\091\000\092\000\118\000\093\000\094\000\
\023\000\028\000\029\000\072\000\034\000\035\000\076\000\036\000\
\037\000\032\000\012\000\102\000\024\000\102\000\182\000\012\000\
\012\000\102\000\120\000\077\000\038\000\039\000\040\000\102\000\
\102\000\133\000\188\000\009\000\053\000\208\000\034\000\035\000\
\125\000\150\000\078\000\028\000\029\000\072\000\148\000\149\000\
\073\000\142\000\143\000\032\000\028\000\029\000\072\000\039\000\
\040\000\073\000\179\000\156\000\032\000\077\000\057\000\058\000\
\059\000\209\000\028\000\029\000\072\000\157\000\085\000\073\000\
\034\000\035\000\032\000\249\000\078\000\145\000\146\000\101\000\
\102\000\034\000\035\000\151\000\228\000\011\000\012\000\145\000\
\146\000\039\000\040\000\184\000\028\000\029\000\072\000\034\000\
\035\000\073\000\148\000\149\000\032\000\109\000\109\000\109\000\
\171\000\109\000\109\000\028\000\029\000\072\000\230\000\172\000\
\073\000\142\000\143\000\032\000\109\000\023\000\023\000\023\000\
\109\000\034\000\035\000\113\000\113\000\074\000\109\000\109\000\
\113\000\113\000\113\000\113\000\113\000\113\000\173\000\113\000\
\174\000\113\000\175\000\183\000\181\000\113\000\113\000\185\000\
\073\000\114\000\114\000\113\000\113\000\250\000\114\000\114\000\
\114\000\114\000\114\000\114\000\068\000\114\000\205\000\114\000\
\068\000\148\000\149\000\114\000\114\000\210\000\068\000\068\000\
\211\000\114\000\114\000\112\000\112\000\112\000\112\000\112\000\
\112\000\212\000\112\000\213\000\112\000\214\000\215\000\219\000\
\112\000\112\000\142\000\145\000\148\000\220\000\112\000\112\000\
\104\000\104\000\104\000\223\000\104\000\104\000\104\000\104\000\
\104\000\104\000\104\000\104\000\221\000\028\000\029\000\072\000\
\251\000\225\000\073\000\222\000\104\000\032\000\028\000\029\000\
\072\000\252\000\226\000\073\000\145\000\146\000\032\000\074\000\
\217\000\227\000\028\000\029\000\072\000\148\000\149\000\073\000\
\074\000\236\000\032\000\028\000\029\000\130\000\224\000\232\000\
\073\000\245\000\247\000\032\000\074\000\238\000\246\000\248\000\
\255\000\028\000\090\000\091\000\092\000\074\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\012\000\140\000\
\110\000\110\000\110\000\016\000\110\000\110\000\110\000\110\000\
\110\000\110\000\110\000\110\000\103\000\110\000\103\000\101\000\
\039\000\101\000\103\000\033\000\100\000\101\000\100\000\105\000\
\103\000\103\000\100\000\101\000\101\000\065\000\126\000\123\000\
\100\000\100\000\090\000\091\000\092\000\254\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\109\000\109\000\
\109\000\206\000\109\000\109\000\109\000\109\000\109\000\109\000\
\109\000\109\000\104\000\104\000\104\000\000\000\104\000\104\000\
\104\000\104\000\000\000\104\000\104\000\104\000\090\000\091\000\
\092\000\000\000\093\000\094\000"

let yycheck = "\012\000\
\000\000\000\000\032\000\033\000\033\000\033\000\003\001\007\001\
\037\000\077\000\078\000\011\001\012\001\182\000\035\001\184\000\
\020\001\085\000\020\001\024\000\001\001\002\001\003\001\020\001\
\020\001\006\001\022\001\033\000\009\001\033\001\026\001\033\001\
\001\000\020\001\039\000\040\000\032\001\033\001\019\001\020\001\
\070\000\071\000\026\001\008\000\074\000\032\001\033\001\077\000\
\077\000\078\000\003\001\026\001\057\000\058\000\059\000\085\000\
\085\000\226\000\227\000\003\001\090\000\091\000\092\000\093\000\
\094\000\095\000\096\000\097\000\098\000\099\000\100\000\077\000\
\078\000\025\001\142\000\143\000\028\001\145\000\146\000\085\000\
\148\000\149\000\087\000\008\000\049\000\050\000\051\000\026\001\
\101\000\102\000\043\001\044\001\022\001\020\001\019\000\026\001\
\026\001\020\001\020\001\022\001\001\001\002\001\003\001\026\001\
\063\000\006\001\033\001\066\000\009\001\032\001\033\001\033\001\
\026\001\142\000\143\000\020\001\145\000\146\000\019\001\148\000\
\149\000\022\001\020\001\151\000\049\000\050\000\051\000\023\000\
\033\001\030\001\031\001\025\001\033\001\034\001\032\001\033\001\
\142\000\143\000\020\001\145\000\146\000\027\001\148\000\149\000\
\020\001\046\001\047\001\048\001\003\001\021\001\032\001\033\001\
\182\000\003\001\184\000\027\001\032\001\033\001\119\000\055\000\
\228\000\122\000\230\000\020\001\060\000\061\000\007\001\008\001\
\009\001\065\000\011\001\012\001\023\001\001\001\002\001\003\001\
\033\001\020\001\006\001\020\001\027\001\009\001\020\001\003\001\
\004\001\040\001\041\001\042\001\043\001\044\001\033\001\019\001\
\221\000\222\000\022\001\033\001\226\000\227\000\228\000\228\000\
\230\000\230\000\030\001\031\001\020\001\033\001\034\001\001\001\
\002\001\003\001\019\001\036\001\006\001\038\001\039\001\009\001\
\032\001\033\001\046\001\047\001\048\001\019\001\228\000\015\001\
\230\000\019\001\007\001\008\001\009\001\026\001\011\001\012\001\
\021\001\001\001\002\001\003\001\030\001\031\001\006\001\033\001\
\034\001\009\001\020\001\020\001\027\001\022\001\027\001\025\001\
\026\001\026\001\003\001\019\001\046\001\047\001\048\001\032\001\
\033\001\020\001\020\001\003\001\003\001\020\001\030\001\031\001\
\001\001\020\001\034\001\001\001\002\001\003\001\032\001\033\001\
\006\001\032\001\033\001\009\001\001\001\002\001\003\001\047\001\
\048\001\006\001\022\001\025\001\009\001\019\001\040\001\041\001\
\042\001\020\001\001\001\002\001\003\001\025\001\019\001\006\001\
\030\001\031\001\009\001\020\001\034\001\032\001\033\001\032\001\
\033\001\030\001\031\001\027\001\019\001\053\001\054\001\032\001\
\033\001\047\001\048\001\019\001\001\001\002\001\003\001\030\001\
\031\001\006\001\032\001\033\001\009\001\007\001\008\001\009\001\
\026\001\011\001\012\001\001\001\002\001\003\001\019\001\026\001\
\006\001\032\001\033\001\009\001\022\001\040\001\041\001\042\001\
\026\001\030\001\031\001\008\001\009\001\019\001\032\001\033\001\
\013\001\014\001\015\001\016\001\017\001\018\001\026\001\020\001\
\026\001\022\001\026\001\020\001\024\001\026\001\027\001\015\001\
\006\001\008\001\009\001\032\001\033\001\020\001\013\001\014\001\
\015\001\016\001\017\001\018\001\022\001\020\001\015\001\022\001\
\026\001\032\001\033\001\026\001\027\001\020\001\032\001\033\001\
\036\001\032\001\033\001\013\001\014\001\015\001\016\001\017\001\
\018\001\036\001\020\001\022\001\022\001\003\001\003\001\003\001\
\026\001\027\001\032\001\032\001\032\001\003\001\032\001\033\001\
\007\001\008\001\009\001\028\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\052\001\001\001\002\001\003\001\
\020\001\020\001\006\001\052\001\027\001\009\001\001\001\002\001\
\003\001\020\001\019\001\006\001\032\001\033\001\009\001\019\001\
\020\001\019\001\001\001\002\001\003\001\032\001\033\001\006\001\
\019\001\020\001\009\001\001\001\002\001\003\001\021\001\003\001\
\006\001\021\001\020\001\009\001\019\001\020\001\022\001\020\001\
\022\001\026\001\007\001\008\001\009\001\019\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\025\001\020\001\
\007\001\008\001\009\001\026\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\020\001\020\001\022\001\020\001\
\003\001\022\001\026\001\022\001\020\001\026\001\022\001\020\001\
\032\001\033\001\026\001\032\001\033\001\020\001\069\000\065\000\
\032\001\033\001\007\001\008\001\009\001\245\000\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\007\001\008\001\
\009\001\151\000\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\007\001\008\001\009\001\255\255\011\001\012\001\
\013\001\014\001\255\255\016\001\017\001\018\001\007\001\008\001\
\009\001\255\255\011\001\012\001"

let yynames_const = "\
  TIMES\000\
  PLUS\000\
  MINUS\000\
  UMINUS\000\
  DIV\000\
  MOD\000\
  LT\000\
  GT\000\
  EQ\000\
  NEQ\000\
  LEQ\000\
  GEQ\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  OBRACK\000\
  CBRACK\000\
  COLON\000\
  SEMICOLON\000\
  COMMA\000\
  ARROW\000\
  PRIME\000\
  BTRUE\000\
  BFALSE\000\
  BAND\000\
  BOR\000\
  BNOT\000\
  EOF\000\
  NTSDECL\000\
  INTDECL\000\
  NATDECL\000\
  REALDECL\000\
  BOOLDECL\000\
  INITSTATE\000\
  FINALSTATE\000\
  ERRORSTATE\000\
  INPUTVARLIST\000\
  OUTPUTVARLIST\000\
  LOCALVARLIST\000\
  HAVOC\000\
  EXISTS\000\
  FORALL\000\
  IMPLY\000\
  EQUIV\000\
  LRARROW\000\
  DOT\000\
  INSTANCES\000\
  INIT\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  IDENT\000\
  DOTTEDIDENT\000\
  REAL\000\
  PRIMEDVAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'comma_sep_header_list) in
    Obj.repr(
# 265 "ntl_parser.mly"
                                                         (
  let nts_name = _2 in
  rebuild_top_level_infos nts_name _4 
)
# 715 "ntl_parser.ml"
               : Ntsint.Nts_int.nts_system))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'decl_sequence) in
    Obj.repr(
# 270 "ntl_parser.mly"
                                        ( 
  let nts_name = _2 in
  {
    nts_system_name = nts_name;
    nts_global_vars = [] ;
    nts_automata = cautomata_hashtbl_of_cautomata_list _4;
    nts_gvars_init = None;
    nts_system_threads = None ;
  }
)
# 732 "ntl_parser.ml"
               : Ntsint.Nts_int.nts_system))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'comma_sep_header_list) in
    Obj.repr(
# 280 "ntl_parser.mly"
                                                      (
  let nts_name = _2 in
  rebuild_top_level_infos nts_name _4 
)
# 743 "ntl_parser.ml"
               : Ntsint.Nts_int.nts_system))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'decl_sequence) in
    Obj.repr(
# 285 "ntl_parser.mly"
                                              ( 
  let nts_name = _2 in
  {
    nts_system_name = nts_name;
    nts_global_vars = [] ;
    nts_automata = cautomata_hashtbl_of_cautomata_list _4;
    nts_gvars_init = None;
    nts_system_threads = None ;
  }
)
# 760 "ntl_parser.ml"
               : Ntsint.Nts_int.nts_system))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typed_id_list_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decl_sequence) in
    Obj.repr(
# 298 "ntl_parser.mly"
                                                                   (
  let nts_automata = cautomata_hashtbl_of_cautomata_list _3 in
  `Decl_gvars(_1)::`Decl_seq(nts_automata)::[]
)
# 771 "ntl_parser.ml"
               : 'comma_sep_header_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typed_id_list_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'comma_sep_header_list) in
    Obj.repr(
# 303 "ntl_parser.mly"
                                                     (
    `Decl_gvars(_1)::_3
  )
# 781 "ntl_parser.ml"
               : 'comma_sep_header_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'gvars_initial_conditions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'comma_sep_header_list) in
    Obj.repr(
# 307 "ntl_parser.mly"
                                                           (
      `Gvars_cond(_1)::_3
  )
# 791 "ntl_parser.ml"
               : 'comma_sep_header_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'gvars_initial_conditions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decl_sequence) in
    Obj.repr(
# 311 "ntl_parser.mly"
                                                   (
  let nts_automata  = cautomata_hashtbl_of_cautomata_list _3 in
  `Gvars_cond(_1)::`Decl_seq(nts_automata)::[]
)
# 802 "ntl_parser.ml"
               : 'comma_sep_header_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'thread_declaration) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decl_sequence) in
    Obj.repr(
# 316 "ntl_parser.mly"
                                             (
  let nts_automata  = cautomata_hashtbl_of_cautomata_list _3 in
  `Thread_decl(_1)::`Decl_seq(nts_automata)::[]
)
# 813 "ntl_parser.ml"
               : 'comma_sep_header_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'thread_declaration) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'comma_sep_header_list) in
    Obj.repr(
# 321 "ntl_parser.mly"
                                                     (
  `Thread_decl(_1)::_3
)
# 823 "ntl_parser.ml"
               : 'comma_sep_header_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'nts_trans_split_prec) in
    Obj.repr(
# 331 "ntl_parser.mly"
                                                     (_2)
# 830 "ntl_parser.ml"
               : 'gvars_initial_conditions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 334 "ntl_parser.mly"
                   ([_1])
# 837 "ntl_parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident_list) in
    Obj.repr(
# 335 "ntl_parser.mly"
                         (_1::_3)
# 845 "ntl_parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'thread_decl_list) in
    Obj.repr(
# 341 "ntl_parser.mly"
                                                ( _2 )
# 852 "ntl_parser.ml"
               : 'thread_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'thread_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'thread_decl_list) in
    Obj.repr(
# 343 "ntl_parser.mly"
                                                      ( _1 :: _3 )
# 860 "ntl_parser.ml"
               : 'thread_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'thread_decl) in
    Obj.repr(
# 344 "ntl_parser.mly"
               ([_1])
# 867 "ntl_parser.ml"
               : 'thread_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Big_int.big_int) in
    Obj.repr(
# 347 "ntl_parser.mly"
                                      (
   (_1 , _3 )
)
# 877 "ntl_parser.ml"
               : 'thread_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl_automata) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl_sequence) in
    Obj.repr(
# 351 "ntl_parser.mly"
                                            (_1::_2)
# 885 "ntl_parser.ml"
               : 'decl_sequence))
; (fun __caml_parser_env ->
    Obj.repr(
# 352 "ntl_parser.mly"
      ([])
# 891 "ntl_parser.ml"
               : 'decl_sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'vars_automaton) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'states_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'transitions_list) in
    Obj.repr(
# 359 "ntl_parser.mly"
(
  (*Format.printf "!!!!!##### Creating a new counter automaton %s \n" $1;*)
  let cautomata_name = _1 in
  let varlocs = _3 in
  let trans_list = _5 in
  let states_list = _4 in
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
  
)
# 922 "ntl_parser.ml"
               : 'decl_automata))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'states_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'transitions_list) in
    Obj.repr(
# 383 "ntl_parser.mly"
(
  (*Format.printf "!!!!!##### Creating a new counter automaton %s \n" $1;*)
  let cautomata_name = _1 in
  let varlocs = [] in
  let trans_list = _4 in
  let states_list = _3 in
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
  
)
# 952 "ntl_parser.ml"
               : 'decl_automata))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vars_loc) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vars_automaton) in
    Obj.repr(
# 406 "ntl_parser.mly"
                                          ( _1 @ _2 )
# 960 "ntl_parser.ml"
               : 'vars_automaton))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vars_loc) in
    Obj.repr(
# 407 "ntl_parser.mly"
           (_1)
# 967 "ntl_parser.ml"
               : 'vars_automaton))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typed_id_list_list) in
    Obj.repr(
# 410 "ntl_parser.mly"
                                                      (
  List.map (  build_local_var_list_mapper  Cautomaton_input) _2  )
# 975 "ntl_parser.ml"
               : 'vars_loc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typed_id_list_list) in
    Obj.repr(
# 413 "ntl_parser.mly"
                                              (
  List.map ( build_local_var_list_mapper  Cautomaton_output) _2  )
# 983 "ntl_parser.ml"
               : 'vars_loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typed_id_list_list) in
    Obj.repr(
# 416 "ntl_parser.mly"
                               (
  List.map ( build_local_var_list_mapper  Cautomaton_local ) _1  )
# 991 "ntl_parser.ml"
               : 'vars_loc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typed_id_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typed_id_list_list) in
    Obj.repr(
# 423 "ntl_parser.mly"
                                                            (_1 @ _3)
# 999 "ntl_parser.ml"
               : 'typed_id_list_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typed_id_list) in
    Obj.repr(
# 424 "ntl_parser.mly"
                (_1)
# 1006 "ntl_parser.ml"
               : 'typed_id_list_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    Obj.repr(
# 426 "ntl_parser.mly"
                                         (
List.map (fun s -> NtsGenVar(NtsVar(s,NtsIntType),NtsUnPrimed)) _1
)
# 1015 "ntl_parser.ml"
               : 'typed_id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    Obj.repr(
# 430 "ntl_parser.mly"
                           (
List.map (fun s -> NtsGenVar(NtsVar(s,NtsRealType),NtsUnPrimed)) _1
)
# 1024 "ntl_parser.ml"
               : 'typed_id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    Obj.repr(
# 433 "ntl_parser.mly"
                           (
List.map (fun s -> NtsGenVar(NtsVar(s,NtsBoolType),NtsUnPrimed)) _1
)
# 1033 "ntl_parser.ml"
               : 'typed_id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'transitions_list) in
    Obj.repr(
# 438 "ntl_parser.mly"
                                                (_1::_2)
# 1041 "ntl_parser.ml"
               : 'transitions_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'transitions) in
    Obj.repr(
# 439 "ntl_parser.mly"
              ( _1 :: [])
# 1048 "ntl_parser.ml"
               : 'transitions_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 442 "ntl_parser.mly"
                                                (
  let control_org = control_of_id_param _1 in
  let control_dest= control_of_id_param _3 in
  let transit = [] in
  (control_org, control_dest, transit )
 
)
# 1062 "ntl_parser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'nts_trans_split) in
    Obj.repr(
# 450 "ntl_parser.mly"
                                                  (
  let control_org = control_of_id_param _1 in
  let control_dest= control_of_id_param _3 in
  let transit = _5 in
  (control_org, control_dest, transit)
)
# 1076 "ntl_parser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'nts_trans_split) in
    Obj.repr(
# 457 "ntl_parser.mly"
                                                              (
  let control_org = control_of_id_param _3 in
  let control_dest= control_of_id_param _5 in
  let transit = _7 in
  (control_org, control_dest, transit ) 
)
# 1091 "ntl_parser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 464 "ntl_parser.mly"
                                               (
  let control_org = control_of_id_param _3 in
  let control_dest= control_of_id_param _5 in
  let transit = [] in
  (control_org, control_dest, transit )
)
# 1105 "ntl_parser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cautomaton_state) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'states_list) in
    Obj.repr(
# 473 "ntl_parser.mly"
                                            (_1 @ _2)
# 1113 "ntl_parser.ml"
               : 'states_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cautomaton_state) in
    Obj.repr(
# 474 "ntl_parser.mly"
                   (_1)
# 1120 "ntl_parser.ml"
               : 'states_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ident_list) in
    Obj.repr(
# 477 "ntl_parser.mly"
                                                    (
  (*Format.printf "Adds an initial state \n";*) 
  List.map (build_cautomata_states_mapper Cautomaton_start 
	    ) _2 
  
  )
# 1132 "ntl_parser.ml"
               : 'cautomaton_state))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ident_list) in
    Obj.repr(
# 484 "ntl_parser.mly"
                                  (
  (*Format.printf "Adds a final state \n";*)
  List.map ( build_cautomata_states_mapper Cautomaton_final
  ) _2
)
# 1143 "ntl_parser.ml"
               : 'cautomaton_state))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ident_list) in
    Obj.repr(
# 490 "ntl_parser.mly"
                                  (
  (*Format.printf "Adds an error state \n";*)
  List.map ( build_cautomata_states_mapper Cautomaton_error  
  ) _2
    
)
# 1155 "ntl_parser.ml"
               : 'cautomaton_state))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_atomic_bool) in
    Obj.repr(
# 502 "ntl_parser.mly"
                             (`Trans_atom_neg(_2))
# 1162 "ntl_parser.ml"
               : 'nts_trans_elem))
; (fun __caml_parser_env ->
    Obj.repr(
# 503 "ntl_parser.mly"
      ( `Trans_new_guard )
# 1168 "ntl_parser.ml"
               : 'nts_trans_elem))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'pressburg_atomic_bool) in
    Obj.repr(
# 504 "ntl_parser.mly"
                                            (`Trans_atom_neg(_3))
# 1175 "ntl_parser.ml"
               : 'nts_trans_elem))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'pressburg_tree_guards) in
    Obj.repr(
# 505 "ntl_parser.mly"
                                            (`Trans_atom_neg(_3))
# 1182 "ntl_parser.ml"
               : 'nts_trans_elem))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pressburg_atomic_bool) in
    Obj.repr(
# 506 "ntl_parser.mly"
                                      ( `Trans_atom_bool(_2) )
# 1189 "ntl_parser.ml"
               : 'nts_trans_elem))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pressburg_tree_guards) in
    Obj.repr(
# 507 "ntl_parser.mly"
                                      ( `Trans_tree_bool(_2) )
# 1196 "ntl_parser.ml"
               : 'nts_trans_elem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_atomic_bool) in
    Obj.repr(
# 508 "ntl_parser.mly"
                        ( `Trans_atom_bool(_1) )
# 1203 "ntl_parser.ml"
               : 'nts_trans_elem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'gen_affect) in
    Obj.repr(
# 509 "ntl_parser.mly"
             (`Trans_gen_affect(_1))
# 1210 "ntl_parser.ml"
               : 'nts_trans_elem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'havocise) in
    Obj.repr(
# 510 "ntl_parser.mly"
           (`Trans_havoc (_1))
# 1217 "ntl_parser.ml"
               : 'nts_trans_elem))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'qformula) in
    Obj.repr(
# 511 "ntl_parser.mly"
           (`Qformula(_1))
# 1224 "ntl_parser.ml"
               : 'nts_trans_elem))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'qformula) in
    Obj.repr(
# 512 "ntl_parser.mly"
                         (`Qformula(_2))
# 1231 "ntl_parser.ml"
               : 'nts_trans_elem))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'ident_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_atomic_bool) in
    Obj.repr(
# 515 "ntl_parser.mly"
                                                                     (
  let var_list = List.map (fun s -> NtsGenVar(NtsVar(s,NtsIntType),NtsUnPrimed)) _2
  in 
  CntQVarsGenRel(var_list,NtsExists,_6)
 )
# 1243 "ntl_parser.ml"
               : 'qformula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'ident_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'pressburg_tree_guards) in
    Obj.repr(
# 521 "ntl_parser.mly"
                                                                          (
  let var_list = List.map (fun s -> NtsGenVar(NtsVar(s,NtsIntType),NtsUnPrimed)) _2
  in 
  CntQVarsGenRel(var_list,NtsExists,_7)
 )
# 1255 "ntl_parser.ml"
               : 'qformula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'ident_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'qformula) in
    Obj.repr(
# 527 "ntl_parser.mly"
                                                             (
  let var_list = List.map (fun s -> NtsGenVar(NtsVar(s,NtsIntType),NtsUnPrimed)) _2
  in 
  CntQVarsGenRel(var_list,NtsExists,_7)
 )
# 1267 "ntl_parser.ml"
               : 'qformula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'ident_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_atomic_bool) in
    Obj.repr(
# 533 "ntl_parser.mly"
                                                             (
  let var_list = List.map (fun s -> NtsGenVar(NtsVar(s,NtsIntType),NtsUnPrimed)) _2
  in 
  CntQVarsGenRel(var_list,NtsForall,_6)
 )
# 1279 "ntl_parser.ml"
               : 'qformula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'ident_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'pressburg_tree_guards) in
    Obj.repr(
# 539 "ntl_parser.mly"
                                                                          (
  let var_list = List.map (fun s -> NtsGenVar(NtsVar(s,NtsIntType),NtsUnPrimed)) _2
  in 
  CntQVarsGenRel(var_list,NtsForall,_7)
 )
# 1291 "ntl_parser.ml"
               : 'qformula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'ident_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'qformula) in
    Obj.repr(
# 545 "ntl_parser.mly"
                                                             (
  let var_list = List.map (fun s -> NtsGenVar(NtsVar(s,NtsIntType),NtsUnPrimed)) _2
  in 
  CntQVarsGenRel(var_list,NtsForall,_7)
)
# 1303 "ntl_parser.ml"
               : 'qformula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'nts_trans_elem) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nts_trans_split_prec) in
    Obj.repr(
# 554 "ntl_parser.mly"
(
_1 :: `Trans_new_guard :: _3
)
# 1313 "ntl_parser.ml"
               : 'nts_trans_split_prec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'nts_trans_elem) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nts_trans_split_prec) in
    Obj.repr(
# 557 "ntl_parser.mly"
                                           ( _1 :: _3 )
# 1321 "ntl_parser.ml"
               : 'nts_trans_split_prec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nts_trans_elem) in
    Obj.repr(
# 558 "ntl_parser.mly"
                 ( _1 :: [] )
# 1328 "ntl_parser.ml"
               : 'nts_trans_split_prec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nts_trans_split_prec) in
    Obj.repr(
# 562 "ntl_parser.mly"
                                       (
  let list_res = _1 in
  let guard = rebuild_trans_guards list_res in
  let affects_n_havocs = rebuild_non_guard_trans list_res in
  guard::affects_n_havocs

)
# 1341 "ntl_parser.ml"
               : 'nts_trans_split))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'primed_express) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'primed_var_list) in
    Obj.repr(
# 571 "ntl_parser.mly"
                                                                          (_1::_3)
# 1349 "ntl_parser.ml"
               : 'primed_var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'primed_express) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'primed_express) in
    Obj.repr(
# 572 "ntl_parser.mly"
                                      (_1::[_3])
# 1357 "ntl_parser.ml"
               : 'primed_var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'arithm_expr_list) in
    Obj.repr(
# 575 "ntl_parser.mly"
                                                                (
  let vname = get_varname_of_primedvarname _1 in
  let vinfolist =  NtsGenVar(NtsVar(vname,NtsUnTyped),NtsPrimed)::[] 
  in 
  CntGenCall(_3,Some(vinfolist),_5)

)
# 1372 "ntl_parser.ml"
               : 'gen_affect))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 584 "ntl_parser.mly"
                                     (
  let vname = get_varname_of_primedvarname _1 in
  let vinfolist =  NtsGenVar(NtsVar(vname,NtsUnTyped),NtsPrimed)::[] 
  in 
  CntGenCall(_3,Some(vinfolist),[])

)
# 1386 "ntl_parser.ml"
               : 'gen_affect))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 592 "ntl_parser.mly"
                             (
  let vname = get_varname_of_primedvarname _1 in
  let vinfo = (*get_vinfo*) NtsGenVar(NtsVar(vname,NtsUnTyped),NtsPrimed) 
  in 
  (* We need to
     get the type of the
     variable a posteriori
     --Sementical verification
     phase
  *)
  CntGenGuard(CntGenRel(CntEq,CntGenVar(vinfo),_3))
)
# 1405 "ntl_parser.ml"
               : 'gen_affect))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'arithm_expr_list) in
    Obj.repr(
# 605 "ntl_parser.mly"
                                                                  (
  let vname = get_varname_of_primedvarname _2 in
  let vinfolist =  NtsGenVar(NtsVar(vname,NtsUnTyped),NtsPrimed)::[] 
  in 
  CntGenCall(_5,Some(vinfolist),_7)
)
# 1419 "ntl_parser.ml"
               : 'gen_affect))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 612 "ntl_parser.mly"
                                                  (
  let vname = get_varname_of_primedvarname _2 in
  let vinfolist =  NtsGenVar(NtsVar(vname,NtsUnTyped),NtsPrimed)::[] 
  in 
  CntGenCall(_5,Some(vinfolist),[])
)
# 1432 "ntl_parser.ml"
               : 'gen_affect))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'primed_var_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'arithm_expr_list) in
    Obj.repr(
# 619 "ntl_parser.mly"
                                                                        (
  CntGenCall(_5,Some(_2),_7)
)
# 1443 "ntl_parser.ml"
               : 'gen_affect))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'primed_var_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 622 "ntl_parser.mly"
                                                       (
  CntGenCall(_5,Some(_2),[])
)
# 1453 "ntl_parser.ml"
               : 'gen_affect))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arithm_expr_list) in
    Obj.repr(
# 625 "ntl_parser.mly"
                                        (CntGenCall(_1,None,_3))
# 1461 "ntl_parser.ml"
               : 'gen_affect))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 626 "ntl_parser.mly"
                      (CntGenCall(_1,None,[]))
# 1468 "ntl_parser.ml"
               : 'gen_affect))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pressburg_atomic_bool) in
    Obj.repr(
# 631 "ntl_parser.mly"
  (_2)
# 1475 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pressburg_tree_guards) in
    Obj.repr(
# 632 "ntl_parser.mly"
                                      (_2)
# 1482 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'qformula) in
    Obj.repr(
# 634 "ntl_parser.mly"
                         (_2)
# 1489 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_atomic_bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_atomic_bool) in
    Obj.repr(
# 636 "ntl_parser.mly"
                                                   (
 
  CntGenRelComp(CntGenBAnd,_1,_3)
)
# 1500 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_tree_guards) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_tree_guards) in
    Obj.repr(
# 641 "ntl_parser.mly"
                                                   (
  CntGenRelComp(CntGenBAnd,_1,_3)
)
# 1510 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_tree_guards) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_atomic_bool) in
    Obj.repr(
# 645 "ntl_parser.mly"
                                                    (
  CntGenRelComp(CntGenBAnd,_1,_3)
)
# 1520 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_atomic_bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_tree_guards) in
    Obj.repr(
# 649 "ntl_parser.mly"
                                                    (
   CntGenRelComp(CntGenBAnd,_1,_3)
  
)
# 1531 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'qformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_tree_guards) in
    Obj.repr(
# 654 "ntl_parser.mly"
                                       (
  CntGenRelComp(CntGenBAnd,_1,_3)
)
# 1541 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'qformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_atomic_bool) in
    Obj.repr(
# 658 "ntl_parser.mly"
                                      (
 CntGenRelComp(CntGenBAnd,_1,_3)
)
# 1551 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_tree_guards) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'qformula) in
    Obj.repr(
# 663 "ntl_parser.mly"
                                       (
  CntGenRelComp(CntGenBAnd,_1,_3)
)
# 1561 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_atomic_bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'qformula) in
    Obj.repr(
# 667 "ntl_parser.mly"
                                       (
 CntGenRelComp(CntGenBAnd,_1,_3)
)
# 1571 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_atomic_bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_atomic_bool) in
    Obj.repr(
# 672 "ntl_parser.mly"
                                                  (
   CntGenRelComp(CntGenBOr,_1,_3)
)
# 1581 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_tree_guards) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_atomic_bool) in
    Obj.repr(
# 677 "ntl_parser.mly"
                                                   (
  CntGenRelComp(CntGenBOr,_1,_3)

)
# 1592 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_atomic_bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_tree_guards) in
    Obj.repr(
# 683 "ntl_parser.mly"
                                                   (
  CntGenRelComp(CntGenBOr,_1,_3)

)
# 1603 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_tree_guards) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_tree_guards) in
    Obj.repr(
# 689 "ntl_parser.mly"
                                                  (
  CntGenRelComp(CntGenBOr,_1,_3)
)
# 1613 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'qformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_tree_guards) in
    Obj.repr(
# 693 "ntl_parser.mly"
                                      (
  CntGenRelComp(CntGenBOr,_1,_3)
)
# 1623 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'qformula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_atomic_bool) in
    Obj.repr(
# 697 "ntl_parser.mly"
                                     (
 CntGenRelComp(CntGenBOr,_1,_3)
)
# 1633 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_tree_guards) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'qformula) in
    Obj.repr(
# 702 "ntl_parser.mly"
                                      (
  CntGenRelComp(CntGenBOr,_1,_3)
)
# 1643 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pressburg_atomic_bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'qformula) in
    Obj.repr(
# 706 "ntl_parser.mly"
                                      (
 CntGenRelComp(CntGenBOr,_1,_3)
)
# 1653 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_tree_guards) in
    Obj.repr(
# 712 "ntl_parser.mly"
                              (
  CntGenNot(_2)
  
)
# 1663 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pressburg_atomic_bool) in
    Obj.repr(
# 717 "ntl_parser.mly"
                              (
  CntGenNot(_2)

)
# 1673 "ntl_parser.ml"
               : 'pressburg_tree_guards))
; (fun __caml_parser_env ->
    Obj.repr(
# 723 "ntl_parser.mly"
                              ( CntGenTrue )
# 1679 "ntl_parser.ml"
               : 'pressburg_atomic_bool))
; (fun __caml_parser_env ->
    Obj.repr(
# 724 "ntl_parser.mly"
         (CntGenFalse)
# 1685 "ntl_parser.ml"
               : 'pressburg_atomic_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 726 "ntl_parser.mly"
                             (CntGenRel(CntGt,_1,_3))
# 1693 "ntl_parser.ml"
               : 'pressburg_atomic_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 727 "ntl_parser.mly"
                             (CntGenRel(CntLt,_1,_3))
# 1701 "ntl_parser.ml"
               : 'pressburg_atomic_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 728 "ntl_parser.mly"
                              (CntGenRel(CntGeq,_1,_3))
# 1709 "ntl_parser.ml"
               : 'pressburg_atomic_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 729 "ntl_parser.mly"
                              (CntGenRel(CntLeq,_1,_3))
# 1717 "ntl_parser.ml"
               : 'pressburg_atomic_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 730 "ntl_parser.mly"
                             (CntGenRel(CntEq,_1,_3))
# 1725 "ntl_parser.ml"
               : 'pressburg_atomic_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 731 "ntl_parser.mly"
                              (CntGenRel(CntNeq,_1,_3))
# 1733 "ntl_parser.ml"
               : 'pressburg_atomic_bool))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 734 "ntl_parser.mly"
                                            ( 
  let varname = get_varname_of_primedvarname _1 in
  (*Format.printf "Primed var string is %s \n %!" $1;
  Format.printf "Primed var has name %s \n %! " varname ;*)
  NtsGenVar(NtsVar(varname,NtsUnTyped),NtsPrimed)
 
)
# 1746 "ntl_parser.ml"
               : 'primed_express))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 744 "ntl_parser.mly"
                               (_1::[])
# 1753 "ntl_parser.ml"
               : 'arithm_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithm_expr_list) in
    Obj.repr(
# 745 "ntl_parser.mly"
                                     (_1::_3)
# 1761 "ntl_parser.ml"
               : 'arithm_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Big_int.big_int) in
    Obj.repr(
# 748 "ntl_parser.mly"
                  ( let  cst =  _1 in 
		   CntGenCst(CntGenICst(cst),NtsIntType))
# 1769 "ntl_parser.ml"
               : Nts_types.nts_genrel_arithm_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 750 "ntl_parser.mly"
        (
  let cst = _1 in
  CntGenCst(CntGenFCst(cst),NtsRealType)
)
# 1779 "ntl_parser.ml"
               : Nts_types.nts_genrel_arithm_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 756 "ntl_parser.mly"
        ( let vname = _1 in
	  CntGenVar(NtsGenVar(NtsVar(vname,NtsUnTyped),NtsUnPrimed))
	)
# 1788 "ntl_parser.ml"
               : Nts_types.nts_genrel_arithm_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primed_express) in
    Obj.repr(
# 760 "ntl_parser.mly"
                 (
  CntGenVar(_1)
)
# 1797 "ntl_parser.ml"
               : Nts_types.nts_genrel_arithm_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 763 "ntl_parser.mly"
                            (_2)
# 1804 "ntl_parser.ml"
               : Nts_types.nts_genrel_arithm_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 764 "ntl_parser.mly"
                                 ( CntGenArithmUOp(CntGenUMinus,_2,NtsUnTyped) )
# 1811 "ntl_parser.ml"
               : Nts_types.nts_genrel_arithm_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 765 "ntl_parser.mly"
                               ( CntGenArithmBOp(CntGenSum,_1,_3,NtsUnTyped) )
# 1819 "ntl_parser.ml"
               : Nts_types.nts_genrel_arithm_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 766 "ntl_parser.mly"
                                (CntGenArithmBOp( CntGenMinus,_1,_3,NtsUnTyped) )
# 1827 "ntl_parser.ml"
               : Nts_types.nts_genrel_arithm_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 767 "ntl_parser.mly"
                              (CntGenArithmBOp( CntGenDiv,_1,_3,NtsUnTyped) )
# 1835 "ntl_parser.ml"
               : Nts_types.nts_genrel_arithm_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 768 "ntl_parser.mly"
                              (CntGenArithmBOp( CntGenMod,_1,_3,NtsUnTyped) )
# 1843 "ntl_parser.ml"
               : Nts_types.nts_genrel_arithm_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nts_types.nts_genrel_arithm_exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nts_types.nts_genrel_arithm_exp) in
    Obj.repr(
# 769 "ntl_parser.mly"
                                ( CntGenArithmBOp( CntGenProd,_1,_3,NtsUnTyped) )
# 1851 "ntl_parser.ml"
               : Nts_types.nts_genrel_arithm_exp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ident_list) in
    Obj.repr(
# 773 "ntl_parser.mly"
                                          (
  let ntvarlist = (*List.map get_vinfo $3*) List.map (fun s -> NtsGenVar(NtsVar(s,NtsUnTyped),NtsUnPrimed)) _3 in 
  CntGenHavoc(ntvarlist)
)
# 1861 "ntl_parser.ml"
               : 'havocise))
; (fun __caml_parser_env ->
    Obj.repr(
# 778 "ntl_parser.mly"
                        (
  CntGenHavoc([])
)
# 1869 "ntl_parser.ml"
               : 'havocise))
(* Entry ntldescr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let ntldescr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ntsint.Nts_int.nts_system)
