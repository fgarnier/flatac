%{

  open Lexing
  open Nts_types
  open Nts_functor

  open Ntsint.Nts_int
  open Trace_types

  exception Fail_to_extract_Sysname_Statename_from of string


  let starts_with_underscore s =
    if s.[0]='_' then true
    else false

  let state_of_str s =
    Format.printf "Separting state and system from %s \n %!" s;
    let slen = String.length s in
    let index_beg = ref 0 in
    let index_end = ref 0 in
    let starts_with_underscore = ref (starts_with_underscore s) 
    in 
    let is_state_part = ref false in
    let sysname = ref "" in
    let state_name = ref "" in
    let finish = ref false in
    
    while !index_end < slen && (not !finish) do
      if ( (not !is_state_part) && !starts_with_underscore 
	   && s.[(!index_end)] != '_') 
      then
	begin
	  index_end := (!index_end) +1 ;
	  starts_with_underscore := false 
	end
      else if ( (not !is_state_part) && !starts_with_underscore 
		&& s.[!index_end] = '_') 
      then
	begin
	  index_end := !index_end +1 ;
	end
	  
      else if ( (not !starts_with_underscore) && 
		  (not !is_state_part) && 
		  s.[!index_end]='_')
      then
	begin
	  sysname := String.sub s !index_beg !index_end ;
	  index_beg := !index_end+1;
	  index_end := !index_end+1;
	  is_state_part := true;
	end
      else if (!is_state_part && (s.[!index_end]='_')) 
      then
	begin
	  state_name := (String.sub s !index_beg (!index_end -1));
	  finish :=true
	end
      else
	index_end:= !index_end + 1;
    done;
    if (not !is_state_part) then
      raise (Fail_to_extract_Sysname_Statename_from s)
    else
      begin
	Format.printf "sysname = %s; control is = %s \n %!" !sysname !state_name;
	Trace_types.Sys_control(!sysname,!state_name)
      end
       
	  
      
	



%}

%type <Trace_types.trace> gettrace

%token <string> IDENT
%token EOF
%token COMMA
%token LBRACE
%token RBRACE
%token COLON
%token TRACEDECL
%token SEMICOLON
%start gettrace

%%



gettrace : TRACEDECL LBRACE statelist RBRACE EOF {
  let parsedlist = $3 in
   parsedlist
};

statelist : state COMMA statelist { $1 :: $3 }
	    | state {[$1]};

state : LBRACE IDENT SEMICOLON IDENT RBRACE {Trace_types.Sys_control($2,$4)}



