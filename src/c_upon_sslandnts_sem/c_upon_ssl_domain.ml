(** This file  contains aims encodes the semantic of some C-functions
over the SSL formula. Those one are currently hard coded, which is
not satisfactory in a long term view.

Question & remarks : Address to florent dot garnier AT imag dot fr.
*)
open Format
open Cil_types
open Ssl_types
open SSL_lex
open Ssl
open Ssl_decision
open Debug_printers
open Global_mem
open List
open Self


exception No_pvar_in_free_expression
exception Wrong_parameter_type_in_free
exception Debug_information of string
exception Contains_no_pvar



(** This function aims at getting the first variable name
of the list of parameters. Might be useful if some parameter
expressions are prefixed by a cast or any other ugly stuff so
pecuiliar to the C-language.
*)

(** This function checks whether the argument is a pointer 
variable or a casted pointer variable. It returns a Ssl_type.PVar("vname")
if so, and raise an exception is not.*)
let rec get_pvar_from_exp (expr : Cil_types.exp ) =
  match expr.enode with
      Lval ( Var( p ) , _ ) ->
	begin
	  match p.vtype with 
	      TPtr(_,_) -> (PVar(p.vname))
	    | _ -> raise Contains_no_pvar
	end
    | CastE (TPtr (_,_), e ) ->
	get_pvar_from_exp e

    | _ ->  raise Contains_no_pvar
	  

let rec get_first_ptvar_from_lparam ( lparam : Cil_types.exp list ) =
 match lparam with 
     [] -> raise No_pvar_in_free_expression
   | h::l' ->
	 begin
	   match h.enode with
	       (Lval(Var(varinfo),_)) ->
		 begin
		   match varinfo.vtype with
		       TPtr(_,_) -> (PVar(varinfo.vname))
		     | _ -> get_first_ptvar_from_lparam l'
		 end
		    | CastE(_,expr) -> 
		      begin 
			Self.debug ~level:0 "J'ai vu un cast dans la liste
des parametres \n" ;
			try
			  get_pvar_from_exp expr
			with
			    Contains_no_pvar -> get_first_ptvar_from_lparam l'
		      end
		(* begin
		 match param.enode with
		     Lval(Var(varinf),_) -> (PVar(varinf.vname))
		   | _ -> get_first_ptvar_from_lparam l'
		 end *)
       
		    | _  -> get_first_ptvar_from_lparam l' 
	 end
	   

(** This function modifies the sslf formula that abstracts the current
heap and stack when a call to malloc is performed.*)
let malloc_upon_ssl  ( v : Cil_types.varinfo option ) ( mid : global_mem_manager )  (sslf : ssl_formula ) =
  match v with Some (vinfo) ->
    let lvar = mid#lvar_from_malloc () in
    let pvar = (PVar(vinfo.vname)) in
    let affect = (Pointsto (pvar,lvar)) in
    Ssl.add_quant_var lvar sslf;
    Ssl.and_atomic_affect affect sslf;
    Ssl.add_alloc_cell lvar sslf
      
    | None ->
       let lvar = mid#lvar_from_malloc () in
       Ssl.add_quant_var lvar sslf;
       Ssl.add_alloc_cell lvar sslf
	 
(** Effect of a free(x),  where x is a pointer variable, on an ssl
formula.*)
let free_upon_ssl (pvar : ptvar)(sslf : ssl_formula) =
  try
    let lvar = get_ptr_affectation sslf pvar in
    if not (space_contains_locvar lvar  sslf.space )
    then set_heap_to_top sslf 
    else
      try_remove_segment lvar sslf
  with
      Not_found -> set_heap_to_top sslf (* Here we get that pvar
					does not belong to the 
					the affectation table*)
	
(** For testing purposes *)
let next_on_ssl_instr_debug  (mid : global_mem_manager ) ( sslf :ssl_formula) ( instruction : Cil_types.instr) =
  let lvar = mid#lvar_from_malloc () in
  let pvar = (PVar("Dummy")) in
  let affect = (Pointsto (pvar,lvar)) in
  Ssl.add_quant_var lvar sslf;
  Ssl.and_atomic_affect affect sslf;
  Ssl.add_alloc_cell lvar sslf



(** mid must be an instance of the class global mem manager*)
let next_on_ssl_instr  (mid : global_mem_manager ) ( sslf :ssl_formula) ( instruction : Cil_types.instr) =
   	Self.debug ~level:0 "\n Dans next_on_ssl_instr \n" ;
    match instruction with 
	  (*****************************************************************)
	
       
	  (*   We consider here the call of function that have an impact
	  on the heap and the stack, namely :
	       _malloc & calloc
	       _free
	  *)


	  (*****************************************************************)
    
      | Set () ->      (* Here we handle value 
		        affectations and pointer 
			affectations*)
     
      |  Call( Some(lvo) , exp1, lparam , _ )->
	begin
	  	Self.debug ~level:0 " I have a call with some affectation to a variable \n" ;
	      match lvo , exp1.enode with
		  ((Var(v),_) , Lval((Var(f),_)) ) ->
		    begin
		       	Self.debug ~level:0 "\n Dans Call de %s=%s \n" v.vname f.vname ;
		      match v.vtype with
			  (*Returned value has an integer type*)
			   
			  TPtr(_,_)->
			    begin
			      match f.vname with
				  "malloc" | "calloc" -> (malloc_upon_ssl (Some(v)) mid sslf)
				|  _ -> () (** Plug other functions name
					   that behaves as malloc in this 
					   space*)
			    end
			 (*The returned value is a variable that has another
			 type than an integer type. Tpointer, float for instance*)

			| _ -> () (** Here the formula is let untouched*)
		    end
		| _ -> () (** Here the returned value is not a variable,
			  the returned value shall be logged in this case.*)
	  end


      |  Call( None , exp1, lparam , _ )->
		Self.debug ~level:0 "I've got a call with no affectation of the returned value \n" ;
	begin
	  match  exp1.enode  with
	      Lval((Var(f),_))->
		begin
		 	Self.debug ~level:0  "Called function is %s \n" f.vname ; 
		  match f.vname with
		      "free" -> 
			begin
			try 
			  let pv = get_first_ptvar_from_lparam lparam in
			  match pv with
			      PVar (vname) ->
				begin
				 	Self.debug ~level:0  "Pvar name is : %s \n" vname ;
				  free_upon_ssl pv sslf 
				end			
			with
			    No_pvar_in_free_expression -> 
			      set_heap_to_top sslf
			end
		    | "malloc" | "calloc" -> (malloc_upon_ssl  None mid sslf)
		    | _ -> () (** All other function name that are dropped leads 
			       here*)
		end
	    | _ -> () (** Here the formula is let untouched*)
	end

      | _ -> () (** This is the default case, that's to say when
		    the parsed operation doesn't match the semantics.
		    At this point, we shall add some relevant information
		    dealing with the abstracted part of the ast.
		*)







(** Modifies the ssl formual that abstracts the current stack
and heap. 
The parameter mid shall be an instance of the global_mem_manager class.
 *)

let next_on_ssl (mid : global_mem_manager ) (sslf : ssl_formula ) (skind : Cil_types.stmtkind ) _  =
  match skind with 
      Instr ( instruction ) ->  next_on_ssl_instr  mid sslf instruction
    | _ -> ()


(**  We are mostly considering pointer modfications and affectations in this
function.*)
(*
let next_on_affectations  ( sslf :ssl_formula) ( instruction : Cil_types.instr) =
  match instruction with 
     Set( (Var(vinfo), _ ) , exp , _ ) ->
  
*)	  










    
    
  
  
