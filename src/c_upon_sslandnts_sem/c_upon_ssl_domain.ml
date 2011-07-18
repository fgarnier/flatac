(** This file  contains aims encodes the semantic of some C-functions
over the SSL formula. Those one are currently hard coded, which is
not satisfactory in a long term view.

Question & remarks : Address to florent dot garnier AT imag dot fr.
*)
open Cil_types
open Ssl_types
open SSL_lex
open Ssl
open Ssl_decision
open Debug_printers
open Global_mem
open List


exception No_pvar_in_free_expression



(** This function aims at getting the first variable name
of the list of parameters. Might be useful if some parameter
expressions are prefixed by a cast or any other ugly stuff so
pecuiliar to the C-language.
*)


let rec get_first_ptvar_from_lparam ( lparam : Cil_types.exp list ) =
 match lparam with 
     [] -> raise No_pvar_in_free_expression
   | h::l' ->
	 begin
	   match h.enode with
	       (Lval(Var(varinf),_)) -> (PVar(varinf.vname))
	     | _  -> get_first_ptvar_from_lparam l' 
	 end
	   

(** This function modifies the sslf formula that abstracts the current
heap and stack when a call to malloc is performed.*)
let malloc_upon_ssl  ( v : Cil_types.varinfo option )  mid  (sslf : ssl_formula ) =
  match v with Some (vinfo) ->
    let lvar = mid#lvar_from_malloc in
    let pvar = (PVar(vinfo.vname)) in
    let affect = Pointsto (pvar,lvar) in
    Ssl.add_quant_var lvar sslf;
    Ssl.and_atomic_affect affect sslf;
    Ssl.add_alloc_cell lvar sslf
      
    | None ->
       let lvar = mid#lvar_from_malloc in
       (Ssl.add_alloc_cell lvar sslf)
	 
(** Effect of a free(x),  where x is a pointer variable, on an ssl
formula.*)
let free_upon_ssl (pvar : ptvar)(sslf : ssl_formula) =
  let lvar = get_ptr_affectation sslf pvar in
  if not (space_contains_locvar lvar  sslf.space )
  then set_heap_to_top sslf 
  else
    try_remove_segment lvar sslf



(** mid must be an instance of the class global mem manager*)
let next_on_ssl_instr  mid ( sslf :ssl_formula) ( instruction : Cil_types.instr) =
    match instruction with 
	  (*****************************************************************)
	
       
	  (*   We consider here the call of function that have an impact
	  on the heap and the stack, namely :
	       _malloc & calloc
	       _free
	  *)


	  (*****************************************************************)
	  
      |  Call( Some(lvo) , exp1, lparam , _ )->
	  begin
	      match lvo , exp1.enode with
		  ((Var(v),_) , Lval((Var(f),_)) ) ->
		    begin
		      match v.vtype with
			  (*Returned value has an integer type*)
			  TPtr(TInt(_,_),_)->
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
	begin
	  match  exp1.enode  with
	      Lval((Var(f),_))->
		begin
		match f.vname with
		    "free" -> free_upon_ssl ( get_first_ptvar_from_lparam lparam) sslf 
		  | "malloc" | "calloc" -> (malloc_upon_ssl  None mid sslf)
		  | _ -> (** All other function name that are dropped leads 
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

let next_on_ssl mid (sslf : ssl_formula ) (skind : Cil_types.stmtkind ) ()  =
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










    
    
  
  
