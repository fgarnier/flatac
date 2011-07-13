
open Cil_types
open Ssl_types
open Ssl_decision
open Ssl_printers






(** Modifies the ssl formual that abstracts the current stack
and heap.  *)
let next_on_ssl (sslf : ssl_formula ) (skind : Cil_types.stmtkind ) =
  match skind with 
      Instr ( instruction ) ->  next_on_ssl_instr sslf instruction
    | _ -> ssl_formula 


let next_on_ssl_instr (sslf :  ssl_formula) ( instruction : Cil_types.instr)=
    match instruction with 
	  (*****************************************************************)
	
       
	  (*   We consider here the call of function that have an impact
	  on the heap and the stack, namely :
	       _malloc
	       _free
	  *)


	  (*****************************************************************)
	  

      |  Instr(Call( Some(lvo) , exp1, lparam , _ ))->
	  begin
	      match lvo , exp1.enode with
		  ((Var(v),_) , Lval((Var(f),_)) ) ->
		    begin
		      match v.vtype with
			  (*Returned value has an integer type*)
			  TPtr(TInt(_,_),_)->
			    begin
			      
			    end
			 (*The returned value is a variable that has another
			 type than an integer type. Tpointer, float for instance*)

			| _ ->  self#register_failure stmtp ( "returned value 
is not of an integer type." )  
		    end
	  end

      |  Instr(Call( None , exp1, lparam , _ ))->
	
