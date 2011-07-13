(**
This file specializes the generic front end, in order to extract a
counter automata based model which nodes are labelled with values
of the SSL logic. 
*)
open Intermediate_language
open Cautomata
open Cil_types
open Sem_and_logic_front_end
open Ssl_types
open Cautomata
open Ssl_decision
open Ssl_printers
open C_upon_ssl_domain (* Contains the semantic tranformation
		       of the C instruction on the SSL formulae*)


class ssl_flatac_front_end = object (self)
 (* inherit [SSL_lex.formula , Cautomata.trans_label list ]  sem_and_logic_front_end*)
    
  inherit [SSL_lex.formula , () ]  sem_and_logic_front_end 

  val gmalloc_id = ref 1

  method get_empty_transition_label () =
    []

  method get_entry_point_abstraction () =
    Ssl_lex.create_ssl_f ()

  method is_error_state (sslf : ssl_formula ) =
    match ssl_formula.space with
	Top_heap -> true
      | _ -> not (Ssl_decision.sat_ssl sslf)


  method pretty (sslf : ssl_formula ) =
    Ssl_pprinters.pprint_ssl_formula sslf

    (*  
  method next (sslf : ssl_formula)()(skind : Cil_types.stmtkind) =
    *)

      
    

    
    
end


