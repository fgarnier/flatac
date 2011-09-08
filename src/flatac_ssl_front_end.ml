(**
This file specializes the generic front end, in order to extract a
counter automata based model which nodes are labelled with values
of the SSL logic. 
*)
(*open Intermediate_language*)
open Cil_types
open Sem_and_logic_front_end 
open Ssl_types
open SSL_lex
open Ssl_entailement
open Ssl_decision
open Ssl_pprinters 
open C_upon_ssl_domain (* Contains the semantic tranformation
		       of the C instruction on the SSL formulae*)
open Nts_types
open Global_mem


open Self

class ssl_flatac_front_end = object 
  inherit [SSL_lex.formula , Nts_types.cnt_trans_label list ]  sem_and_logic_front_end
   

  val mid =  (new global_mem_manager )

  method get_empty_transition_label () =
    []

  method get_entry_point_abstraction () =
    Ssl.create_ssl_f ()

  method is_error_state (sslf : ssl_formula ) =
    match sslf.space with
	Top_heap -> true
      | _ -> not (Ssl_decision.sat_ssl sslf)


  method pretty (sslf : ssl_formula ) =
    Ssl_pprinters.pprint_ssl_formula_tex sslf

    
  method next (sslf : ssl_formula)(translist : trans_lanel list)
    (skind : Cil_types.stmtkind) =
    let sslf_local = Ssl.copy sslf in
    C_upon_ssl_domain.next_on_ssl mid sslf_local skind trans_list;
    (sslf_local , ())::[]
    

 
  method pretty_label () = ""
    
  method accepts sslfg sslfd =

    (** One checks that the current abstraction entails the next state
    abstraction, where the current state abstraction is sslfd and
    the next state abstraction is sslfg (Inverted order) *)
   
    let etp = {
      left = sslfd ;
      right = sslfg;
    }
    in
    not (Ssl_entailement.does_entail etp )

end


