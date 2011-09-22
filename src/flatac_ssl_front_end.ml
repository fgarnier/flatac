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
 (* This type defines 2-uples of ssl_formual and
			    a validity_loc_map*)
open  Ssl_valid_abs_dom_types
open  Ssl_valid_abs_dom (*Contains the copy_validity_absdomain function*)

open Self

class ssl_flatac_front_end = object 
  inherit [ssl_validity_absdom , Nts_types.cnt_trans_label list ]  sem_and_logic_front_end
   

  val mid =  (new global_mem_manager )

  method get_empty_transition_label () =
    []

  method get_entry_point_abstraction () =
     Ssl_valid_abs_dom.create_validity_abstdomain

  method is_error_state (sslv : ssl_validity_absdom ) =
    match sslv.ssl_part.space with
	Top_heap -> true
      | _ -> not (Ssl_decision.sat_ssl sslv.ssl_part)


  method pretty (sslv : ssl_validity_absdom ) =
    Ssl_pprinters.pprint_ssl_formula_tex sslv.ssl_part

    
  method next (sslv : ssl_validity_absdom )(translist : Nts_types.cnt_trans_label list)
    (skind : Cil_types.stmtkind) =
   (** we now need to copy the current sslf_formula of sslv. 
       validinfo is not a persistant structure, as it is based
       upon a standard library Map.
   *)
    let sslv_local =Ssl_valid_abs_dom.copy_validity_absdomain sslv in
    C_upon_ssl_domain.next_on_ssl_nts mid sslv_local skind; (* translist;*) 
    (sslv_local , ())::[]
    

 
  method pretty_label () = ""
    
  method accepts sslvg sslvd =

    (** One checks that the current abstraction entails the next state
    abstraction, where the current state abstraction is sslfd and
    the next state abstraction is sslfg (Inverted order) *)
   
    let etp = {
      left = sslvd.ssl_part ;
      right = sslvg.ssl_part;
    }
    in
    not (Ssl_entailement.does_entail etp )

end


