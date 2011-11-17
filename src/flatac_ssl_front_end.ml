(**
This file specializes the generic front end, in order to extract a
counter automata based model which nodes are labelled with values
of the SSL logic. 
*)
(*open Intermediate_language*)

exception IndexOfCompositeTypesNotSet

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
open Nts
open Global_mem
 (* This type defines 2-uples of ssl_formual and
			    a validity_loc_map*)
open  Ssl_valid_abs_dom_types
open  Ssl_valid_abs_dom (*Contains the copy_validity_absdomain function*)

open Composite_type_types
open Composite_types (* Type definition for analysing C language composite
		     types, such as structures and enumerations*)
open Composite_type_visitors
open Visitor (* Frama-c visitors*)

open Self

let pprint_trans_list_foldleft (s : string ) ( trans : cnt_trans_label ) =
  match s with 
      "" -> s^(cnt_pprint_translabel trans )
    | _ -> s^" and "^(cnt_pprint_translabel trans )
  


class ssl_flatac_front_end = object 
  inherit [ssl_validity_absdom , Nts_types.cnt_trans_label list ]  sem_and_logic_front_end
   

  val mid =  (new global_mem_manager )

  val mutable index_of_pointers_of_composite_types =
    Composite_types.create_index_of_composite_types
  val mutable index_of_composite_types_set = false
    
  method set_index_of_composite_types ( i : index_of_composite_types ) =
    index_of_pointers_of_composite_types <- 
      Composite_types.copy_index_of_composite_types i ;
    index_of_composite_types_set <- true
      
  method get_empty_transition_label () =
    []

  method get_entry_point_abstraction () =
     Ssl_valid_abs_dom.create_validity_abstdomain
 
  method get_entry_point_from_fundec ( funinfo : Cil_types.fundec ) =
    if ( not index_of_composite_types_set  ) then
      raise IndexOfCompositeTypesNotSet
    else
      let absdom = Ssl_valid_abs_dom.create_validity_abstdomain in
      absdom.composite_types_infos <- index_of_pointers_of_composite_types ; 
      let absdom = Ssl_valid_abs_dom.register_slocals mid funinfo absdom in
      Ssl_valid_abs_dom.register_sformals mid funinfo absdom

(*
  method get_entry_point_from_fundec_and_type_infos 
    (funinfo : Cil_types.fundec) (i : index_of_composite_types ) =
    index_of_composite_type_set <- true;
    index_of_pointers_of_composite_types <- 
      copy_index_of_composite_types i;
    self#get_entry_point_from_fundec funinfo
*)    
      
  (*
    method copy_transit_label (label : Nts_types.cnt_trans_label list ) =
    [] *)
	
  method copy_transit_label _ =
    []

  
  method copy_absdom_label (absval : ssl_validity_absdom ) =
     copy_validity_absdomain absval 
      

  method is_error_state (sslv : ssl_validity_absdom ) =
    match sslv.ssl_part.space with
	Top_heap -> true
      | _ -> 
	  begin 
	    if (Ssl_decision.sat_ssl sslv.ssl_part)
	    then  ( (Ssl_decision.garbage_ssl sslv.ssl_part) )
	    else false
	  end
	    

  method pretty (sslv : ssl_validity_absdom ) =
    Ssl_pprinters.pprint_ssl_formula_tex sslv.ssl_part

    
  method next (sslv : ssl_validity_absdom ) _
    (skind : Cil_types.stmtkind) =
   (** we now need to copy the current sslf_formula of sslv. 
       validinfo is not a persistant structure, as it is based
       upon a standard library Map.
   *)
    let sslv_local =Ssl_valid_abs_dom.copy_validity_absdomain sslv in
    C_upon_ssl_domain.next_on_ssl_nts mid sslv_local skind (* translist;*) 
    
    

  (* Pretty prints each elements of tlist and concatenates it on the
  returned string.*)
  method pretty_label tlist = 
    let  str = List.fold_left pprint_trans_list_foldleft "" tlist in
      str
    
  method entails sslvg sslvd =
     let etp = {
      left = sslvd.ssl_part ;
      right = sslvg.ssl_part;
    }
    in
     (Ssl_entailement.does_entail etp )
     (*not (accept_new_abstraction etp)*)


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
    (*Ssl_entailement.accept_new_abstraction etp*)

end


