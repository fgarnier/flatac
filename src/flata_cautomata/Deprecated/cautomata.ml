(**  This module contains object and functions used to implement and transform counter automata. The design and implementation choices are influenced by the
fact that this module is designed to be used by some FRAMA-C modules 

Verimag laboratory.
For any question or remark, please contact florent.garnier@imag.fr

*)

open Hashtbl
open List
open Format
open Intermediate_language


exception Negative_value
exception State_name_already_defined



(*****************************************************************************)
(*                             Type Declaration                              *)
(*                                                                           *)
(*****************************************************************************)




(* the type control_state is used to model any counter automaton control state. Additional
information might be added, to refine code analysis, i.e. line number, infos concerning
value type (pointer, etc ...)  *)

(*SI stands for State Information*)

type state_info_type = SINote of string
		       | SIAssertion of string
		       | SIBAssertion of c_bool
    
type state_info = SInfo of state_info_type 
		  |SINone

(*type state_info = string*)
		 
type control_state = Control of string * state_info 

(*Maybe another type than string would be a better choice 
 Identifiers are either primed or unprimed. We a constructor
to make a distinction between the new value (primed) and the
former value (unprimed) of the same identifier during a transition.
*)

(*
type id = Prime of string 
	 | Unprimed of string
*)
(*type id = Id of baseid*) 
type number = Num of int (* number must be an unsigned value. Must be cheched
		  during the construction of the automaton, or specified
		  with type unsigned int*)



(** type of a constainst/expression **)


type trans_label = Guard of c_bool
	     | FunCall of string * c_scal list
	     | Affect_int of c_int_var * c_scal
	     | Affect_int_ptr of c_int_ptr * c_scal
	     



type transition = { id : string ; orig : control_state ; dest : control_state ; listlabel : trans_label list }   


(** Type state_table_element define the data structure used to model a controle state of a counter automaton, it also contains the list of the Id of each incomming, resp. outcomming, transitions *)

type state_table_element = { control : control_state ; mutable incoming : string list; mutable outcoming : string list}



 (******************************************************************)

(*            Version 0.1 pretty printing                         *)

(**************************************************************)


let rec scalar_list_to_string (list : c_scal list ) =
  let i = ref 1 in
  let j = List.length list in
  let ret = ref "" in
  List.iter ( fun s -> ret:= !ret ^(Intermediate_language.scal_to_string  s); if !i<j then ret:=!ret^","; i:= !i + 1) list;
   !ret



let rec pretty_print_label_list (out_channel : Format.formatter) (transition_list : trans_label list ) =
  match transition_list with
      [] -> Format.fprintf out_channel "}" (*Prints closing brackets *)
	
    | Guard ( g )::l ->  
      Format.fprintf out_channel "%s" (Intermediate_language.c_bool_to_string g ); 
      if List.length l > 0 then  Format.fprintf out_channel ",";
      pretty_print_label_list out_channel l
    
    | FunCall( fun_name , scalar ):: l -> 
      Format.fprintf out_channel "%s(%s)" fun_name ( scalar_list_to_string scalar );
      if List.length l > 0 then  Format.fprintf out_channel ",";
      pretty_print_label_list out_channel l
    (* If the function returns a value, that is affected to a var, e.g. x, 
then x must appear as x' at the end of the parameter list*)

    | Affect_int ( LiIntVar(s) , scalar )::l -> 
     fprintf out_channel "%s" (s^"'"^"="^(scal_to_string scalar ));  
      if List.length l > 0 then  Format.fprintf out_channel ",";
      pretty_print_label_list out_channel l

    | _ :: l ->  pretty_print_label_list out_channel l

 
(*
       | Affect_int_ptr ( c_int_ptr , c_scal )

*)



(***************************************************************)

(*     End of version 0.1 pretty printing *)

(***************************************************************)


(*     A function that pretty prints a transiton       *)

 let pretty_print_transition (out_channel: Format.formatter) (trans : transition  ) =
   match trans.orig , trans.dest with
       Control(org, _ ) , Control (dest , _ )->
	 Format.fprintf out_channel "%s: %s->%s {" trans.id org dest;  
	 pretty_print_label_list out_channel trans.listlabel







(****************************************************************************)

(*                  Definition of class cautomaton                          *)

(****************************************************************************)


(** This class defines an object type that allows to store and operate manipulation on couter automata.
Namely :
 _ Defining a set of initial and a set of final control state.
 _ Storing the set of transitions.
 _ Computing the set of control state from the set of transitions.
 _ Adding a transition.
 _ Pretty printing a counter automaton into FLATA syntax.
*)

class cautomaton name_id = object(self)
(** Class Cautomaton defines the type of the object used to represent
a counter automate. The interface that follows allows to add both initial
and terminal state as well as transitions *)

  val mutable name = ""
  (** The two fields that follows are used to represent the set
      of control state and the set of transition of a counter automaton*)  
  val  transitions = ( Hashtbl.create 97 : ( string , transition) t)
  val  control_states =  Hashtbl.create 97 (*: ( string , state_table_element) t *)

 (** Here are both stored the reference to the set of initial and
     final states *)
  val initial_states_id = (Hashtbl.create 97 : ( string, unit ) t )
  val final_states_id = (Hashtbl.create 97  : ( string, unit) t )
 (** This list contains the set of the Id of the states that are neither
     initial nor terminal*)
  val common_states_id =( Hashtbl.create 97 : (string, unit ) t )
  val mutable is_init = false 
  

  initializer name <- name_id

  method set_name (name_id : string) =
    name <- name_id

 (** This method is used to try add an initial state. Return true if the
 control state is not referenced, returns false otherwise. The two methods
that follow add_init_state perform the same task, for the final and common states *)
  method add_init_state s =
    match s with 
	Control (name_id , si ) -> if (self#check_id_availability name_id) then
	    begin 
	      Hashtbl.add initial_states_id name_id ();
	      let cs = { control = Control ( name_id , si) ; incoming = [] ; outcoming = []} in Hashtbl.add control_states name_id cs; 
	      true
	   end
	  else false 

(** Perform the same task as the function above, but doesn't return true/false on success/failure
of adding the state s.*)

  method add_init_state_u s=
     match s with 
	Control (name_id , si ) -> if (self#check_id_availability name_id) then
	    begin 
	      Hashtbl.add initial_states_id name_id ();
	      let cs = { control = Control ( name_id , si ) ; incoming = [] ; outcoming = []} in Hashtbl.add control_states name_id cs; 
	   end
	  else ()



 method add_final_state s  = 
    match s with 
	Control (name_id , si ) -> if (self#check_id_availability name_id) then
	    begin 
	      Hashtbl.add final_states_id name_id ();
	      let cs = { control = Control ( name_id , si) ; incoming = [] ; outcoming = []} in Hashtbl.add control_states name_id cs; 
	      true
	   end
	  else false 

(** Same as function above, but returns nil*)
 method add_final_state_u s  = 
    match s with 
	Control (name_id , si ) -> if (self#check_id_availability name_id) then
	    begin 
	      Hashtbl.add final_states_id name_id ();
	      let cs = { control = Control ( name_id , si) ; incoming = [] ; outcoming = []} in Hashtbl.add control_states name_id cs; 
	   end
	  else () 



method add_set_final_state_u s  = 
    match s with 
	Control (name_id , si ) -> if (self#check_id_availability name_id) == false
	then 
	  begin
	    if Hashtbl.mem common_states_id name_id then
	      Hashtbl.remove common_states_id name_id;
	    Hashtbl.add final_states_id name_id () 
	  end
	else
	  Hashtbl.add final_states_id name_id ();
	  let cs = { control = Control ( name_id , si) ; incoming = [] ; outcoming = []} in Hashtbl.add control_states name_id cs; 

 


 method private add_common_state s = 
    match s with 
	Control (name_id , si ) -> if (self#check_id_availability name_id) then
	    begin 
	      Hashtbl.add common_states_id name_id ();
	      let cs = { control = Control ( name_id , si) ; incoming = [] ; outcoming = []} in Hashtbl.add control_states name_id cs; 
	      true
	   end
	  else false 

	    

 method check_id_availability (str : string ) =
	    if ( Hashtbl.mem initial_states_id str  ) then false
	    else if ( Hashtbl.mem final_states_id str  ) then false
	    else  if ( Hashtbl.mem common_states_id str  ) then false
	    else true 
 (** Truc suspect : Pourquoi ne puis-je pas calculer
     !(Hashtbl.mem common_states_id str) alors que
     ça s'évalue à Bool.    
 *)
 



(*
The method add_in_trans adds to a state_table_element the string corresponding 
to the id of an incoming transition
type state_table_element = { control : control_state ; mutable incoming : string list; mutable outcoming : string list} *)

(* method add_in_trans trans id_origin =
   let state = Hashtbl.find control_states id_origin;
   state.incoming <- trans.id :: state.incoming 
*)

 (* Adds a transition to the set of transitions. Must add any control state
that is not yet refrenced in the initial, final and common set of id.*)


 method add_transition (trans :  transition ) = 
   match  trans.id , trans.orig , trans.dest  with 
       ( tid , Control( corg , si_org ) , Control ( dest , si_dest )) -> if ( Hashtbl.mem transitions tid ) then false (* Returns false if a transition with
			    the same label is already registered *) 
 
	  (*One try to add the control states to the set of common states. 
	    We anote the states with the Id of the outgoing and incomming
	    transition, in the following cases : The case when the control
	    state need to be registered first and in the case it is already
	    registered.
	  *)
	 else
	   let state_orig = Control(corg, si_org) in
	   let state_dest = Control(dest, si_dest) in 
	     if (self#add_common_state state_orig  )
	     then begin 
	       let state_orig = Hashtbl.find control_states corg in
		 state_orig.outcoming <- trans.id :: state_orig.outcoming
	     end;
	     if ( self#add_common_state state_dest ) 
	     then  
	       begin
	       let state_dest = Hashtbl.find control_states dest in
		  state_dest.incoming <- trans.id :: state_dest.incoming
	       end;
	     Hashtbl.add transitions tid trans; 
	       true
	   
	 
        (*Here the operation is successfuly completed*)
	  

(**Try to add a transition, without notifying wheter the transition has been successfuly added
or not. The return value is nil.*)
 method add_transition_u (trans : transition) = 
   match  trans.id , trans.orig , trans.dest  with 
       ( tid , Control( corg , si_org ) , Control ( dest , si_dest )) -> if ( Hashtbl.mem transitions tid ) then () (* Returns f if a transition with
			    the same label is already registered *) 
 
	  (*One try to add the control states to the set of common states. 
	    We anote the states with the Id of the outgoing and incomming
	    transition, in the following cases : The case when the control
	    state need to be registered first and in the case it is already
	    registered.
	  *)
	 else
	   let state_orig = Control(corg,si_org) in
	   let state_dest = Control(dest,si_dest) in 
	     if (self#add_common_state state_orig  )
	     then begin 
	       let state_orig = Hashtbl.find control_states corg in
		 state_orig.outcoming <- trans.id :: state_orig.outcoming
	     end;
	     if ( self#add_common_state state_dest ) 
	     then  
	       begin
	       let state_dest = Hashtbl.find control_states dest in
		  state_dest.incoming <- trans.id :: state_dest.incoming
	       end;
	     Hashtbl.add transitions tid trans;  
	       
	   (** Et voilà*)



(*This methods prints/drops the content of the automaton in the channel out_channel, using FLATA syntax *)



 method pretty_print_final_states (out_channel : Format.formatter ) =
   let i =  ref 0 in
   Format.fprintf out_channel "final {";
   let length = Hashtbl.length final_states_id in
   Hashtbl.iter (fun name_state () -> if ( !i < (length-1)) 
     then Format.fprintf out_channel "%s," name_state 
     else Format.fprintf out_channel "%s" name_state; i:=!i+1 ) final_states_id;
   Format.fprintf out_channel "}\n"
   
method pretty_print_initial_states (out_channel : Format.formatter ) =
   let i =  ref 0 in
   Format.fprintf out_channel "initial {";
   let length = Hashtbl.length final_states_id in
   Hashtbl.iter (fun name_state () -> if ( !i < (length-1)) 
     then Format.fprintf out_channel "%s," name_state 
     else Format.fprintf out_channel "%s" name_state; i:=!i+1 ) initial_states_id;
   Format.fprintf out_channel "}\n"

 method pretty_print out_channel =
   Format.fprintf out_channel "automaton %s { @[ \n" name;
   Format.fprintf out_channel "initial {";
   let length= Hashtbl.length initial_states_id in
   let i = ref 0 in
   Hashtbl.iter (fun name_state () -> if ( !i < (length-1)) 
     then Format.fprintf out_channel "%s," name_state 
     else Format.fprintf out_channel "%s" name_state; i:=!i+1 ) initial_states_id;
   Format.fprintf out_channel "}\n"; (* initial{...} set bracket closed, new line*) 
   
   Format.fprintf out_channel "final {";
   i:=0;
   let length = Hashtbl.length final_states_id in
   Hashtbl.iter (fun name_state () -> if ( !i < (length-1)) 
     then Format.fprintf out_channel "%s," name_state 
     else Format.fprintf out_channel "%s" name_state; i:=!i+1 ) final_states_id;
   Format.fprintf out_channel "}\n"; (* final{...} set bracket closed, new line*)

 (** local variable trans_name is not used for real. is only here to not generate a warning, avoiding the building process.
(** Hack déguelasse à dégager ASAP, (fun s-> if s=s then () else ()) trans_name ) *)
*)
   Hashtbl.iter (fun trans_name trans -> pretty_print_transition out_channel trans; Format.fprintf out_channel "\n"; (fun s-> if s=s then () else ()) trans_name )  transitions; (* One transition per line *)

   Format.fprintf out_channel "}\n"  (* atomaton{...} set bracket closed*)
  

  

end ;;
