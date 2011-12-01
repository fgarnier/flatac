(**  This module contains object and functions used to implement and transform counter automata. The design and implementation choices are influenced by the
fact that this module is designed to be used by some FRAMA-C modules 

Verimag laboratory.
For any question or remark, please contact florent.garnier@imag.fr

*)

open Hashtbl
open List
open Format



exception Negative_value
exception State_name_already_defined



(*****************************************************************************)
(*                             Type Declaration                              *)
(*                                                                           *)
(*****************************************************************************)

type primed = Primed
              | Unprimed
type c_int_var = LiIntVar of string
type c_int_cst = LiIConst of int
type c_int_sym_const = LiSymIConst of string
                     
type c_ptr = LiIntPtr of string (*The represented type is indeed an int*)


(* We define the type of  the scalars as follows.
basicaly, the scalar we consider in this language are all the scalars
that are the evaluation of arithmetic
expressions which ground terms are free integer variables
and integers constants.

*)

exception Bad_expression_type of string

(** The type of integers scalar expressions*)
type c_scal = LiVar of primed * c_int_var
              | LiConst of c_int_cst
              | LiSymConst of c_int_sym_const  (*Like sizeof of types or 
                                               defined constant *)
              | LiProd of c_scal * c_scal
              | LiSum of c_scal * c_scal
              | LiMinus of c_scal * c_scal
              | LiUnMin of c_scal
              | LiMod of c_scal * c_scal   (*Modulo operator*)
              | LiMinusPP of c_ptrexp * c_ptrexp *  Cil_types.typ
              | LiScalOfAddr of c_ptrexp * Cil_types.typ 
                                        (* While casting a ptr to an 
                                         integer type*) 
                  
and c_ptrexp = LiPVar of primed * c_ptr *  Cil_types.typ
               | LiPlusPI of c_ptrexp * c_scal  * Cil_types.typ
               | LiIndexPI of c_ptrexp * c_scal * Cil_types.typ
               | LiMinusPI of c_ptrexp * c_scal * Cil_types.typ
               | LiAddrOfScal of c_scal * Cil_types.typ


type il_expr = IlScal of c_scal
               | IlPtr of c_ptrexp
               
type c_bool = LiBNot of c_bool 
              | LiBAnd of c_bool * c_bool 
              | LiBOr of c_bool * c_bool  
              | LiBTrue
              | LiBFalse
              | LiBEq of c_scal * c_scal
              | LiBNeq of c_scal * c_scal
              | LiBLt of c_scal * c_scal
              | LiBGt of c_scal * c_scal
              | LiBLeq of c_scal * c_scal
              | LiBGeq of c_scal * c_scal
              | LiBScal of c_scal (* true iff != 0 *)
              | LiBPtrEq of c_ptrexp *  c_ptrexp
              | LiBPtrNeq of c_ptrexp * c_ptrexp
              | LiBPtrGt of c_ptrexp * c_ptrexp
              | LiBPtrLt of c_ptrexp * c_ptrexp
              | LiBPtrGeq of c_ptrexp * c_ptrexp
              | LiBPtrLeq of c_ptrexp * c_ptrexp



let rec scal_to_string ( b_exp : c_scal ) =
  match b_exp with
      LiVar(Unprimed,LiIntVar(vname)) -> vname (* returns the name of the variable*)
    | LiVar(Primed,LiIntVar(vname)) -> vname^"'"
    | LiConst(LiIConst(i)) -> (Printf.sprintf "%d" i )
    | LiSymConst(LiSymIConst(const_name)) -> const_name
    | LiScalOfAddr(e , _)->"(TINT of Addr cast)"^(ptrexp_to_str e)
    | LiProd( sg , sd ) ->
      let rhs= ref "" in
      let lhs = ref "" in
      begin
        match sg with
            LiSum (_,_) | LiMinus (_,_) -> rhs := "("^(scal_to_string  sg)^")"
          | _ -> rhs := (scal_to_string  sg)
      end;
      begin
         match sd with
            LiSum (_,_) | LiMinus (_,_) -> lhs := "("^(scal_to_string  sd)^")"
          | _ -> lhs := (scal_to_string  sd)
      end;
      (!lhs)^"*"^(!rhs) (*Returned value*)
        (*End of the LiProd pretty print*)

    | LiSum ( sg , sd ) -> (scal_to_string sg) ^"+" ^ (scal_to_string sd)
    | LiMinus( sg , sd ) ->
     begin
      match sd with
          LiConst(_) | LiSymConst (_) | LiVar(_,_) ->
            (scal_to_string sg) ^"-" ^ (scal_to_string sd)
        | _ ->(scal_to_string sg )^"-("^(scal_to_string sd)^")"
     end

    | LiUnMin ( s ) -> "-"^(scal_to_string s)

    | LiMod ( sg , sd ) ->  (scal_to_string sg)^"%"^(scal_to_string sd)

    | LiMinusPP (ptrg , ptrd, _ ) ->
      "("^( ptrexp_to_str  ptrg )^"-"^ ( ptrexp_to_str  ptrd )^")"

and ptrexp_to_str ( cptr : c_ptrexp ) =
  match cptr with

      LiPVar ( Primed , LiIntPtr ( vname), _ ) ->
        vname^"'"

    | LiPVar ( Unprimed , LiIntPtr ( vname ), _) ->
      vname

    |  LiAddrOfScal (e , _) -> "(Addr of TINT)"^(scal_to_string e)

    | LiPlusPI ( ptr_in , offset, _ ) ->
      ( ptrexp_to_str  ptr_in )^"["^(scal_to_string offset)^"]"

    | LiIndexPI ( ptr_in , offset , _ ) ->
       ( ptrexp_to_str  ptr_in )^"["^(scal_to_string offset)^"]"

    | LiMinusPI (ptr_in , offset , _ ) ->
      ( ptrexp_to_str  ptr_in )^"["^(scal_to_string offset)^"]"



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
	     | Affect_int_ptr of c_int_var * c_scal
	     



type transition = { id : string ; orig : control_state ; dest : control_state ; listlabel : trans_label list }   


(** Type state_table_element define the data structure used to model a controle state of a counter automaton, it also contains the list of the Id of each incomming, resp. outcomming, transitions *)

type state_table_element = { control : control_state ; mutable incoming : string list; mutable outcoming : string list}



 (******************************************************************)

(*            Version 0.1 pretty printing                         *)

(**************************************************************)


(** One need to make sure that the output syntax complies with the NTS-lib
syntax.*)
let rec c_bool_to_string ( b_exp :  c_bool) =
  match b_exp with
        LiBNot ( b ) ->
          begin
            match b with
                (* Here we manage not to add a Lot of Insane and Stupid Parentheses*)
                LiBAnd (_,_) | LiBOr (_,_) | LiBNeq (_,_)
              | LiBEq (_,_) | LiBLt (_,_)
              | LiBGt(_,_) | LiBGeq(_,_)
              | LiBLeq (_,_)|  LiBPtrLeq (_ , _)
              | LiBPtrGeq (_,_) | LiBPtrLt (_, _)
              | LiBPtrGt (_, _) | LiBPtrNeq (_, _)
                ->

"!"^(c_bool_to_string b)

              | _ -> "!("^(c_bool_to_string b)^")"
          end
    | LiBAnd ( b1 , b2 ) -> "("^(c_bool_to_string b1) ^"&&"^(c_bool_to_string b2) ^")"
    | LiBOr ( b1 , b2 ) ->  "("^(c_bool_to_string b1) ^"||"^(c_bool_to_string b2) ^")"
    | LiBTrue -> "1"
    | LiBFalse -> "0"
    | LiBEq( bg , bd) -> "("^(scal_to_string bg) ^"="^(scal_to_string bd) ^")"
    | LiBNeq ( bg , bd ) -> "("^(scal_to_string bg) ^"!="^(scal_to_string bd) ^")"
    | LiBLt( bg , bd ) ->  "("^(scal_to_string bg) ^"<"^(scal_to_string bd) ^")" 
    | LiBGt ( bg , bd ) ->  "("^(scal_to_string bg) ^">"^(scal_to_string bd) ^")"
    | LiBLeq ( bg , bd ) ->  "("^(scal_to_string bg) ^"<="^(scal_to_string bd) ^")"
    | LiBGeq ( bg , bd ) ->  "("^(scal_to_string bg) ^">="^(scal_to_string bd) ^")"
    | LiBScal ( c_scal ) ->
      "("^(scal_to_string c_scal)^")!=0"(* true iff != 0 *)

    | LiBPtrEq ( ptrg , ptrd ) ->
      "("^(ptrexp_to_str ptrg)^"="^(ptrexp_to_str ptrd)^")"

    | LiBPtrLeq ( eg , ed )  ->
      "("^(ptrexp_to_str eg )^"<="^(ptrexp_to_str ed )^")"

    | LiBPtrGeq  ( eg , ed )  ->
      "("^(ptrexp_to_str eg )^">="^(ptrexp_to_str ed )^")"

    | LiBPtrGt  ( eg , ed )  ->
      "("^(ptrexp_to_str eg )^">"^(ptrexp_to_str ed )^")"

    | LiBPtrLt ( eg , ed )  ->
      "("^(ptrexp_to_str eg )^"<"^(ptrexp_to_str ed )^")"

    | LiBPtrNeq ( ptrg , ptrd ) ->
      "("^(ptrexp_to_str ptrg)^"!="^(ptrexp_to_str ptrd)^")"

(** Call the function above and prints the output in the out formater.*)
let pretty_print_c_bool ( out_channel :Format.formatter) (b_exp : c_bool ) =
  Format.fprintf  out_channel "%s" (c_bool_to_string b_exp)






let rec scalar_list_to_string (list : c_scal list ) =
  let i = ref 1 in
  let j = List.length list in
  let ret = ref "" in
  List.iter ( fun s -> ret:= !ret ^(scal_to_string  s); if !i<j then ret:=!ret^","; i:= !i + 1) list;
   !ret



let rec pretty_print_label_list (out_channel : Format.formatter) (transition_list : trans_label list ) =
  match transition_list with
      [] -> Format.fprintf out_channel "}" (*Prints closing brackets *)
	
    | Guard ( g )::l ->  
      Format.fprintf out_channel "%s" (c_bool_to_string g ); 
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
  (** The two fields that follow are used to represent the set
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



   Hashtbl.iter (fun trans_name trans -> pretty_print_transition out_channel trans; Format.fprintf out_channel "\n"; (fun s-> if s=s then () else ()) trans_name )  transitions; (* One transition per line *)

   Format.fprintf out_channel "}\n"  (* atomaton{...} set bracket closed*)
  

  

end ;;
