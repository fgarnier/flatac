open Ast_info
open Int64
open Cautomata
open Format
open Hashtbl
open Lexing
open Cil_types
open List
open Printf
open Intermediate_language 


exception Mult_with_non_int_value of Cil_types.stmt

(*****************************************************************************)
(*

 Laboratoire Verimag, 2 Avenue de Vignate, 38610 Gières, France.


This file contains the functions and the classes used to :
_ Transform Cil statments and expressions into flata grammar/syntax
_ The definition of a counter automata used to abstract C programs,
defined as a specialized class of the generic cautomata class.


 For questions or remarks, contact florent.garnier__at__imag__dot__fr
**)
(*****************************************************************************)







 

(** Abstracts a C functions, contains the CFG and an abstraction
of variables valuation variations   *)

class c2cautomata_abstraction (funinfos : Cil_types.fundec) = object(self)
  inherit Cautomata.cautomaton (funinfos.svar.vname)
    (*    
	  val mutable global_vars = (Hashtbl.create 97 : ( string , Cil_types.varinfo ) t)
	  val mutable local_vars = (Hashtbl.create 97 : ( string , Cil_types.varinfo ) t)
    *) 
    
    
  val visited =(Hashtbl.create 97 : (string, unit) t) 
  val v_int_globals = (Hashtbl.create 97 : (string, unit) t)
  val v_int_formals = (Hashtbl.create 97 : (string, unit) t) (*Input*)
  val v_int_locals = (Hashtbl.create 97 : (string, unit) t) 
  val v_int_return = (Hashtbl.create 97 : (string, unit) t) (*Output values*)
  val error_states = (Hashtbl.create 97 : (string, unit) t)
  val table_stmt_loc =(Hashtbl.create 97 : (string, Cil_types.location) t)
 

  method mark_as_visited (key : string ) =
    Hashtbl.add visited key ()

  method is_visited (key : string ) =
    Hashtbl.mem visited key

  method clear_visited =
    Hashtbl.clear visited

  method add_error_state (id : string ) =
    if not (Hashtbl.mem error_states id ) then
      Hashtbl.add error_states id () 

  (* Calls fundec2auto upon initialize argument during the object instance
     creation*)
  (*This method is called during the initialization phase, in order to
    store the se of input/formal integers parameters.
  *)

  method private set_formals(funinfos:Cil_types.fundec ) =
    List.iter (fun s ->
		 match s.vtype with
		     TInt (IInt,_) -> Hashtbl.add  v_int_formals s.vname ()
		   | _ -> ()
	      )funinfos.sformals 

  method private set_locals(funinfos:Cil_types.fundec ) =
    List.iter (fun s ->
		 match s.vtype with
		     TInt (IInt,_) -> Hashtbl.add  v_int_locals s.vname ()
		   | _ -> ()		       
	      ) funinfos.slocals;
    if Hashtbl.mem v_int_locals  "__retres" then Hashtbl.add v_int_return   "__retres" ()
      (** This method is called by buildgraph, while iterating on the
	  list of a stmt successor. Oveload it to define a more refined
      *)
  method  add_trans_from_stmt (stmtp: Cil_types.stmt ) (iterande : Cil_types.stmt) =
    let t_id = Printf.sprintf "s%d_s%d" stmtp.sid iterande.sid in
    let torig = Printf.sprintf "s%d" stmtp.sid in
    let tdest = Printf.sprintf "s%d" iterande.sid in
    let ntrans = {id = t_id ; orig = Control (torig,SINone) ; dest = Control (tdest,SINone) ;listlabel = []} in
      self#add_transition_u  ntrans; Hashtbl.add table_stmt_loc torig (Ast_info.loc_stmt stmtp)
	
  method buildgraph (stmtp : Cil_types.stmt ) =
    List.iter ( self#add_trans_from_stmt stmtp ) stmtp.succs ;
    self#mark_as_visited (Printf.sprintf "s%d" stmtp.sid );
    if ( (List.length stmtp.succs) > 0 ) then
      List.iter (fun s ->
		   let sid = Printf.sprintf "s%d" s.sid in
		     if  ( self#is_visited sid ) == false then 
		       self#buildgraph s 
			 (* We must indeed not traverse already visited
			    nodes*)
		     else () 
		) stmtp.succs
    else
      let final_state_id = Printf.sprintf "s%d" stmtp.sid in 
	self#add_set_final_state_u ( Control( final_state_id ,SInfo(SINote("Terminal")) ))

  method fundec2cauto ( f : Cil_types.fundec ) =
    self#set_name f.svar.vname;
    let init_state = (List.nth f.sallstmts 0) in
    let init_state_id = Printf.sprintf "s%d" (init_state.sid) in
      self#add_init_state_u (Control(init_state_id, SInfo(SINote("Initial"))));
      self#buildgraph init_state 

  initializer self#fundec2cauto funinfos; self#set_formals funinfos; self#set_locals funinfos



  (**************************************************************************)

  (**  Pretty printing methods *)
    
  (**************************************************************************)

    
  method pretty_print_locals out_channel = 
    Format.fprintf out_channel "locals {";
    let i= ref 0 in
    let length = Hashtbl.length v_int_locals in
      Hashtbl.iter (fun name_state () -> if ( !i < (length-1)) 
		    then Format.fprintf out_channel "%s," name_state 
		    else Format.fprintf out_channel "%s" name_state; i:=!i+1 ) v_int_locals; 
      if ( (Hashtbl.length v_int_formals) > 0 ) then
	
	Format.fprintf out_channel ",";
      i := 0;
      let length = Hashtbl.length v_int_formals in
	Hashtbl.iter (fun name () -> if ( !i < (length-1)) 
		      then Format.fprintf out_channel "%s," name 
		      else Format.fprintf out_channel "%s" name; 
			i:=!i+1 ) v_int_formals;
	
	Format.fprintf out_channel "}\n"
	  

  method pretty_print_formals out_channel = 
    Format.fprintf out_channel "in (";
    let i= ref 0 in
    let length = Hashtbl.length v_int_formals in
      Hashtbl.iter (fun name () -> if ( !i < (length-1)) 
		    then Format.fprintf out_channel "%s," name 
		    else Format.fprintf out_channel "%s" name; 
		      i:=!i+1 ) v_int_formals;
      Format.fprintf out_channel ")\n" 


  method pretty_print_out_v out_channel = 
    Format.fprintf out_channel "out (";
    let i= ref 0 in
    let length = Hashtbl.length v_int_return in
      Hashtbl.iter (fun name () -> if ( !i < (length-1)) 
		    then Format.fprintf out_channel "%s'," name 
		    else Format.fprintf out_channel "%s'" name; 
		      i:=!i+1 ) v_int_return;
      Format.fprintf out_channel ")\n" 


  method pretty_print_error_states out_channel = 
    if Hashtbl.length error_states > 0 then
      begin 
	Format.fprintf out_channel "error {";
	let i= ref 0 in
	let length = Hashtbl.length error_states in
	  Hashtbl.iter (fun name () -> if ( !i < (length-1)) 
			then Format.fprintf out_channel "%s," name 
			else Format.fprintf out_channel "%s" name; 
			  i:=!i+1 ) error_states;
	  Format.fprintf out_channel "}\n" 
      end
	
  method pretty_print out_channel =
    Format.fprintf out_channel "automaton %s { @[ \n" name;
    self#pretty_print_locals out_channel;
    self#pretty_print_formals out_channel;
    (**Rajouter le calcul des valeurs de sortie*)
    self#pretty_print_out_v out_channel;
    
    self#pretty_print_initial_states  out_channel;
    self#pretty_print_final_states out_channel;
    self#pretty_print_error_states out_channel;
    (** local variable trans_name is not used for real. is only here to not generate a warning, avoiding the building process.
    (** Hack déguelasse à dégager ASAP, (fun s-> if s=s then () else ()) trans_name ) *)
    *)


    Hashtbl.iter (fun trans_name trans -> pretty_print_transition out_channel trans; Format.fprintf out_channel "\n"; (fun s-> if s=s then () else ()) trans_name )  transitions; (* One transition per line *)

    Format.fprintf out_channel "}\n"  (* atomaton{...} set bracket closed*)
      


  method print_table_stmt_loc (out_channel : Format.formatter) =
    (*Format.fprintf out_channel "Table correspondances entre états et position dans le fichier source. \n";*)
    let cmpt = ref 0 in
      Hashtbl.iter (
	fun str (loca,locb) -> 
	  if !cmpt == 0 then
	    begin
	      Format.fprintf out_channel "Automaton : %s, source_file : %s :\n" name loca.pos_fname;
	      cmpt:=!cmpt+1
	    end; 
	  Format.fprintf out_channel "State : %s location : from (line:%d,col:%d) to (line:%d,col:%d) \n" str loca.pos_lnum (loca.pos_cnum-loca.pos_bol)  locb.pos_lnum (locb.pos_cnum - locb.pos_bol)
    ) table_stmt_loc; Format.fprintf out_channel "\n"


end ;;

(***************************************************************)

 (* This class inherists class  c2cautomata_abstraction and overload method add_trans_from_stmt. *)

(***************************************************************)


class panalyse (funinfos : Cil_types.fundec) = object(self)
  inherit c2cautomata_abstraction (funinfos : Cil_types.fundec)


  val mutable log_messages  = ([] : string list )

    

  method register_failure (stmp: Cil_types.stmt ) (info : string ) =
    let (l1,_) = Ast_info.loc_stmt stmp in
    let line = l1.pos_lnum in
    let col = (l1.pos_cnum - l1.pos_bol ) in
    let message = Printf.sprintf "In file %s, Line %d, Col %d : %s" l1.pos_fname line col info in
    log_messages <- message::log_messages
    
    
    

  method add_trans_from_stmt (stmtp: Cil_types.stmt ) (iterande : Cil_types.stmt) =
    let t_id = ref ( Printf.sprintf "s%d_s%d" stmtp.sid iterande.sid )in
    let torig = ref (Printf.sprintf "s%d" stmtp.sid) in
    let tdest = ref (Printf.sprintf "s%d" iterande.sid) in
    let label_list = ref [] in
    let node_info_orig = ref SINone in
    let node_info_dest = ref SINone in

    
    begin
      match stmtp.skind with 
	 
  

       (*****************************************************************)
 
       
       (*       Assignation of a new value to a variable.               *)


       (*****************************************************************)
	  Instr( Set ((Var(vinfo),_),expr , _ ) ) ->
	    begin
	      try
		let rhs = cil_expr_2_scalar expr in
		match vinfo.vtype with (* check the type of the variable,
				     deals with the operation accordingly*)
		    TInt (_,_) ->
		      let label =  
			Affect_int ( LiIntVar(vinfo.vname) , rhs) 
		      in  label_list := (label :: !label_list)
			  
		  | TPtr(TInt(_,_),_) -> 
		   
		    let label =  
		      Affect_int_ptr ( LiIntPtr(vinfo.vname),  rhs) in
		    label_list := (label :: !label_list) 

		  | _ -> self#register_failure stmtp "Affectation to a non integer variable or pointer. Operation ignored"  
	      with
		  | Bad_expression_type ( msg ) -> self#register_failure stmtp ("failed to parse scalar expression "^msg);

		  (*Need to register a non expected behaviour*)
	    end



	  
	(*****************************************************************)
	
	(**Function call with returned value of type int, and affectation *)
        

	(*****************************************************************)
	     
	|  Instr(Call( Some(lvo) , exp1, lparam , _ ))->
	  begin
	      match lvo , exp1.enode with
		  ((Var(v),_) , Lval((Var(f),_)) ) ->
		    begin
		      match v.vtype with
			  (*Returned value has an integer type*)
			  TInt(_,_)->
			    begin
			      try
				let label =  FunCall(f.vname, (cil_expr_list_2_scalar_list  lparam)@( (LiVar(Primed,LiIntVar(v.vname)))::[]) ) in
				label_list := label :: !label_list
			      with 
				| Bad_expression_type ( msg ) -> self#register_failure stmtp ("Failed to parse: "^msg); 
			    end
			 (*The returned value is a variable that has another
			 type than an integer type. Tpointer, float for instance*)

			| _ ->  self#register_failure stmtp ( "returned value is not of an integer type." )  
		    end

		(*Other operations, notified here.*)

		| _ ->  self#register_failure stmtp ( "The returned value is not affected to a variable type. Not consided in this version")    

	  end 
	
	    (* Evaluation of a block of statments*)
(*	|  Block ( b ) ->
	  begin
	    
	  end
*)

	(**Function call with no affectation                           *)

	|  Instr(Call( None , exp1, lparam , _ ))->
	  

	  begin
	    match  exp1.enode with
		Lval((Var(f),_))  ->
		  begin
		    match f.vname with  
			"__assert_fail" ->
			  begin
			  (** 
			      In this case, the control reached the macro 
			      assertion failure, meaning the program reached 
			      an error state
			  *)
			    tdest := "assertion_failure";
			    self#add_error_state !tdest; 
			    t_id :=  Printf.sprintf "s%d_%s" stmtp.sid !tdest;
			    node_info_orig:=SInfo(SIAssertion("assertion_failure"));
			    (*if (List.length lparam ) > 0 then
			      begin
				let hd_param = List.hd lparam in
				try
				  let label = cil_expr_2_bool hd_param in
				    debug_bool := label;
				(* If the condition is satisfied, the control
				   goes straight to the statment successor 
				   One must add a transition between the current
				   control state and the newly added error state,
				   labelled with the negation of the assertion.*)
				
				  let l_label = negate_bool_sym label in
				  let id_t= Printf.sprintf "%s_%s" torig tdest_err in
				  let new_transit = 
				    {id=id_t ;orig=Control(torig,SINone);dest=Control(tdest_err,SINone);listlabel= ( Guard(l_label)::[])} in
				  self#add_transition_u new_transit ; 
				  label_list := Guard (label) :: !label_list;
				with    (* Cacthing exception *)
				  | Bad_expression_type ( msg ) -> self#register_failure stmtp ("Failed to parse: "^msg^"size of argument list :"^(Format.sprintf "%d" (List.length lparam))^"\n Expression :"^(c_bool_to_string !debug_bool));
			      end (*endif*)
			      *)
			  end (* "dummy_assert block"*)
			   

		      | _ -> 
			    begin	    
			      try
				let label =  
				  FunCall(f.vname, (cil_expr_list_2_scalar_list  lparam)) in
			      label_list := label :: !label_list
				with
				   | Bad_expression_type ( msg ) -> self#register_failure stmtp (" There exists a non scalar expression in the arg_list of "^f.vname^" :"^msg); 
			    end
			
	      
		  end
		    | _ -> self#register_failure stmtp ("This returnless function hasn't be parsed ");


	  (*Register a non parse expression here*)
	  end
	  

	(*****************************************************************)	    
        (*   If expr then block1 else block2                      *)

	(*****************************************************************)	    

	| If( expr , block_if , block_else , _ ) ->
	  begin
	    try
	      let label =  cil_expr_2_bool expr in
	      if ((List.length  block_if.bstmts) != 0) then
	      
		begin
		  let sid_bif = (List.hd  block_if.bstmts).sid in
		  if ( iterande.sid == sid_bif  )
		  then 
		    label_list :=  Guard ( label ) :: !label_list
		  else 
		    label_list := ( Guard ( negate_bool_sym  label ):: !label_list)
		end

	      else if ((List.length  block_else.bstmts) != 0) then
		begin
		  let sid_belse = (List.hd  block_else.bstmts).sid in
		  if ( iterande.sid == sid_belse  )
		  then 
		    label_list := ( Guard(negate_bool_sym  label ):: !label_list)
		  else label_list := (Guard(label)::!label_list)   
		end
	      else label_list := (Guard(label)::!label_list)
	    with 
	      | Bad_expression_type ( msg ) -> self#register_failure stmtp ("Failed to parse scalar expression in block if[Scalar] "^msg);
	  end
	    

	    (***************************************************************)

	    (* Default case, must deal with that sooner than latter        *)
	    
	    (**************************************************************)
	| _ -> self#register_failure stmtp ("Statment kind neither parsed nor abstracted ");

    end;
    let ntrans = {id = !t_id ; orig = Control (!torig,!node_info_orig) ; dest = Control (!tdest,!node_info_dest) ; listlabel = !label_list} in
    self#add_transition_u  ntrans; node_info_dest :=SINone; 
      if not ( Hashtbl.mem table_stmt_loc !torig) 
      then Hashtbl.add table_stmt_loc !torig (Ast_info.loc_stmt stmtp);

 (*Last command of the sequence adds stmt number -> file position  in the stmt_loc  table.*)

    

  method print_log_messages (out_channel : Format.formatter) = 
    List.iter (fun s -> Format.fprintf out_channel "[log ] %s \n" s) log_messages 

end;;
