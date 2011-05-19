(** This file contains the definitions of the syntax and the types
used to represent and manipulate the SSL logic formuala.

For questions, comments, write to florent.garnier__AT__imag-dOt-fr.
For any bugs reports, additional work, concerns or ill-exlpained stuff, 
please blame : gaudin.maxime\\at//gmail-DoT-fr. :).
*)


open List
open Hashtbl

module SSL_types_gen = functor ( P : sig 
				   val order_relation : string -> string -> bool
				   val equals_to : string -> string -> bool
				 end ) ->
struct
  let order_relation = P.order_relation
  let equals_to = P.equals_to
  type ptvar = PVar of string
  type locvar = LVar of string
  type eq = Eqloc of locvar * locvar
  type affect = Pointsto of ptvar * locvar
  type affectnil = Pointsnil of ptvar
  
  type pure_formula = { 
    equations:  eq  list; 
                       (** The set of equations between location variables *)
    affectations : (ptvar , (locvar , unit) t ) t; 
                       (** The set of affectations of pointer variables. A
		       single variables can be affected different values in
			a non normalized formula*)
    ptnil : (ptvar , unit) t;
                      (** The set of pointer variables that points to nil
			 a.k.a. NULL *)
  }
  
 
  type space_formula = Space of (locvar , int ) t (** Contains the set of
						  the possibly empty allocated
						  cells, and the number
						  of corresponding occurences *)
                       | Top_heap                 (** Corrupted heap *)


(** A ssl formula consists in a set --possibly empty-- of existentially
quantified variables, a pure and a spatial part.*)
  
  type ssl_formula = {
    quant_vars:(locvar , unit ) t;
    pure : pure_formula;
    space : space_formula; 
  }
  

      
end;;


module SSL_lex = SSL_types_gen (struct 
			      let order_relation = (>)
			      let equals_to = (=)
				end) 

