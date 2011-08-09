(***********************************************************************)
(* This files contains the functions that allows to determine wheter
some aritmetic expressions that might contain pointers are valid w.r.t.
the notion of validity introduced in the technical report. 

Questions and/or remarks : mail to florent dot garnier At imag dot fr
**********************************************************************)

open Cil_types
open Intermediate_language
open Ssl_types
open Ssl
open SSL_lex



(** returns the location variable l which models the base address
of the pointer expression.
*)
let and_valid fg fd =
  match fg , fd with
      ( FalseValid , _ ) -> FalseValid
    | ( _ , FalseValid) -> FalseValid
    | ( TrueValid, TrueValid ) -> TrueValid
    | ( a , b) -> AndValid(a,b)

let rec base_ptrexp (sslf : ssl_formula )( ptr_exp : c_ptrexp ) =
  match ptr_exp with 
      LiPVar ( _ , LiIntPtr(vname), _ ) ->
	get_ptr_affectation sslf (PVar(vname))

    | LiPlusPI ( cptr , _ ) -> base_ptrexp sslf cptr
    | LiIndexPI ( cptr , _ ) -> base_ptrexp sslf cptr
    | LiMinusPI ( cptr , _ ) -> base_ptrexp sslf cptr
  
(** Generates counter based expression that allows to determinate
wheter an arithmetical expression is valid or not.
*)
let rec valid_cscal (sslf : ssl_formula ) ( scal : c_scal) =
  match scal with
      LiVar(_ , LiIntVar(vname)) -> IntValid(vname)
    | LiConst(_) -> TrueValid
    | LiSymConst(_) -> TrueValid
    
    | LiProd ( cscalg, cscald ) ->
	begin
	  let fg = valid_cscal sslf cscalg in
	  let fd = valid_cscal sslf cscald in
	    and_valid fg fd
	end
    
    | LiSum (cscalg , cscald ) -> 
	begin
	  let fg = valid_cscal sslf cscalg in
	  let fd = valid_cscal sslf cscald in
	    and_valid fg fd
	end	  
    
    | LiMinus (cscalg , cscald ) -> 
	begin
	  let fg = valid_cscal sslf cscalg in
	  let fd = valid_cscal sslf cscald in
	    and_valid fg fd
	end
    
    |  LiUnMin (cscalg) -> valid_cscal sslf cscalg
    
    |  LiMod(cscalg, cscald ) ->
	 begin
	  let fg = valid_cscal sslf cscalg in
	  let fd = valid_cscal sslf cscald in
	    and_valid fg  fd 
	 end 
	 
    | LiMinusPP ( ptrexpg , ptrexpd ) ->
	begin
	  if not ( (base_ptrexp sslf ptrexpg)==(base_ptrexp sslf ptrexpd) )
	  then FalseValid
	  else 
	    begin
	      let fg = valid_ptrexp sslf ptrexpg in
	      let fd = valid_ptrexp sslf ptrexpd in
		and_valid fg fd 
	    end
	end
	  
and valid_ptrexp (sslf : ssl_formula ) ( ptrexp :  c_ptrexp ) =
  match ptrexp with 
      LiPVar ( _ , LiIntPtr(vname), _ ) ->  (PtValid(vname)) 
    | LiPlusPI ( ptrexpprime , cscal) -> 
	begin
	  let fg = valid_ptrexp sslf ptrexpprime in
	  let fd = valid_cscal sslf cscal in
	    and_valid fg fd 
	end
	  
    | LiIndexPI ( ptrexpprime , cscal) -> 
	begin
	  let fg = valid_ptrexp sslf ptrexpprime in
	  let fd = valid_cscal sslf cscal in
	    and_valid fg fd 
	end

    |  LiMinusPI ( ptrexpprime , cscal) -> 
	begin
	  let fg = valid_ptrexp sslf ptrexpprime in
	  let fd = valid_cscal sslf cscal in
	    and_valid fg fd 
	end
