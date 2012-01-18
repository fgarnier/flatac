(***********************************************************************)
(* 

This files contains the functions that allow to translate the validity
conditions concerning pointer arithmetic expressions into NTS systems
expressions.


Questions and/or remarks : mail to florent dot garnier At imag dot fr
**********************************************************************)

open Cil_types
open Intermediate_language
open Ssl_types
open Ssl
open SSL_lex
open Validity_types

exception Cannot_find_pvar
exception Cannot_find_lvar




(** returns the location variable l which models the base address
of the pointer expression.
*)
let and_valid fg fd =
  match fg , fd with
      ( FalseValid , _ ) -> FalseValid
    | ( _ , FalseValid) -> FalseValid
    | ( TrueValid, TrueValid ) -> TrueValid
    | ( a , b) -> AndValid(a,b)

(** base_ptrexp(PTRexp) returns the locvar l such that PTRexp -> l*)

let base_ctab tab =
  match tab with
      LiTab ( Some(tab_name),_,_) -> PVar(tab_name)
    | LiTab(None,_,_) -> raise UnnammedLocalArray


let rec base_var_ptrexp ( ptr_exp : c_ptrexp ) =
   match ptr_exp with 
      LiPVar ( _ , LiIntPtr(vname), _ ) ->
	PVar(vname) (* That's indeed the name of the pointer
		    var we are looking for.*)
    
     | LiBaseAddrOfArray(_,cptr) -> base_ctab  cptr

     | LiPlusPI ( cptr , _ , _) -> base_var_ptrexp  cptr
     | LiIndexPI ( cptr , _ , _) -> base_var_ptrexp  cptr
     | LiMinusPI ( cptr , _ , _) -> base_var_ptrexp  cptr
     | LiAddrOfScal(expr , _) ->  base_var_cscal expr(*Nil*) (*
						  Ça n'est
						  pas aussi simple que ça.
						  le cscal peut égalmenent
'						  être un cast d'un pointer,
						  qui lui même a son addresse 
						 de base propre/une variable 
						 de poiteur de base qui lui 
						 est propre.
						*)
and base_var_cscal ( c_exp : c_scal ) =
  match c_exp with
      LiScalOfAddr( add , _ ) -> base_var_ptrexp add
    | _ -> raise Cannot_find_pvar

(* Nil. Maybe raising an customized exception
		    will help to handle this case without confusion*)



let rec base_ptrexp (sslf : ssl_formula )( ptr_exp : c_ptrexp ) =
  match ptr_exp with 
      LiPVar ( _ , LiIntPtr(vname), _ ) ->
	get_ptr_affectation sslf (PVar(vname))
	  
    | LiPlusPI ( cptr , _ , _) -> base_ptrexp sslf cptr
    | LiIndexPI ( cptr , _ , _) -> base_ptrexp sslf cptr
    | LiMinusPI ( cptr , _ , _) -> base_ptrexp sslf cptr
    | LiAddrOfScal( scalar , _ ) -> base_cscalptrexp sslf scalar
    | LiBaseAddrOfArray (_,LiTab(Some(tab_name),_,tt)) -> 
      (*let cptr = base_var_ptrexp ptr_exp in*) 
      base_ptrexp sslf (LiPVar(Unprimed,LiIntPtr(tab_name),tt))
   | LiBaseAddrOfArray (_,LiTab(None,_,tt)) ->  
     LVar("")
and
    base_cscalptrexp (sslf : ssl_formula )( scal : c_scal ) =
  match scal with
      LiScalOfAddr(ptrexp , _ ) -> base_ptrexp sslf ptrexp
    | _ -> LVar("") (*Nil*)



(** Generates counter based expressions that allow to determinate
whether an arithmetical expression is valid or not.

!!! THIS FUNCTION OUGHT BE USED TO GENERATE THE GUARDS FOR FLATA,
WHEN THE VALIDITY CAN'T BE STATICALLY DEDCIED FROM THE ABSTRACT
DOMAIN ( SSL*NTS_COUNTERS ).
THIS function performs a TRANSLATION.

IT shall not be confuse with Var_validity.validity_of and consort.

*)

(*let validity_name_of_counter (counter_name : string) =
  "__validity_"^counter_name^"_" *)


let rec valid_cscal (sslf : ssl_formula ) ( scal : c_scal) =
  match scal with
      LiVar(_ , LiIntVar(vname)) -> 
	let valname = Nts.valid_name_of_var vname in 
	IntValid(valname)

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
	 
    | LiMinusPP ( ptrexpg , ptrexpd, _ ) ->
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

    | LiScalOfAddr( ptrexp , _) -> 
      valid_ptrexp sslf ptrexp
	  
and valid_ptrexp (sslf : ssl_formula ) ( ptrexp :  c_ptrexp ) =
  match ptrexp with 
      LiPVar ( _ , LiIntPtr(vname), _ ) ->  
	let valname =  Nts.valid_name_of_var vname in 
	(PtValid(valname))
 
    | LiPlusPI ( ptrexpprime , cscal , _) -> 
	begin
	  let fg = valid_ptrexp sslf ptrexpprime in
	  let fd = valid_cscal sslf cscal in
	    and_valid fg fd 
	end
	  
    | LiIndexPI ( ptrexpprime , cscal , _) -> 
	begin
	  let fg = valid_ptrexp sslf ptrexpprime in
	  let fd = valid_cscal sslf cscal in
	    and_valid fg fd 
	end

    |  LiMinusPI ( ptrexpprime , cscal, _) -> 
	begin
	  let fg = valid_ptrexp sslf ptrexpprime in
	  let fd = valid_cscal sslf cscal in
	    and_valid fg fd 
	end

    | LiAddrOfScal ( scal_exp , _ ) ->
      valid_cscal sslf scal_exp


    | LiBaseAddrOfArray(_,LiTab(Some(tname),dim_list,t)) ->
      begin
	let val_ptr_exp = 
	  valid_ptrexp sslf (LiPVar(Unprimed,LiIntPtr(tname),t)) in
	let valid_criterion pre size =
	  match size with
	    | None -> and_valid pre TrueValid
	    | Some (scal_size) ->
	      and_valid pre (valid_cscal sslf scal_size) 
	in
	List.fold_left valid_criterion TrueValid dim_list
      end

