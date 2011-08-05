(**********************************************************************)
(* 
This file contains the functions that translates the arithmetics
expression from CIL into counter automata based one. The focus is maid
on pointer arithmetic, especialy for int * pointer arithmetics

Questions and/or remarks : mail to florent dot garnier At imag dot fr
*) 

open Cil_types
open Intermediate_language
open Ssl_types
open Ssl
open SSL_lex
open Global_mem
open Validity

exception Not_LiPVar
exception Not_LiVar

type cnt_binop = CntEq
		 | CntNeq
		 | CntLeq
		 | CntLt
		 | CntGt
		 | CntGeq

type cnt_arithm_exp = CntCst of int
		      | CntVar of string
		      | CntMinus of cnt_arithm_exp * cnt_arithm_exp
		      | CntSum of cnt_arithm_exp * cnt_arithm_exp
		      | CntProd of cnt_arithm_exp * cnt_arithm_exp
		      | CntMod of cnt_arithm_exp * cnt_arithm_exp
		      | CntUnmin of cnt_aritme_exp (* I want to remove that*)
		      | CntInvalidExp

(** This function aims at computing the name of the couter var name
associated to the offset of a pointer variable*)
let offset_cnt_name ( ptvar : c_ptrexp ) =  
  match ptvar with
      LiPVar(_,LiIntPtr(vname)) -> CntVar( (sprintf "offset(%s)" vname) )
    | _ -> raise Not_LiPVar
  
let int_var_cnt_name ( cexpr : c_scal) =
   match ptvar with
      LiVar(_,LiIntVar(vname)) -> CntVar( (sprintf "intvar(%s)" vname) )
    | _ -> raise Not_LiVar

let rec interpret_c_scal_to_cnt  ( sslf : ssl_formula )( scalexp : c_scal ) =
  match scalexp with 
      LiVar(_) -> int_var_cnt_name scalexp
    | LiConst(i) ->  CntCst(i)
    | LiProd ( l , r ) ->
	begin
	  let lg = interpret_c_scal_to_cnt l in
	  let ld = interpret_c_scal_to_cnt r in
	    CntProd ( lg , ld )
	end
    |  LiSum  LiProd ( l , r ) ->
	 begin
	   let lg = interpret_c_scal_to_cnt l in
	   let ld = interpret_c_scal_to_cnt r in
	     CntProd ( lg , ld )
	 end
    | LiMinus ( l , r ) ->
	begin
	  let lg = interpret_c_scal_to_cnt l in
	  let ld = interpret_c_scal_to_cnt r in
	    CntMinus ( lg , ld )
	end
	  
    | LiMod ( l , r ) ->
	begin
	  let lg = interpret_c_scal_to_cnt l in
	  let ld = interpret_c_scal_to_cnt r in
	    CntMod ( lg , ld )
	end
	  
    | LiUnMin( t ) -> 
	let tin = interpret_c_scal_to_cnt t in
	  CunUnMin( tin)
	    
    | LiMinusPP ( l , r ) ->
	let basel = Validity.base_ptrexp sslf l in
	let baser = Validity.base_ptrexp sslf r in
	  if basel = baser then
	    begin
	      let lg = interpret_c_ptrexp_to_cnt sslf l in
	      let ld = interpret_c_ptrexp_to_cnt sslf r in
		CntMinus ( lg , ld )
	    end
	  else CntInvalidExp

and interpret_c_ptrexp_to_cnt (sslf : ssl_formula )( ptrexp : c_ptrexp ) =
  
