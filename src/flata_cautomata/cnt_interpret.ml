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
		      | CntPlus of cnt_arithm_exp * cnt_arithm_exp
		      | CntTimes of cnt_arithm_exp * cnt_arithm_exp
		      | CntModulo of cnt_arithm_exp * cnt_arithm_exp
		      | InvalidExp

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
    | 
