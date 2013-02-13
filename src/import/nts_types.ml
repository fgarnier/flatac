
(**
This file contains the types definitions for the arithmetical
expressions used in counter automata transitions and guards.


Questions, remarks, suggestions : y.x__at__imag.fr, substitute x with garnier and
y with florent.



Written by Florent Garnier, at Verimag Labs  2012 
Contact florent dot garnier at gmail dot com for  further informations.

This files is released under the terms of the LGPL v2.1 Licence.

 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor,
 Boston, MA  02110-1301  USA

*)



exception Not_LiPVar
exception Not_LiVar
exception Not_Guard
exception Invalid_nts_expression
exception Not_an_if_then_else_condition_guard 

(* That type describes **)

type nts_quantifier = NtsExists
		      | NtsForall

type nts_base_types = NtsIntType
		      | NtsRealType
		      | NtsBoolType
		      | NtsUnTyped
		     



type nts_var = NtsVar of string * nts_base_types

type nts_primed_type = NtsPrimed
		       | NtsUnPrimed


(* Variables in the general relations : Primed or unprimed nts vars*)
type nts_genrel_var = NtsGenVar of nts_var * nts_primed_type 



type cnt_binop = 
    CntEq
  | CntNeq
  | CntLeq
  | CntLt
  | CntGt
  | CntGeq

type nts_gen_arithm_binop = 
    CntGenSum
  | CntGenMinus
  | CntGenProd
  | CntGenDiv
  | CntGenMod


type nts_gen_bool_cst = CntBTrue | CntBFalse


type nts_gen_arithm_unop = CntGenUMinus

type nts_gen_bool_binop = CntGenBAnd 
			  | CntGenBOr
 			 

type nts_base_type_cst = 
    CntGenICst of  Big_int.big_int
  | CntGenFCst of float
  | CntGenBCst of nts_gen_bool_cst

type nts_symbolic_constant = CntSymCst of string * nts_base_types

type nts_genrel_arithm_exp =
    CntGenCst of nts_base_type_cst * nts_base_types
	
 (* | CntGenNdetVar of string (* non deterministic value *) *)
  | CntGenSymCst of nts_symbolic_constant * nts_base_types
  | CntGenVar of nts_genrel_var
      
  | CntGenArithmBOp of nts_gen_arithm_binop *
      nts_genrel_arithm_exp * nts_genrel_arithm_exp * nts_base_types
      
  | CntGenArithmUOp of nts_gen_arithm_unop *
      nts_genrel_arithm_exp * nts_base_types
		    



type ref_nts_array = 
    RefBasicTypeArray of nts_base_types
  | RefMulDimArray of ref_nts_array
			 
type nts_array = 
    RefNtsArray of ref_nts_array
  | FixedSizeNtsArray of fixed_size_nts_array
		     
and fixed_size_nts_array = 
    FixedSizeBasicTypeNtsArray of 
	nts_genrel_arithm_exp * nts_base_types
  | FixedSizeMulDimNtsArray of 
      nts_genrel_arithm_exp * nts_array 
				 
type nts_array_var = 
    NtsArrayVar of string * nts_array * nts_base_types



type nts_gen_relation = 
    CntGenRel of cnt_binop * nts_genrel_arithm_exp  * nts_genrel_arithm_exp
  | CntGenRelComp of nts_gen_bool_binop * nts_gen_relation * nts_gen_relation
  | CntGenNot of nts_gen_relation
  | CntGenTrue
  | CntGenFalse
  | CntQVarsGenRel of nts_genrel_var list * nts_quantifier * nts_gen_relation   
      

      
      
(* Generic type definition for NTS lib transitions  *)
type nts_trans_label = CntGenGuard of nts_gen_relation
		       
		       | CntGenCall of string * nts_genrel_var list option * nts_genrel_arithm_exp list
			   
			   
		       | CntGenHavoc of nts_genrel_var list 
(* The value of the listed 
   variables are not copied.
   See NTL documentation.
*)
		             
