
(**
This file contains the types definitions for the arithmetical
expressions used in counter automata transitions and guards.


Questions, remarks, suggestions : y.x@imag.fr, substitute x with garnier and
y with florent.
*)


exception Not_LiPVar
exception Not_LiVar
exception Invalid_nts_expression

(* That type describes **)

type nts_var = NtsIVar of string

type cnt_binop = CntEq
		 | CntNeq
		 | CntLeq
		 | CntLt
		 | CntGt
		 | CntGeq
		 
type cnt_arithm_exp = CntCst of int
		      | CntSymCst of string
		      | CntVar of nts_var
		      | CntMinus of cnt_arithm_exp * cnt_arithm_exp
		      | CntSum of cnt_arithm_exp * cnt_arithm_exp
		      | CntProd of cnt_arithm_exp * cnt_arithm_exp
		      | CntMod of cnt_arithm_exp * cnt_arithm_exp
		      | CntUnMin of cnt_arithm_exp (* I want to remove that*)
		      | CntDiv of cnt_arithm_exp * cnt_arithm_exp
		      | CntInvalidExp

type cnt_bool = CntBool of cnt_binop *  cnt_arithm_exp * cnt_arithm_exp
		| CntNot of cnt_bool
		| CntBTrue
		| CntBFalse
		| CntBAnd of  cnt_bool * cnt_bool
		| CntBOr of cnt_bool * cnt_bool

type cnt_trans_label = CntGuard of cnt_bool
		   | CntFunCall of string * cnt_arithm_exp list
		   | CntAffect of nts_var * cnt_arithm_exp

