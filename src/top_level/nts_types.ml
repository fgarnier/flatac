
(**
This file contains the types definitions for the arithmetical
expressions used in counter automata transitions and guards.


Questions, remarks, suggestions : y.x@imag.fr, substitute x with garnier and
y with florent.
*)


exception Not_LiPVar
exception Not_LiVar
exception Not_Guard
exception Invalid_nts_expression

(* That type describes **)

type nts_base_types = NtsIntType
		      | NtsRealType
		      | NtsBool
		      


(*type nts_array =  NtsArray of string * int * nts_base_types
		  |  NtsMDimArray of string * int * nts_array  
*)
(* An array has a name, a size and contains the type of
each element 
This type definition allows to encode multi dimentional arrays.
*)   			      
 

			      

type nts_var = NtsIVar of string (*Integer type variable*)
	       | NtsRVar of string (*Real valued variable*)
	       (*| NtsBVar of String (*Boolean variable*)*)
	       | NtsMiscType of string (* Just here for the demo*)
	       

	       

type cnt_binop = CntEq
		 | CntNeq
		 | CntLeq
		 | CntLt
		 | CntGt
		 | CntGeq
		 
type cnt_arithm_exp = CntCst of int
		      | CntNdet
		      | CntNdetVar of string (* non deterministic value *)
		      | CntSymCst of string
		      | CntVar of nts_var 
		      | CntMinus of cnt_arithm_exp * cnt_arithm_exp
		      | CntSum of cnt_arithm_exp * cnt_arithm_exp
		      | CntProd of cnt_arithm_exp * cnt_arithm_exp
		      | CntMod of cnt_arithm_exp * cnt_arithm_exp
		      | CntUnMin of cnt_arithm_exp (* I want to remove that*)
		      | CntDiv of cnt_arithm_exp * cnt_arithm_exp
		      | CntInvalidExp


type ref_nts_array = RefBasicTypeArray of nts_base_types
		     | RefMulDimArray of ref_nts_array

type nts_array = RefNtsArray of ref_nts_array
		 | FixedSizeNtsArray of fixed_size_nts_array

and fixed_size_nts_array = FixedSizeBasicTypeNtsArray of 
    cnt_arithm_exp * nts_base_types
			   | FixedSizeMulDimNtsArray of 
			       cnt_arithm_exp * nts_array 
				 
type nts_array_var = NtsArrayVar of string * nts_array

type cnt_bool = CntBool of cnt_binop *  cnt_arithm_exp * cnt_arithm_exp
		| CntNot of cnt_bool
		| CntBTrue
		| CntBFalse
		| CntBAnd of  cnt_bool * cnt_bool
		| CntBOr of cnt_bool * cnt_bool



(* Types used to deal with function calls and returned values that
includes information concerning the validity of pointers and integer
values*)

type il_ptr_fun_arg = 
    {
      base_of_exp :  Ssl_types.SSL_lex.locvar ;
      offset_of_exp : cnt_arithm_exp;
      validity_of_ptr_exp : cnt_arithm_exp ;
    }

type il_int_fun_arg =
    {
      expr : cnt_arithm_exp;
      validity_of_exp : cnt_arithm_exp ;
    }
    

type il_fun_arguments = IlScalArg of il_int_fun_arg
			| IlPtrArg of il_ptr_fun_arg


type cnt_trans_label = CntGuard of cnt_bool
		   | CntFunCall of string * nts_var list option * il_fun_arguments list
		   | CntAffect of nts_var * cnt_arithm_exp
		   | CntNdetAssign of nts_var 
		   | CntHavoc of nts_var list (* The value of the listed 
						 variables are not copied.
						 See NTL documentation.
					       *)
		   