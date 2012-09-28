 (* 
Boolean doesn't have a peculiar type in ANSI C. 
Any non-zero scalar expression (char, Int, float, double, signed or unsigned)
is evaluated to true. Zero values are interpreted as false.

(** Scalars "type" contains as well pointers values. NULL is evaluated
to false and any other non-zero address is evaluated as true. *)
*)
type primed = Primed
	      | Unprimed
type c_int_var = LiIntVar of string
type c_int_cst = LiIConst of  My_bigint.t


type c_float_var = LiFloatVar of string
type c_float_cst = LiFloatConst of float


type c_int_sym_const = LiSymIConst of string
		       | LiTypeSizeof of Cil_types.typ
		       | LiCAliasTypeSizeof of Composite_type_types.c_type_name

type c_ptr = LiIntPtr of string (*The represented type is indeed an int*)
	     



(* We define the type of  the scalars as follows.
basicaly, the scalar we consider in this language are all the scalars
that are the evaluation of arithmetic
expressions which ground terms are free integer variables
and integers constants.

*)




(*type c_tab*)
	     (*| LiMultDimTab of int list * Cil_types.typ*)


type  c_scal_bit_op = LiBitAnd 
		      | LiBitOr
		      | LiBitXor

(** The type of integers scalar expressions*)
type c_scal = LiVar of primed * c_int_var
	      | LiFVar of primed * c_float_var
	      | LiConst of c_int_cst
	      | LiFConst of c_float_cst
	      | LiSymConst of c_int_sym_const  (*Like sizeof of types or 
					       defined constant *)
	      | LiProd of c_scal * c_scal
	      | LiSum of c_scal * c_scal
	      | LiMinus of c_scal * c_scal
	      | LiUnMin of c_scal
	      | LiMod of c_scal * c_scal   (*Modulo operator*)
	      | LiDiv of c_scal * c_scal (* Integer division*) 
	      | LiMinusPP of c_ptrexp * c_ptrexp *  Cil_types.typ
	      | LiScalOfAddr of c_ptrexp * Cil_types.typ 
                                        (* Type must be TPtr(t,_) 
					Where t is not TPtr(_,_)
					   Correponds to *p.
					*)
	      | LiElemOfCTab of c_scal list * c_tab
	      | LiScalOfLiBool of c_bool

	      | LiBitLogicOp of c_scal_bit_op * c_scal * c_scal
	      | LiIntStarOfPtr  of c_ptrexp * Cil_types.typ
	     (* Access to the value of a pointer of type TPtr(TInt(_,_),_)*)
	      | LiIntStarOfPtrField of c_ptr * string * Cil_types.typ 
		  (*Access to an int field of a struct through a pointer
		  on an instance of the structure.*)

	     
and c_ptrexp = LiPVar of primed * c_ptr *  Cil_types.typ
	       | LiBaseAddrOfArray (* Base address of an array*)
		   of (c_scal ) list * c_tab (* the index list specifies
				        which subtab we are refering to.
				       *)
	       | LiPlusPI of c_ptrexp * c_scal  * Cil_types.typ
	       | LiIndexPI of c_ptrexp * c_scal * Cil_types.typ
	       | LiMinusPI of c_ptrexp * c_scal * Cil_types.typ
	       | LiAddrOfScal of c_scal * Cil_types.typ
	       | LiDerefCVar of string  * Cil_types.typ
		   (*&v where v is of type typ*)
	       | LiStarOfPtr of c_ptrexp * Cil_types.typ (*
							 Type must be
							   TPtr(TPtr(_,_))
							 *p.
							 *)
	       | LiStarOfPtrStructField of c_ptr * string * Cil_types.typ
	       | LiDerefCPtr of c_ptrexp  * Cil_types.typ
		   (*&v where v is a pointer expression *)
	       | LiDerefCTab of c_tab
 
and li_array_size = LiArraySize of c_scal
		     | LiArraySizeUnknown

and c_tab = LiTab of string option * (c_scal option) list * Cil_types.typ

and c_bool = LiBNot of c_bool 
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

type il_expr = IlScal of c_scal
	       | IlPtr of c_ptrexp
