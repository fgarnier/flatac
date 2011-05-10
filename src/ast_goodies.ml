open Cil_types
(*open Option *)

  module Ast_goodies = struct
    
    let loc_of_instr (inst:Cil_types.instr) =
      match inst with 
	  Set ( _ , _ , loc ) -> loc
	| Call ( _ , _ , _ , loc ) -> loc 
	| Asm ( _ , _ , _ , _ , _ , loc ) -> loc
	| Skip ( loc ) -> loc
	| Code_annot ( _ , loc ) -> loc
       
      
(* This function is only here to provide some function that takes as
input a statement and that returns the location. This function 
was provided in the Ast.info module, but vanished during the migration
from Boron to Carbon ... *)
    let loc_stmt_opt ( stmt : Cil_types.stmt ) =  

      match stmt.skind with 
	  Instr( inst ) -> Some ( loc_of_instr inst )
	| Return( _ , loc ) -> Some ( loc )
	| Break ( loc ) -> Some ( loc )
	| If ( _ , _ , _ , loc ) -> Some (loc)
	| Switch ( _ , _, _, loc ) -> Some ( loc )
	| Loop ( _ , _ ,loc, _ , _ ) -> Some ( loc )
	| TryFinally (_ , _ , loc ) -> Some ( loc )
	| TryExcept ( _ , _ , _, loc ) -> Some ( loc )
	| _ -> None (* Default case *)
(*	| Block ( _ ) -> None
	| UnspecifiedSequence ( _ )
*)


  end ;;
