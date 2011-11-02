(** This module describes a visitor that inherit from the generic_framac_visitor class. Its purpose consists in traversing the CIL AST and collect all the name of each global functions. 

The set of names can be recovered using the method get_fun_list
*)

open Cil_types
open Cil
open Visitor 
open List
open Format
open Lexing
open C2cautomatanalys
open Cfg

 exception No_computation_performed


class flatac_debug_visitor (prj : Project.t) =  object
  (*inherit frama_c_visitor*)
  inherit Visitor.generic_frama_c_visitor prj (Cil.inplace_visit())


  val mutable is_computed = false
  val mutable fun_cautomata =[]

(** This method triggers the visit of the current AST. Each time a node describes a Global function *)



  method vglob_aux  p = 
      is_computed <-true;
 
 match p with 
      GFun ( funinfos , _ ) ->   
	Cfg.prepareCFG funinfos; Cfg.computeCFGInfo funinfos true; 
	let cauto_fun = new panalyse funinfos in
	fun_cautomata <- ( cauto_fun :: fun_cautomata);
	DoChildren
    | _ -> DoChildren
  
 

  method pretty_print_tables ( out_channel : Format.formatter ) =
    List.iter (fun ca ->ca#print_table_stmt_loc out_channel ) fun_cautomata

  method pretty_print_f2ca ( out_channel : Format.formatter ) =
    List.iter (fun ca ->ca#pretty_print out_channel ) fun_cautomata

  method print_log_mess ( out_channel : Format.formatter ) =
    List.iter (fun ca ->ca#print_log_messages out_channel ) fun_cautomata

end 

