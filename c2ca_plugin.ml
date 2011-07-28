open Cautomata
open Cil_types
open Cil
open Visitor 
open List
open Format


(*module Print_fun = struct*) 

 exception No_computation_performed


(** The print_fun_visitor let the AST untouched. The method vglob_aux traverses the AST tree and store the function name in list fun_name.*)
class print_fun_visitor (prj : Project.t) =  object
  (*inherit frama_c_visitor*)
  inherit Visitor.generic_frama_c_visitor prj (Cil.inplace_visit())

  (* mutable fun_names_hash = Hashtbl.create 97 *) (* 97 is prime and has good propreties 

 						for the Hash algorithm, Nicolas said ... *) 
  val mutable is_computed = false
  val mutable fun_name_list = []

(** This method triggers the visit of the current AST. Each time a node describes a Global functio
constructor, the method adds its name (funinfo.svar.vname) into the list ``fun_name_list'' *)
  method vglob_aux  p = 
      is_computed <-true;
  match p with 
      GFun ( funinfos , _ ) ->   (fun_name_list <- funinfos.svar.vname::fun_name_list); 
	DoChildren
    | _ -> DoChildren
  
  (** Returns the list of the names of all global methods visited in the AST. If
the visitor has not be used to perform a visit yet, calling this method result
in raising a "No_computatio_performed" exception *)
  method get_fun_list =
    if is_computed then fun_name_list else raise No_computation_performed 
      
  method pretty_print_list_name out_chanel =
    if  is_computed then
      List.iter ( fun  s -> Format.fprintf out_chanel "Function déclaration: %s \n" s )  fun_name_list
    else raise  No_computation_performed 
 end

(*end *) 
(*Marks the end of module Print_fun *) 

