open Self
open Cil
open Cil_types
open Cfg
open Visitor
open Sem_and_logic_front_end
open Flatac_ssl_front_end
open Extended_cfg_types
open Extended_cfg


exception Untraversed_ast

module Flatac_extended_cfg =  
  Extended_cfg_definition (
    struct
      type abstract_type = Ssl_valid_abs_dom_types.ssl_validity_absdom
      type label_type = Nts_types.cnt_trans_label list
    end
  )

open Flatac_extended_cfg




class flatac_visitor (prj : Project.t ) = object (self)
  inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
    
  val mutable is_computed = false
  val function_tables = Hashtbl.create 97
  

  method private register_ecfg_of_gfun ( funinfos : Cil_types.fundec ) =
    let funname = funinfos.svar.vname in
    let flatac_ssl_frontend = new ssl_flatac_front_end in
    let ecfg_of_visited_gfun = new extended_cfg funname funinfos flatac_ssl_frontend  in
      Hashtbl.add function_tables funname ecfg_of_visited_gfun
	

  method vglob_aux (g : Cil_types.global ) =
    is_computed <- true ;
    match g with 
        GFun ( funninfos , _ ) ->          
       	  self#register_ecfg_of_gfun funninfos;
	  DoChildren
      | _ -> DoChildren 

  (* This function returns the persistant structure that contains
     the set of all extended control flow graphs, that each desribes 
     one C global function. *)
	
  method get_ecfgs_of_file =
    function_tables

  method pprint_all_ecfgs =
    let pprint_folder _ registered_ecfg pre_msg =
    let current_ecfg_output = registered_ecfg#pprint_to_nts in
      pre_msg^current_ecfg_output^"\n"
    in
      Hashtbl.fold pprint_folder function_tables ""
      

  method save_in_file ( file_name : string ) =
    if not is_computed then
      raise Untraversed_ast 
    else 
      let out_file = open_out file_name in
      let format_out_file = Format.formatter_of_out_channel out_file in
      Format.fprintf format_out_file "%s" (self#pprint_all_ecfgs);
      Format.fprintf format_out_file "%!";
      close_out out_file;
      
end;;
