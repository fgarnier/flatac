open Self
open Cil
open Cil_types
open Cfg
open Visitor
open Sem_and_logic_front_end
open Flatac_ssl_front_end
open Extended_cfg_types
open Extended_cfg
open Composite_type_visitors
open Composite_type_types
open Composite_types

exception Untraversed_ast
exception No_file_name_given

module Flatac_extended_cfg =  
  Extended_cfg_definition (
    struct
      type abstract_type = Ssl_valid_abs_dom_types.ssl_validity_absdom
      type label_type = Nts_types.cnt_trans_label list
    end
  )

open Flatac_extended_cfg


let get_list_of_int_type_gvars (file : Cil_types.file) =
  let gvar_list_folder pre_list global_elem =
    match global_elem with
	GVar()
	  GVarDecl of funspec 


class flatac_visitor (prj : Project.t )  = object (self)
  inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
    
  val local_file_ast = Ast.get ()
  val source_file_name = 
    begin 
      match ( Kernel.Files.get() ) with
	  [] -> raise No_file_name_given
	| f::_ -> f
    end

  val mutable is_computed = false
  val mutable nts_name = ""

  val function_tables = Hashtbl.create 97
  
  val mutable index_of_pointers_of_composite_types =
    Composite_types.create_index_of_composite_types ()
  val mutable index_of_composite_types_set = false


   initializer self#get_nts_name()

  method private get_nts_name () =
    let index = ref 0 in
    while 
      ((!index < (String.length source_file_name)) && 
	 (source_file_name.[!index] != '.')) 
    do
      nts_name<-nts_name^(String.make 1 (source_file_name.[!index]));
      index:=!index+1
    done 

  method private register_ecfg_of_gfun ( funinfos : Cil_types.fundec ) =
    let gtype_info_visitor = new global_composite_types_visitor ( prj ) 
    in 
    Visitor.visitFramacFile ( gtype_info_visitor :> frama_c_copy) local_file_ast;
    let index = gtype_info_visitor#get_index_of_composite () in
    let debug_msg = pprint_index_of_type_pointer_path index in
    Format.printf "flatac_visitor: Index of global types : %s \n" debug_msg;
    let funname = funinfos.svar.vname in
    let flatac_ssl_frontend = new ssl_flatac_front_end in
    flatac_ssl_frontend#set_index_of_composite_types index;
    let ecfg_of_visited_gfun = 
      new extended_cfg  funname local_file_ast funinfos 
	(flatac_ssl_frontend :> ((Flatac_extended_cfg.Extended_cfg_base_types.abs_dom_val, Flatac_extended_cfg.Extended_cfg_base_types.trans_label_val) Sem_and_logic_front_end.sem_and_logic_front_end) ) 
    in
    Hashtbl.add function_tables funname ecfg_of_visited_gfun
	

  method vglob_aux (g : Cil_types.global ) =
    is_computed <- true ;
    match g with 
        GFun ( funninfos , _ ) ->
          let strinfo = Ast_goodies.pprint_slocal_vars funninfos.slocals in
	    Format.printf "%s \n" strinfo;
       	  self#register_ecfg_of_gfun funninfos;
	  DoChildren
      | _ -> DoChildren 

  (* This function returns the persistant structure that contains
     the set of all extended control flow graphs, that each desribes 
     one C global function. *)
	
  method get_ecfgs_of_file () =
    function_tables

  method set_index_of_composite_type ( i : index_of_composite_types ) =
    index_of_pointers_of_composite_types <- i

  method pprint_all_ecfgs () =
    let pprint_folder _ registered_ecfg pre_msg =
    let current_ecfg_output = registered_ecfg#pprint_to_nts () in
      pre_msg^current_ecfg_output^"\n"
    in
      Hashtbl.fold pprint_folder function_tables ""


  method pprint_all_ecfgs_states () =
    let pprint_folder _ registered_ecfg pre_msg =
      let current_ecfg_output = registered_ecfg#pprint_ecfg_vertex () in
	pre_msg^current_ecfg_output^"\n"
    in
      Hashtbl.fold  pprint_folder function_tables ("nts "^source_file_name^";")

  method pprint_all_global_var () =
    

  method save_in_file ( file_name : string ) =
    if not is_computed then
      raise Untraversed_ast 
    else 
      let out_file = open_out file_name in
      let format_out_file = Format.formatter_of_out_channel out_file in
      Format.fprintf format_out_file "nts %s;\n" nts_name;  
      Format.fprintf format_out_file "%s" ((self#pprint_all_ecfgs ()));
      Format.fprintf format_out_file "%!";
      close_out out_file;
      
end;;
