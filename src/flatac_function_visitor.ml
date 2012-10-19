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
	GVar(vinfo,_,_) -> 
	  begin
	    match vinfo.vtype with
		TPtr(_,_) -> ("validity__"^vinfo.vname^"_")::(("offset__"^vinfo.vname^"_")::pre_list)
	      | _ ->
		  begin
		    match (Composite_types.is_integer_type vinfo.vtype) with
			Some(_) ->("validity__"^vinfo.vname^"_")::(vinfo.vname::pre_list)
		      | None -> vinfo.vname::pre_list
		  end
	  end
      | _ -> pre_list
   in
     List.fold_left gvar_list_folder [] file.globals  
	  


 let pprint_list_of_gvars (file : Cil_types.file) =
   let gvar_list = get_list_of_int_type_gvars file in
   let size_list = ref (List.length gvar_list) in
   let pprint_list_folder pre_str str =
     if !size_list == 1 then
       pre_str^str^" : int;"
     else if !size_list > 1 then
       begin
	 size_list:=(!size_list - 1);
	 pre_str^str^","
       end
     else
       ""
   in
     List.fold_left pprint_list_folder "" gvar_list

(*GVarDecl of funspec*) 


 let substitute_dot_slash_by_underscore str =
   let str_len = String.length str in
   for i=0 to str_len-1 
   do
     if (str.[i]='.' || str.[i]='/' || str.[i]='-')
     then String.set str i '_'
     else ()
   done;
   str
 

let get_c_file_name fname =
    let len = String.length fname in
    let f_name = String.sub fname 0 (len-2) in
    substitute_dot_slash_by_underscore f_name



 exception Empty_name

class flatac_visitor (prj : Project.t )  = object (self)
  inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
    
  val local_file_ast = Ast.get ()
  val source_file_name = 
    begin 
      match ( Kernel.Files.get() ) with
	  [] -> raise No_file_name_given
	| ""::_ -> raise Empty_name
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
  
    Format.printf "[Get nts name] Source file name is %s \n" source_file_name;
    nts_name <- Filename.basename (get_c_file_name source_file_name);
    Format.printf "[Get nts name] nts_name equals %s \n" nts_name
 

   (* It is required that Cfg.computeFileCFG has been called upon the
   Ast file before calling this method.*)
  method private register_ecfg_of_gfun ( funinfos : Cil_types.fundec ) =
    
    let gtype_info_visitor = new global_composite_types_visitor ( prj ) 
    in 
    Visitor.visitFramacFile ( gtype_info_visitor :> frama_c_copy) 
      local_file_ast;
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
      Hashtbl.fold  pprint_folder function_tables ("nts "^nts_name^";")

  (*method pprint_all_ntsint_global_var () =*)
    

  method save_in_file ( file_name : string ) =
    if not is_computed then
      raise Untraversed_ast 
    else 
      let out_file = open_out file_name in
      let format_out_file = Format.formatter_of_out_channel out_file in
      Format.fprintf Ast_goodies.debug_out "Saving nts : %s \n %!" nts_name;

	Format.fprintf format_out_file "nts %s;\n" nts_name;  
	Format.fprintf format_out_file "%s \n" (pprint_list_of_gvars local_file_ast);
	Format.fprintf format_out_file "%s" ((self#pprint_all_ecfgs ()));
	Format.fprintf format_out_file "%!";
	close_out out_file;
      
end;;
