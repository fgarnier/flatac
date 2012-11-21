(*
Ought we need to add some reference to FRAMAC and LGPL2 copyright
notice here, since this code was once extended form an example
takem from FRAMA-C ugin developper manual ??

I think so.

*)




open Visitor
open Flatac_function_visitor
open Cil_types
open Cil
open Self
(*open Flatac_debug_visitor*)
open Composite_type_visitors
open Intermediate_language

(*module Self =
  Plugin.Register
    (struct
      let name = "FlataC"
      let shortname = "FlataC"
      let help= "Extracts flat counter automata based model of C program for memory faults detection."
     end) *)


exception No_file_name_given
    
module Enabled =
        Self.False
                (struct
                        let option_name= "-flatac"
                        let help= ""
                        let kind= `Correctness
                 end)



let pretty_print_cautomata_obj out = 
  let prj= Project.current() in
  let visited_file = new  flatac_visitor ( prj ) in
  let composite_types = new global_composite_types_visitor ( prj ) in
  let project_name = Project.get_name prj in
  Format.printf "Current project name is : %s \n" project_name ;
 

  let get_c_file_name fname =
    let len = String.length fname in
    let f_name = String.sub fname 0 (len-2) in
    f_name
  in
  let kernel_file_name () =  
    begin match ( Kernel.Files.get() ) with
	[] -> raise No_file_name_given
      | f::_ -> f
    end
  in
  Format.printf "File name provided by the Kernel.Files.get function is %s \n" (kernel_file_name ());
  Format.printf "Name without its extension: %s \n" (get_c_file_name (kernel_file_name()))
 ;
  
 
  let ca_out_name = 
    Format.sprintf "%s.nts" (kernel_file_name ()) in
  let out_file = open_out ca_out_name in
  let types_out_name = Format.sprintf "%sa_types" (kernel_file_name()) in
  let trace_info_out_name = Format.sprintf "%s_tracinfo" (kernel_file_name()) in
    
  let file_ast = Ast.get() in
  Cfg.clearFileCFG file_ast;
  Cfg.computeFileCFG file_ast;
  
  Visitor.visitFramacFile (composite_types :> frama_c_copy) file_ast;
  let index = composite_types#get_index_of_composite () in 
  Visitor.visitFramacFile (visited_file :> frama_c_copy ) file_ast;
  visited_file#save_in_file ca_out_name;

  let compile_out = visited_file#pprint_all_ecfgs () in
  Format.fprintf out "%s%!" compile_out;
  
  let states_out = visited_file#pprint_all_ecfgs_states () in
  Format.fprintf out "%s%!" states_out;
  (* This part consists in checking the cfg structure given by Cil*)
 
  let backtrackinginfos = 
    visited_file#pprint_all_ecfg_info_for_trace_analysis () in
  
  let types_out = open_out types_out_name in
  let trace_out= open_out  trace_info_out_name in
  let format_out_file = Format.formatter_of_out_channel out_file in
  let types_out_file = Format.formatter_of_out_channel types_out in
  let trace_out_file = Format.formatter_of_out_channel  trace_out in
 (* Visitor.visitFramacFile (visit_bibi :> frama_c_copy ) file_ast;
  visit_bibi#pretty_print_f2ca format_out_file;*)  
 
  Format.fprintf types_out_file "%s %!" (composite_types#pprint_pvars_of_comp_types ());
  Format.fprintf format_out_file "%!";
  Format.fprintf trace_out_file "%s %!" backtrackinginfos ;
  close_out out_file;
  close_out types_out;
  close_out trace_out

let print () = Self.result "%t" ( fun out ->  pretty_print_cautomata_obj out )

let print =
  Dynamic.register
    ~plugin:"flatac"
    "run"
    ~journalize:true
    (Datatype.func Datatype.unit Datatype.unit)
    print

(** Print 'Hello World!' whenever the option is set. *)
let run () =  if Enabled.get () then print ()  

(** Register the function [run] as a main entry point. *)
let () = Db.Main.extend run





