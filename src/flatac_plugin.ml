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

(*module Self =
  Plugin.Register
    (struct
      let name = "FlataC"
      let shortname = "FlataC"
      let help= "Extracts flat counter automata based model of C program for memory faults detection."
     end) *)
    
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
  let file_ast = Ast.get() in
  let ca_out_name = Printf.sprintf "%s.ca" file_ast.fileName in
  Visitor.visitFramacFile (visited_file :> frama_c_copy ) file_ast;
  visited_file#save_in_file ca_out_name;
  let compile_out = visited_file#pprint_all_ecfgs in
  Format.fprintf out "%s" compile_out

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




