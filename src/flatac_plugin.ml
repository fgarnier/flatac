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


let pretty_print_cautomata_obj out = 
  let prj= Project.current() in
  let visited_file = new  flatac_fun_visitor ( prj ) in
  let file_ast = Ast.get() in
  let ca_out_name = Printf.sprintf "%s.ca" file_ast.fileName in
  Visitor.visitFramacFile (visited_file :> frama_c_copy ) file_ast;
  visited_file#save_in_file ca_out_name;

let print () = Self.result "%t" ( fun out ->  pretty_print_cautomata_obj out )

(** The code below is not mandatory: you can ignore it in a first 
    reading. It provides an API for the plug-in, so that the function 
    [run] is callable by
    another plug-in and journalized: first, each plug-in can call 
    [Dynamic.get
    "Hello.run" (Type.func Type.unit Type.unit)] in order to call 
    [run] and second, each call to [run] is written in the Frama-C
    journal. *)
  
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





