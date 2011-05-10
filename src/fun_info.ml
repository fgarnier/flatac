(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(*    Laboratoire VERIMAG, 2010.                                          *)
(**************************************************************************)

(** 
 This plugin is copied from the hello_world example from the Frama-C
tutorial and sightly modified so that : It prints the name of the global function of a C-program. *)



open Visitor
open Cil
open Cil_types
open Print_fun
open Printf
open Format


(** Register the new plug-in "Hello World" and provide access to some plug-in
    dedicated features. *)

module Self =
  Plugin.Register
    (struct
       let name = "Flata-C"
       let shortname = "flatac"
       let help = "Generates a counter automata based abstraction of a set of C functions"
     end)

(** Register the new Frama-C option "-hello". *)
module Enabled =
  Self.False
    (struct
       let option_name = "-flatac"
       let help = "Generates a counter automata based abstraction of a set of C functions"
      let kind = `Correctness 
	end)


let pretty_print_cautomata_obj out = 
  let prj= Project.current() in
  let visit_bibi = new  print_fun_visitor ( prj ) in
  let file_ast = Ast.get() in
  let ca_out_name = Printf.sprintf "%s.ca" file_ast.fileName in
  let table_out_name = Printf.sprintf "%s.stbl" file_ast.fileName in
  let messages_log_file = Printf.sprintf "%s.log" file_ast.fileName in
  let out_file = open_out ca_out_name in
  let out_file_tbl = open_out table_out_name in
  let out_file_log = open_out messages_log_file in
  let format_out_file = Format.formatter_of_out_channel out_file in
  let format_out_file_tbl = Format.formatter_of_out_channel out_file_tbl in
  let format_log_msg = Format.formatter_of_out_channel out_file_log in
  Visitor.visitFramacFile (visit_bibi :> frama_c_copy ) file_ast;
  visit_bibi#pretty_print_f2ca out; 
  visit_bibi#pretty_print_f2ca format_out_file;
  visit_bibi#pretty_print_tables format_out_file_tbl; 
  visit_bibi#print_log_mess format_log_msg;
  Format.fprintf format_out_file "%!";
  Format.fprintf format_out_file_tbl "%!"; (*Flushes the formatter*)
  Format.fprintf format_log_msg "%!";
  close_out out_file;
  close_out out_file_log;
  close_out out_file_tbl


let print () = Self.result "%t" ( fun out ->  pretty_print_cautomata_obj out )



(** The code below is not mandatory: you can ignore it in a first reading. It
    provides an API for the plug-in, so that the function [run] is callable by
    another plug-in and journalized: first, each plug-in can call [Dynamic.get
    "Hello.run" (Type.func Type.unit Type.unit)] in order to call [run]
    and second, each call to [run] is written in the Frama-C journal. *)
let print =
  Dynamic.register
    ~plugin:"Flatac"
    "run"
    ~journalize:true
    (Datatype.func Datatype.unit Datatype.unit)
    print

(** Print 'Hello World!' whenever the option is set. *)
let run () =  if Enabled.get () then print ()  

(** Register the function [run] as a main entry point. *)
let () = Db.Main.extend run
