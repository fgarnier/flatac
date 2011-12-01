open Cil_types
open Visitor
open Simple_debug_visitor

class vis = object
  inherit Cil.nopCilVisitor

  method vstmt s =
    (match s.skind with
      | Instr (Skip loc) -> Format.printf "Found skip at %t/%a; succ: %a@."
          Cil.d_thisloc Cil.d_loc loc Cil.d_stmt (List.hd s.succs);
      | _ -> (*Format.printf "Found non-skip statement, loc %t@."
          Cil.d_thisloc*) ()
    );
    Cil.DoChildren

  method vfunc fdec =
    Format.printf "Visiting %s@." fdec.svar.vname;
    Cil.DoChildren
end


module Plop =
   Plugin.Register
      ( struct
             let name = "Boris' test"
             let shortname = " boris "
             let help = "Used to check if wrong locations informations are still present inside the Cil ast"
         end )

module Enabled =
   Plop.False
      ( struct
             let option_name = "-boris"
             let help = "Tracking those wrong info concerning locations "
             let kind = `Correctness
	end )

let compute () =
  Format.printf "Starting@.";
  let ast = Ast.get () in
  let vis = new vis in
  Cil.visitCilFile vis ast;
  
   
  let prj= Project.current() in
  let ast = Ast.get () in
  let debug_instance = new  flatac_debug_visitor ( prj ) in
  let ca_debug_out_name = Printf.sprintf "%s_debug_info.ca" ast.fileName in
  let debug_out_file = open_out ca_debug_out_name in
 
  let format_debug_out_file = 
    Format.formatter_of_out_channel debug_out_file in
  Visitor.visitFramacFile (debug_instance :> frama_c_inplace ) ast;
  debug_instance#pretty_print_f2ca format_debug_out_file;
  Format.fprintf format_debug_out_file "%!";
  close_out debug_out_file

let () = Db.Main.extend compute
