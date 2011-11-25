open Cil_types

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


let compute () =
  Format.printf "Starting@.";
  let ast = Ast.get () in
  let vis = new vis in
  Cil.visitCilFile vis ast

let () = Db.Main.extend compute
