open Cil_types
open Cil
open Visitor 
open List
open Format
open Hashtable




class call_graph_visitor (prj : Project.t)= object
  inherit  Visitor.generic_frama_c_visitor prj (Cil.inplace_visit())




end


class call_graph_visitor (prj : Project.t)= object
  inherit  Visitor.generic_frama_c_visitor prj (Cil.inplace_visit())

(*  val fun_names : string Hashtable
  
  method vglob_aux =
  method *) 

end
