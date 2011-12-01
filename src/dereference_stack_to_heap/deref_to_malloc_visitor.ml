open Cil_types
open Cil
open Visitor


let get_max_expr_eid (f : Cil_types.file) =
  let visitor = object
    inherit Visitor.frama_c_inplace 
       
    val mutable  max_eid = 0  
    method vexp (v : Cil_types.exp) =
      if v.eid > max_eid then
	max_eid <- v.eid;
      DoChildren
 
    method get_max_eid =
      max_eid
  end
  in
  Visitor.visitFramacFile visitor;
  visitor#get_max_eid
 



let from_stack_to_heap ( s : Cil_types.stmt ) =
  


class dereferenced_var_of_gfun_visitor ( prj : Project.t) =
object (self)
  inherit frama_c_inplace as supe
    

end;;

class deref_to_malloc_visitor (prj : Project.t )  = object (self)
  inherit frama_c_copy (prj)

  val mutable max_eid = 0

  method private set_init_max_eid =
    let current_ast = Ast.get () in
    max_eid <- (get_max_expr_eid current_ast)

  initializer self#set_init_max_eid 

  method vblock (b : Cil_types.block ) =
    

    ChangeDoChildrenPost()
    


end;;
