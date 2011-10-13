open Self
open Cil
open Cil_types
open Cfg
open Visitor
open Sem_and_logic_front_end
open Flatac_ssl_front_end
open Extended_cfg_types


module Flatac_extended_cfg =  
  Extended_cfg_definition (
    struct
      type abstract_type = Ssl_valid_abs_dom_types.ssl_validity_absdom
      type label_type = Nts_types.cnt_trans_label list
    end
  )

open Flatac_extended_cfg




class flatac_visitor (prj : Project.t ) = object (self)
  inherit Visitor.generic_frama_c_visitor (prj) (Cil.inplace_visit())
    
  val mutable is_computed = false
  val function_tables = Hashtbl.create 97
  


  
  method private register_ecfg_of_gfun ( funinfos : Cil_types.fundec ) =
    let funname = fununfos.svar.vname in
    let flatac_ssl_frontend = new ssl_flatac_front_end in
    let ecfg_of_visited_gfun = new extended_cfg funname flatac_ssl_frontend in
      Hashtbl.add function_tables funname ecfg_of_visited_gfun
	

  method vglob_aux (g : Cil_types.global ) =
    is_computed <- true ;
    match g with 
        GFun ( funninfos , _ ) ->          
       	  self#register_ecfg_of_gfun funninfos;
	  DoChildren
      | _ -> DoChildren 

  (* This function returns the persistant *)
  method get_ecfgs_of_file =
    function_tables

end;;
