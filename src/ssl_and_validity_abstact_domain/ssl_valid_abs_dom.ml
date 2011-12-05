open Ssl_types
open Ssl
open SSL_lex
open Ssl_types
open Ssl_valid_abs_dom_types
open Var_validity_types
open Var_validity
open Cil_types
open Composite_type_types
open Composite_types
open New_composite_type_upon_ssl
 
exception Debug_info of string

let create_validity_abstdomain () = 
  let sslf = create_ssl_f () in
  let val_map = new_valid_map () in
  let index_types = Composite_types.create_index_of_composite_types in
  {
    ssl_part = sslf ;
    validinfos = val_map ;
    composite_types_infos =  index_types ;
  }

let add_alloc_cell_to_validity_abstdomain  (lvar : locvar) (domain :ssl_validity_absdom) =
  Ssl.add_alloc_cell lvar domain.ssl_part 

let add_atomic_affect_to_validity_abstdomain  (equ : SSL_lex.affect) (domain :ssl_validity_absdom) =
  Ssl.and_atomic_affect equ domain.ssl_part 

(** the fiel ssl_part is mutable and peristant, whereas validinfo isn't.*)
let copy_validity_absdomain (v : ssl_validity_absdom ) =
  let sslf = Ssl.copy v.ssl_part in
  let itable = 
    Composite_types.copy_index_of_composite_types v.composite_types_infos
  in
  let ret_val = {
    ssl_part =  sslf ;
    validinfos = v.validinfos ;
    composite_types_infos = itable ;
  }
  in 
  ret_val


(* This function returns a new domain value with the validity od vinfo updated
to valid. *)
let set_var_validity_in_absdomain  (domain : ssl_validity_absdom) ( vinfo : Cil_types.varinfo )(off : Cil_types.offset option) (valid : var_valid) =
  {
    ssl_part = domain.ssl_part ;
    validinfos = (set_validity_in domain.validinfos vinfo off valid) ;
    composite_types_infos = domain.composite_types_infos ;
  }

  


(* Registers the set of local variables in the validity table*)
let register_slocals mid (funinfos : Cil_types.fundec ) ( absdom_param : ssl_validity_absdom ) =

  let path_to_pointer_field_folder (struct_name : string ) (path : string)  _ (loc_map : validity_loc_map) =
    let pvar_name = struct_name^"."^path in
    let res = set_validity_in_by_name loc_map pvar_name  FalsevarValid 
      LocalVar in
    res
  in

  let path_to_pointer_field_folder_of_ptr_struct (struct_name : string ) (path : string)  _ (loc_map : validity_loc_map) =
    let pvar_name = struct_name^"->"^path in
    let res = set_validity_in_by_name loc_map pvar_name  FalsevarValid 
      LocalVar in
      res
  in
  let slocals_register_folder absdom sform =
    match sform.vtype with 
	
      | TPtr( TComp(_,_,_) ,_) | TPtr( TNamed(_,_) ,_) ->
	  begin
	    let sslf_abstr = absdom_param.ssl_part in
	    let vname = sform.vname in
	    let typedef_index =  absdom_param.composite_types_infos in
	    let sfield_type = 
	      match sform.vtype with
		  TPtr(ttype,_) ->ttype
		| _ -> raise (Debug_info("This must be a pointer \n")) 
	    in
	    let typedef_name = Typename_of_cil_types.typename_of_ciltype 
	      sfield_type
	    in
	    let index_of_pointer_field = 
	      Composite_types.get_index_of_pointer_by_type_name
		typedef_index typedef_name
	    in
	    let valid_info_res = absdom_param.validinfos in
	    let valid_info_res = 
	      Hashtbl.fold ( path_to_pointer_field_folder_of_ptr_struct vname)
		index_of_pointer_field valid_info_res in
	      new_struct_pointer_on_stack sform sslf_abstr typedef_index mid;
	      let new_absinfos =
		{
		  ssl_part = sslf_abstr ;
		  validinfos = valid_info_res ;
		  composite_types_infos = typedef_index ;
		}
	      in
		new_absinfos
	(* A factoriser avec le bloc plus bas ... La, c'est pour la démo,
	mais c'est crade ... *)
	  end
     

      | TPtr(_,_) ->
	begin
	  let fresh_lvar = mid#get_fresh_lvar in
	  let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
	  add_atomic_affect_to_validity_abstdomain atom_aff absdom;
	  absdom
	end 


      | TArray( atyp, opt_size, _ , _ ) ->
	begin
	  let fresh_lvar = mid#get_fresh_lvar in
	  let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
	  add_atomic_affect_to_validity_abstdomain atom_aff absdom;
	  begin
	    match opt_size with
		None -> ()
	      | Some(array_size) ->
		add_alloc_cell_to_validity_abstdomain fresh_lvar absdom
	  end;
	  absdom
	end

      | TComp(_,_,_) | TNamed(_,_) ->
	begin
	  let sslf_abstr = absdom_param.ssl_part in
	  let vname = sform.vname in
	  let typedef_index =  absdom_param.composite_types_infos in
	  let typedef_name = Typename_of_cil_types.typename_of_ciltype 
	    sform.vtype
	  in
	  let index_of_pointer_field = 
	    Composite_types.get_index_of_pointer_by_type_name
	      typedef_index typedef_name
	  in
	  let valid_info_res = absdom_param.validinfos in
	  let valid_info_res = 
	   Hashtbl.fold ( path_to_pointer_field_folder vname)
	    index_of_pointer_field valid_info_res in
	  new_struct_on_stack sform sslf_abstr typedef_index mid;
	  let new_absinfos =
	    {
	      ssl_part = sslf_abstr ;
	      validinfos = valid_info_res ;
	      composite_types_infos = typedef_index ;
	    }
	  in
	  new_absinfos
	
end
      | _ -> absdom
  in
let absdom_param = List.fold_left slocals_register_folder absdom_param funinfos.slocals in
  List.fold_right ( fun vinf_slocal absdom -> set_var_validity_in_absdomain absdom vinf_slocal None FalsevarValid ) (funinfos.slocals) absdom_param



(* Registers the set of local variables in the validity table.
For each pointer variable x, one need to associate a location var

l such that x->l in the ssl part.
*)
    
let register_sformals mid (funinfos : Cil_types.fundec ) 
    ( absdom_param : ssl_validity_absdom ) =
  let formal_register_folder absdom sform =
    match sform.vtype with 
      | TPtr(_,_) ->
	begin
	  let fresh_lvar = mid#get_fresh_lvar in
	  let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
	  add_atomic_affect_to_validity_abstdomain atom_aff absdom;
	  absdom
	end 
      | _ -> absdom
  in
let absdom_param = List.fold_left formal_register_folder absdom_param funinfos.sformals in
  List.fold_right ( fun vinf_slocal absdom -> set_var_validity_in_absdomain absdom vinf_slocal None  DKvarValid ) (funinfos.sformals) absdom_param
