
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
open Compile_2_nts
open Ssl_valid_abs_dom



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
  let slocals_register_folder absdom sloc  =
Format.fprintf Ast_goodies.debug_out "[register_slocal : slocal iterator]  Validity tab contains %s \n" (pprint_validity_loc_map absdom.validinfos );
    
    match sloc.vtype with 
	
      | TPtr( TComp(_,_,_) ,_) | TPtr( TNamed(_,_) ,_) ->
	  begin
	    let sslf_abstr = absdom_param.ssl_part in
	    let vname = sloc.vname in
	    let typedef_index =  absdom_param.composite_types_infos in
	    let sfield_type = 
	      match sloc.vtype with
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
	    let valid_info_res = absdom.validinfos in
	    let valid_info_res = 
	      Hashtbl.fold ( path_to_pointer_field_folder_of_ptr_struct vname)
		index_of_pointer_field.pointers valid_info_res in
	    let valid_info_res = 
	      Hashtbl.fold ( path_to_pointer_field_folder_of_ptr_struct vname)
		index_of_pointer_field.integer_values valid_info_res in
	   
	      new_struct_pointer_on_stack sloc sslf_abstr typedef_index mid;
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
	  let atom_aff = (Pointsto((PVar(sloc.vname)),fresh_lvar)) in
	  add_atomic_affect_to_validity_abstdomain atom_aff absdom;
	  absdom
	end 


      | TArray( atyp, opt_size, _ , _ ) ->  
	( match opt_size with
	    None ->
	      begin
		let fresh_lvar = mid#lvar_from_array_decl None in
		let atom_aff = (Pointsto((PVar(sloc.vname)),fresh_lvar)) in
		add_atomic_affect_to_validity_abstdomain atom_aff absdom
	      end
	  | Some(array_size) ->
	    begin
	      let cnt_array_size =  compile_sizeof_array_type absdom_param sloc.vtype in
	      let fresh_lvar = mid#lvar_from_array_decl (Some(cnt_array_size)) in
	      let atom_aff = (Pointsto((PVar(sloc.vname)),fresh_lvar)) in
	      add_alloc_cell_to_validity_abstdomain fresh_lvar absdom;
	      add_atomic_affect_to_validity_abstdomain atom_aff absdom
	    end
	    );
	absdom
	

      | TComp(_,_,_) | TNamed(_,_) ->
	begin
	  let sslf_abstr = absdom.ssl_part in
	  let vname = sloc.vname in
	  let typedef_index =  absdom.composite_types_infos in
	  let typedef_name = Typename_of_cil_types.typename_of_ciltype 
	    sloc.vtype
	  in
	  let index_of_pointer_field = 
	    Composite_types.get_index_of_pointer_by_type_name
	      typedef_index typedef_name
	  in
	  let valid_info_res = absdom.validinfos in
	  let valid_info_res = 
	   Hashtbl.fold ( path_to_pointer_field_folder vname)
	    index_of_pointer_field.pointers valid_info_res in
	  let valid_info_res = 
	   Hashtbl.fold ( path_to_pointer_field_folder vname)
	    index_of_pointer_field.integer_values valid_info_res in
	  new_struct_on_stack sloc sslf_abstr typedef_index mid;
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

Format.fprintf Ast_goodies.debug_out "[register_slocal] slocal list contains : %s " (List.fold_left (fun  s v -> s^"var :"^v.vname^" \n") "" funinfos.slocals);
Format.fprintf Ast_goodies.debug_out "[register_slocal] Before registerin slocals : Validity tab contains %s \n" (pprint_validity_loc_map absdom_param.validinfos );

let absdom_param = List.fold_left (slocals_register_folder) absdom_param funinfos.slocals in 
Format.fprintf Ast_goodies.debug_out "[register_slocal] After registering slocals : Validity tab contains %s \n" (pprint_validity_loc_map absdom_param.validinfos );
 List.fold_left ( fun absdom vinf_slocal -> set_var_validity_in_absdomain absdom vinf_slocal None FalsevarValid ) absdom_param (funinfos.slocals)
 


(* Registers the set of local variables in the validity table.
For each pointer variable x, one need to associate a location var

l such that x->l in the ssl part.
*)
    


(*
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
*)


(* Registers the set of local variables in the validity table*)
let register_globals mid (fileinfo : Cil_types.file ) ( absdom_param : ssl_validity_absdom ) =

  let path_to_pointer_field_folder (struct_name : string ) (path : string)  _ (loc_map : validity_loc_map) =
    let pvar_name = struct_name^"."^path in
    let res = set_validity_in_by_name loc_map pvar_name  DKvarValid 
      LocalVar in
    res
  in

  let path_to_pointer_field_folder_of_ptr_struct (struct_name : string ) (path : string)  _ (loc_map : validity_loc_map) =
    let pvar_name = struct_name^"->"^path in
    let res = set_validity_in_by_name loc_map pvar_name  DKvarValid 
      LocalVar in
      res
  in
  let sglobal_register_folder absdom gval =
    match gval with 
	GVar(sform,_,_) | GVarDecl(_,sform,_) ->
	  begin
	    Format.printf "Registering globlal variable %s' validity \n" sform.vname ;
	    match sform.vtype with 
	      | TInt(_,_) -> 
		  set_var_validity_in_absdomain absdom sform None DKvarValid 
	      
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
			index_of_pointer_field.pointers valid_info_res in
		    let valid_info_res = 
		      Hashtbl.fold ( path_to_pointer_field_folder_of_ptr_struct vname)
			index_of_pointer_field.integer_values valid_info_res in
		      new_struct_pointer_on_stack sform sslf_abstr typedef_index mid;
		      let new_absinfos =
			{
			  ssl_part = sslf_abstr ;
			  validinfos = valid_info_res ;
			  composite_types_infos = typedef_index ;
			}
		      in
		      let fresh_lvar = mid#get_fresh_lvar in
		      let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
			add_atomic_affect_to_validity_abstdomain atom_aff
			  new_absinfos;
			new_absinfos
	  (* A factoriser avec le bloc plus bas ... La, c'est pour la démo,
	mais c'est crade ... *)
		  end
     

	      | TPtr(_,_) ->
		  begin
		    let fresh_lvar = mid#get_fresh_lvar in
		    let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
		      add_atomic_affect_to_validity_abstdomain atom_aff absdom;
		      let absdom = set_var_validity_in_absdomain absdom sform None DKvarValid in
			absdom
		  end 


	      | TArray( atyp, opt_size, _ , _ ) ->
		begin
		  match opt_size with
		      None -> 
			let fresh_lvar = mid#lvar_from_array_decl None in
			let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
			add_atomic_affect_to_validity_abstdomain atom_aff absdom
		    | Some(array_size) ->
		      let cnt_array_size =  compile_sizeof_array_type 
			absdom_param sform.vtype in
		      let fresh_lvar = mid#lvar_from_array_decl (Some(cnt_array_size)) in
		      let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
		      add_alloc_cell_to_validity_abstdomain fresh_lvar absdom;
		      add_atomic_affect_to_validity_abstdomain atom_aff absdom
		
		end;
		absdom
		

		 (* begin
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
		  end*)

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
			index_of_pointer_field.pointers valid_info_res in
		    let valid_info_res = 
		      Hashtbl.fold ( path_to_pointer_field_folder vname)
			index_of_pointer_field.integer_values valid_info_res in
		      new_struct_on_stack sform sslf_abstr typedef_index mid;
		      let new_absinfos =
			{
			  ssl_part = sslf_abstr ;
			  validinfos = valid_info_res ;
			  composite_types_infos = typedef_index ;
			}
		      in
  
		      let fresh_lvar = mid#get_fresh_lvar in
		      let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
			add_atomic_affect_to_validity_abstdomain atom_aff
			  new_absinfos;
			let new_absinfos =  set_var_validity_in_absdomain absdom sform None DKvarValid  in
			new_absinfos
			  
		  end
	      | _ -> absdom
	  end
      | _ -> absdom
  in
    Cil.foldGlobals fileinfo sglobal_register_folder absdom_param
  
(*let absdom_param = List.fold_left sformals_register_folder absdom_param funinfos.sformals in
    List.fold_right ( fun vinf_slocal absdom -> set_var_validity_in_absdomain absdom vinf_slocal None DKvarValid ) (funinfos.sformals) absdom_param
*)


let register_sformals mid (funinfos : Cil_types.fundec ) ( absdom_param : ssl_validity_absdom ) =

  let path_to_pointer_field_folder (struct_name : string ) (path : string)  _ (loc_map : validity_loc_map) =
    let pvar_name = struct_name^"."^path in
    let res = set_validity_in_by_name loc_map pvar_name  DKvarValid 
      LocalVar in
    res
  in

  let path_to_pointer_field_folder_of_ptr_struct (struct_name : string ) (path : string)  _ (loc_map : validity_loc_map) =
    let pvar_name = struct_name^"->"^path in
    let res = set_validity_in_by_name loc_map pvar_name  DKvarValid 
      LocalVar in
      res
  in
  let sformals_register_folder absdom sform =
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
		index_of_pointer_field.pointers valid_info_res in
	    let valid_info_res = 
	      Hashtbl.fold ( path_to_pointer_field_folder_of_ptr_struct vname)
		index_of_pointer_field.integer_values valid_info_res in
	      new_struct_pointer_on_stack sform sslf_abstr typedef_index mid;
	      let new_absinfos =
		{
		  ssl_part = sslf_abstr ;
		  validinfos = valid_info_res ;
		  composite_types_infos = typedef_index ;
		}
	      in
	      let fresh_lvar = mid#get_fresh_lvar in
	      let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
	      add_atomic_affect_to_validity_abstdomain atom_aff
		new_absinfos;
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
	    index_of_pointer_field.pointers valid_info_res in
	  let valid_info_res = 
	   Hashtbl.fold ( path_to_pointer_field_folder vname)
	    index_of_pointer_field.integer_values valid_info_res in
	  new_struct_on_stack sform sslf_abstr typedef_index mid;
	  let new_absinfos =
	    {
	      ssl_part = sslf_abstr ;
	      validinfos = valid_info_res ;
	      composite_types_infos = typedef_index ;
	    }
	  in
  
	let fresh_lvar = mid#get_fresh_lvar in
	let atom_aff = (Pointsto((PVar(sform.vname)),fresh_lvar)) in
	add_atomic_affect_to_validity_abstdomain atom_aff
	  new_absinfos;
	new_absinfos
	
end
      | _ -> absdom
  in
let absdom_param = List.fold_left sformals_register_folder absdom_param funinfos.sformals in
  List.fold_right ( fun vinf_slocal absdom -> set_var_validity_in_absdomain absdom vinf_slocal None DKvarValid ) (funinfos.sformals) absdom_param


