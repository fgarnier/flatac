open Ssl_types
open Ssl
open SSL_lex
open Printf
open Nts_types


exception No_allocated_vars 

class global_mem_manager = object (self)
  val mutable gmalloc_id = 1
  val mutable fresh_lvar_id = 1  
    
  val mutable list_of_glob_vars = []
  val init_seg_size = ( Hashtbl.create 97  : ( locvar , nts_genrel_arithm_exp option ) Hashtbl.t)
    
  method lvar_from_malloc () =
    let lval_name = sprintf "mid_%d" gmalloc_id in
    list_of_glob_vars <- (Format.sprintf "mid_%d_size" gmalloc_id )::list_of_glob_vars;
    list_of_glob_vars <- (Format.sprintf "mid_%d_base" gmalloc_id )::list_of_glob_vars;
    gmalloc_id <- (gmalloc_id + 1 );
    LVar(lval_name)

  method lvar_from_array_decl ( array_size : nts_genrel_arithm_exp option ) = 
    let lval_name = sprintf "mid_%d" gmalloc_id in
    list_of_glob_vars <- (Format.sprintf "mid_%d_size" gmalloc_id )::list_of_glob_vars;
    list_of_glob_vars <- (Format.sprintf "mid_%d_base" gmalloc_id )::list_of_glob_vars;
    gmalloc_id<-(gmalloc_id + 1);
    let lval_ret = LVar(lval_name) in
    Hashtbl.add init_seg_size (LVar((lval_name^"_size"))) array_size;
    lval_ret
    

  method lvar_from_constant_char (string_size : nts_genrel_arithm_exp option ) =
    let lval_name = sprintf "mid_%d" gmalloc_id in
    list_of_glob_vars <- (Format.sprintf "mid_%d_size" gmalloc_id )::list_of_glob_vars;
    list_of_glob_vars <- (Format.sprintf "mid_%d_base" gmalloc_id )::list_of_glob_vars;
    gmalloc_id<-(gmalloc_id + 1);
    let lval_ret = LVar(lval_name) in
    Hashtbl.add init_seg_size (LVar((lval_name^"_size"))) string_size;
    lval_ret
    
    
  method get_nts_transition_for_init_tab_size () =
    let init_list_folder lvar opt_size list_affect =
      match lvar,opt_size with
	  (LVar(vname),Some(size)) -> 
	    (CntGenGuard(CntGenRel(CntEq,CntGenVar(NtsGenVar(NtsVar(vname,NtsIntType),NtsPrimed)),size)))::list_affect
	      
	| (_,None) -> list_affect
    in
      Hashtbl.fold init_list_folder init_seg_size [CntGenHavoc([])]



  method get_last_mid () =
    if gmalloc_id > 1 then
      (gmalloc_id - 1)
    else 
      raise No_allocated_vars

  method get_last_lvar () =
     if gmalloc_id == 1 then
       raise No_allocated_vars
    else 
       let last_val_id = gmalloc_id - 1 in
       let lval_name = sprintf "mid_%d" last_val_id in
       LVar(lval_name)

  method pprint_vars () =
    let rec pprint_globmvar str l =
      match str, l with 
	  (_,[]) -> str
	| ("",(h::l')) -> pprint_globmvar h l'
	| (_,(h::l')) -> pprint_globmvar (str^","^h) l' 
    in
    (pprint_globmvar "" list_of_glob_vars)
	 
  method get_fresh_lvar  =
    let new_lvar_name = Format.sprintf "l_%d" fresh_lvar_id in
    let new_lvar = LVar(new_lvar_name) in
    fresh_lvar_id <- (fresh_lvar_id + 1);
    new_lvar
	
			   
  

end;;
