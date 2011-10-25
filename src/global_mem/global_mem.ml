open Ssl_types
open Ssl
open SSL_lex
open Printf


exception No_allocated_vars 

class global_mem_manager = object (self)
  val mutable gmalloc_id = 1
  val mutable fresh_lvar_id = 1  
    
  method lvar_from_malloc () =
    let lval_name = sprintf "mid_%d" gmalloc_id in
    gmalloc_id <- (gmalloc_id + 1 );
    LVar(lval_name)

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


 method get_fresh_lvar =
   let new_lvar_name = Format.sprintf "l_%d" fresh_lvar_id in
   let new_lvar = LVar(new_lvar_name) in
   fresh_lvar_id <- (fresh_lvar_id + 1);
   new_lvar
	
			   
  

end;;
