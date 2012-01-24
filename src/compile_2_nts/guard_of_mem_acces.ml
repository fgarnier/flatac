open Ssl_types
open Ssl_types.SSL_lex
open Ssl
open Cil_types
open Cil
open Ast_goodies
open Composite_types
open Intermediate_language
open Nts_types
open Nts
open Compile_2_nts
open Cnt_interpret
open Global_mem
open Ssl_valid_abs_dom_types

exception Unhandled_offset_type of string
exception Dont_know_how_to_generate_guard
(*********************************************************************)
(* We define in this file the set of functions that allow to compute
the guards for memory access --both read and write access-- *)

(*********************************************************************)


 (** The parameter exp_type corresponds to the type of the subexpresion.*)


let make_offset_locpvar (v : ptvar ) =
  match  v  with 
      PVar ( s ) -> let soff =
	"offset__"^s^"_" in
	NtsIVar(soff)

let make_validity_varpvar ( v : ptvar) =
  match  v  with 
      PVar ( s ) -> let vdty =
	"validity__"^s^"_" in
	NtsIVar(vdty)



let get_lsizename_of_locvar ( l : locvar ) =
  match l with
      LVar( vname ) -> vname^"_size"

let get_lbasename_of_locvar ( l : locvar ) =
  match l with 
      LVar( vname ) ->  vname^"_base"



(** If used to descibe a malloc creation of a mem segment,
this function must be called AFTER the generation of the
ssl formula, to get the good gmid identificator.*)
let make_size_locvar ( l : locvar ) (mid : global_mem_manager ) ( block_size : cnt_arithm_exp) =
(*  match l with
      LVar( vname ) ->*) 
  let id_seg = mid#get_last_mid () in
  let lbase_name = get_lbasename_of_locvar l in
  let lsize_name = get_lsizename_of_locvar l  in
  let cnt_lbase = NtsIVar(lbase_name) in
  let cnt_lsize = NtsIVar(lsize_name) in
  let affect_list = (CntAffect(cnt_lbase,CntCst(id_seg))::[] ) in
  let affect_list = (CntAffect(cnt_lsize,block_size))::affect_list in
  affect_list









(* Not yet recursive. One only need to access to the first layer of
pointer for multidimentional pointers -- like int **. *)

let  offset_of_mem_access_to_cnt sslv (t : Cil_types.typ ) ( off : Cil_types.offset) =
  match off with
      NoOffset -> CntCst(0)
    | Index (exp , _ ) -> 
      let offset_exp = compile_cil_exp_2_cnt sslv exp in
      let sizeof_type = interpret_ciltypes_size t in
      (*CntProd(offset_exp,sizeof_type)*) 
      sizeof_type

    | Field(_,_) -> raise (Unhandled_offset_type ("Field met in offset_of_mem_access"))



let cnt_guard_of_array_access sslv (access_offset : Cil_types.offset) 
    ( array_type : Cil_types.typ ) =
  
  let rec array_within_bounds_cst sslv (pre : cnt_bool) 
      (offset_reader : Cil_types.offset) (current_dim_type : Cil_types.typ) =
    match offset_reader, current_dim_type with 
	(NoOffset,_) ->
	  pre
      | (Index(exp,off),TArray(telem,Some(size_curr_row),_,_)) ->
	let accs_index = compile_cil_exp_2_cnt sslv exp in
	let size_curr_dim = compile_cil_exp_2_cnt sslv size_curr_row in
	let current_cst = 
	  CntBAnd(CntBool(CntLt,accs_index,size_curr_dim),CntBool(CntGeq,accs_index,CntCst(0))) in
	array_within_bounds_cst sslv (CntBAnd(pre,current_cst)) off telem
      | (_,_) -> CntBFalse
  in
  array_within_bounds_cst sslv CntBTrue access_offset array_type

let rec cnt_guard_of_mem_access sslv ( expr : Cil_types.exp ) =
  cnt_guard_of_mem_access_enode sslv expr.enode 

and cnt_guard_of_mem_access_enode sslv ( expr_node : Cil_types.exp_node ) =
 (*(exp_type : Cil_types.typ)*) 
    
  
 (* let mem_accs_type = Cil.typeOf expr in*)  
  match  expr_node with
      Lval( Var (p) , off )->
	begin
	  match off with
	      NoOffset ->
		CntBTrue (* à voir ce qui se passe avec les tableaux *)
	    | Index(_,_) ->
	      cnt_guard_of_array_access sslv off p.vtype
	     (* let compiled_array = Compile_2_nts.compile_cil_array_2_cnt sslv
		v.vname v.vtype in
	      let index_of_accessed_tab = *)  
	      
	      
		  
 	end

    | CastE (TPtr (_,_), e ) ->
      cnt_guard_of_mem_access sslv e
    
    | CastE(t , e ) ->
      let is_t_an_int = Composite_types.is_integer_type t in
      begin
	match  is_t_an_int with
	    Some(_) ->  cnt_guard_of_mem_access sslv e
	  | None -> raise Dont_know_how_to_generate_guard
      end
   
    | Const ( _ ) -> 
      CntBTrue
	
    (*| BinOp (PlusPI,a,b,_) ->*)
      
    | BinOp (_,a,b,_)
      ->
      let fg= cnt_guard_of_mem_access sslv a in
      let fd = cnt_guard_of_mem_access sslv b in
      CntBAnd(fg,fd)
	
    | UnOp(_,e,_) ->
      cnt_guard_of_mem_access sslv e
  
    | Info (_,_) ->raise (Debug_info("[get_pvar_from_exp :] Don't know what
to do with Info"))

    | AddrOf (e) -> 
      cnt_guard_of_mem_access_enode sslv (Lval(e))
    (*let dummy_exp = {eid = -1; enode = Lval(e) ; eloc = expr.eloc ;}
      in*)


    | StartOf(_) -> CntBTrue
	  
    | Lval( Mem(e) , off ) -> (*Access at the offset off of base memory e*)
      begin
	let pvar_access = Ast_goodies.get_pvar_from_exp e in
	try
	  let location_var = Ssl.get_ptr_affectation sslv.ssl_part pvar_access in
	  if (Ssl.is_allocated location_var sslv.ssl_part )
	  then 
	    begin
	      (* Computation of the offset of the pointer plus the
	      offset at which the memory is accessed. *)
	   
	      let offset_nts_var = make_offset_locpvar pvar_access in
	      let mem_accs_type = Cil.typeOfLval ( Mem(e), off) in 
	      let top_most_offset = offset_of_mem_access_to_cnt sslv mem_accs_type 
		off in
	      let offset_of_e = Compile_2_nts.compile_cil_exp_2_cnt sslv e 
	      in
	      let sizeof_exp = Cnt_interpret.interpret_ciltypes_size
		mem_accs_type in
	      let total_offset = CntSum(CntProd(CntVar(offset_nts_var),sizeof_exp),top_most_offset)
		(*CntSum(offset_of_e,CntP*) 
	      in
	      let locvar_size_name = get_lsizename_of_locvar location_var 
	      in
	      (* Getting the location variable associated to the pointer 
	      if any.*)
	      let locvar_size = CntVar(NtsIVar(locvar_size_name)) 
	      in
	      (*Generating the access within bounds conditions guards 
	      and the alignement guard.*)
	      let interval_cond = 
		CntBAnd(CntBool(CntLt,total_offset,locvar_size),CntBool(CntGeq,total_offset,CntCst(0))) 
	      in
	      let align_cond = CntBool(CntEq,CntMod(total_offset,sizeof_exp),CntCst(0)) 
	      in
	      CntBAnd(interval_cond,align_cond)
	    end
	  else
	    CntBFalse
	
	with
	    _ -> 
	      begin
		Format.printf "The ptrvar %s has not matching affectation in the heap \n" ;
		  CntBFalse
	      end
	
      end

    | _ -> 
      Format.fprintf Ast_goodies.debug_out "\n I failed to interpret the expression : [ ";
      Cil.d_exp Ast_goodies.debug_out ( Cil.dummy_exp expr_node);
       Format.fprintf Ast_goodies.debug_out " ] \n "; 
      
      raise 
	Dont_know_how_to_generate_guard






let mem_guards_of_funcall_arg_list sslv (l : Cil_types.exp list) =
  let guard_folder (ret_guard : cnt_bool) (arg : Cil_types.exp  ) =
    let arg_guard = cnt_guard_of_mem_access sslv arg in
    match arg_guard with
	CntBTrue -> ret_guard (* The guard need not to be chaged, And(prev,true) => prev*)
      | _ -> CntBAnd(ret_guard,arg_guard)
  in
  List.fold_left guard_folder CntBTrue l
	
