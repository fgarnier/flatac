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
      CntProd(offset_exp,sizeof_type)
    | Field(_,_) -> raise (Unhandled_offset_type ("Field met in offset_of_mem_access"))



let cnt_guard_of_mem_access sslv ( expr : Cil_types.exp ) =
 (*(exp_type : Cil_types.typ)*) 
    
  
  let mem_accs_type = Cil.typeOf expr in  
  match  expr.enode with 
      Lval( Var (p) , off )->
	begin
	  match off with
	      NoOffset ->
		CntBTrue (* à voir ce qui se passe avec les tableaux *)
		  
 	end
	  
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
	      let offset_of_e = offset_of_mem_access_to_cnt sslv mem_accs_type 
		off in
	      let sizeof_exp = Cnt_interpret.interpret_ciltypes_size
		mem_accs_type in
	      let total_offset = 
		CntSum(offset_of_e,CntProd(CntVar(offset_nts_var),sizeof_exp)) 
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
	      let align_cond = CntBool(CntEq,CntMod(total_offset,locvar_size),CntCst(0)) 
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








let mem_guards_of_funcall_arg_list sslv (l : Cil_types.exp list) =
  let guard_folder (ret_guard : cnt_bool) (arg : Cil_types.exp  ) =
    let arg_guard = cnt_guard_of_mem_access sslv arg in
    match arg_guard with
	CntBTrue -> ret_guard (* The guard need not to be chaged, And(prev,true) => prev*)
      | _ -> CntBAnd(ret_guard,arg_guard)
  in
  List.fold_left guard_folder CntBTrue l
	
(*
let rec get_pvar_from_exp_node (expn : Cil_types.exp_node ) =
  match expn with
      Lval ( Var( p ) , off ) ->
        begin
          Format.fprintf  debug_out "get_pvar_from_exp_node : lval is a Var(p) \n";
          Cil.d_lval debug_out  ( Var(p), off);
          Format.fprintf  debug_out "\n";
          match p.vtype with 
              TPtr(_,_) -> 
                begin
                  match off with (* If lval is a subfield of a structure*)
                      Field (finfo, suboffset) ->
                        let pvar_name = get_subfield_name 
                          (p.vname) finfo suboffset in
                        Format.printf "Pvar name is : %s \n" pvar_name;
                          (PVar(pvar_name))

                    | NoOffset -> 
                      Format.printf "No offset for pvar \n";
                      (PVar(p.vname))
                    |  _ -> raise (Debug_info (" In get_pvar_from_exp : I don't know how to deal with array indexes \n"))
                end
                
            | _ -> raise Contains_no_pvar
        end

    | Lval(Mem(e), off ) ->
      Format.printf "Guard of mem acces of e";
      begin 
        match e.enode , off with
            (Lval(Var(v'),_),NoOffset) ->
              Format.printf "Mem(e) :*%s- \n" v'.vname ;
              PVar(v'.vname)

          | (Lval(Var(v'),_), Field(finfo,offs)) -> 
            let pointer_name = Format.sprintf "%s->" v'.vname in
            let pointer_name = get_subfield_name pointer_name finfo offs in
            Format.printf "%s \n" pointer_name;
            PVar(pointer_name)
            
          
          | (_,Index(_,_)) -> Format.printf "Some index \n"; 
            raise (Debug_info ("In get_pvar_from_exp_node : I don't handle
  array indexes here and there is no reason why I should do it here.\n"))
          | (_,_) -> raise (Debug_info ("Lost in get_pvar_from_exp_node \n"))
      end
         

    | CastE (TPtr (_,_), e ) ->
        get_pvar_from_exp e

    | Const ( CInt64 (i ,_,_)) -> 
      if (My_bigint.is_zero i)  then 
        raise Loc_is_nil
      else raise (Loc_is_a_constant(My_bigint.to_int64 i))
    
    | BinOp (PlusPI,e1,_,_) 
    | BinOp (MinusPI,e1,_,_)
        ->
        get_pvar_from_exp e1

    | BinOp (b,_,_,_) ->
        let b = pprint_binop_op b in
        let msg = "[get_pvar_from_exp :] Don't know what
to do with Binop operator "^b in
          raise (Debug_info(msg))

    | Info (_,_) ->raise (Debug_info("[get_pvar_from_exp :] Don't know what
to do with Info"))

    | AddrOf (_) ->  raise (Debug_info("[get_pvar_from_exp :] Don't know what to do with AddrOf"))

    | _ ->  raise Contains_no_pvar
          
and  get_pvar_from_exp (expr : Cil_types.exp ) =
  get_pvar_from_exp_node expr.enode
*)
