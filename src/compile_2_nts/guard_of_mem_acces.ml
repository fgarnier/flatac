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



let rec offset_to_cnt sslv (t : Cil_types.typ ) ( off : Cil_types.offset) =
  match off with
      NoOffset -> CntCst(0)
    | Index (exp ,  off ) -> 
      let offset_exp = compile_cil_exp_2_cnt sslv exp   

let rec cnt_guard_of_mem_access sslv (exp_type : Cil_types.typ) =
    ( expn : Cil_types.exp_node ) =
  match  expn with 
      Lval( Var (p) , off )->
	begin
	  match off with
	      NoOffset ->
		CntBTrue (* à voir ce qui se passe avec les tableaux *)
	  
 	end
    | Lval( Mem(e) , off ) -> (*Access at the offset off of base memory e*)
      begin
	let pvar_access = get_pvar_of_expr e in
	try
	  let location_var = Ssl.get_pointer_affectation sslv.ssl_part pvar_access in
	  if (SSl.is_allocated location_var sslv.ssl_part )
	  then 
	    begin
	      let offset_name_of_pvar = make_offset_loc_pvar pvar_access in
	      let offset_of_e = 
	    end
	  else
	    CntBFalse

	with
	    _ -> 
	      begin
		Format.printf "The ptrvar %s has not matching affectation in the heap \n" 
		CntBFalse
	      end
      end


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
	let 
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
