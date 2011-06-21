(** This files contains the fonctions and the definitions required
to decide the entailement property. *)

open Hashtbl
open List
open Ssl_types 
open Ssl
open Ssl_normalization
open SSL_lex
open Ssl_substitution
open Ssl_decision


exception Get_a_locvar of locvar
exception No_more_vars

type entail_problem = {left : ssl_formula ; right : ssl_formula ;}
type fresh_loc_var = FLVar of string * int


let flvar_to_locvar fvar = 
  match fvar with
      FLVar (name , c ) -> LVar ( (Format.sprintf "%s_%d" name c ) )

(** On elimine les x ->l  ... |- x-> l
*)

let fresh_flvar (flvar : fresh_loc_var ) =
  match flvar with
      FLVar  (name , c ) ->  FLVar (name , (c+1))
  
(** Computes the name of a location variable, that is fresh for both
SSL formulae of the entailement problem
*)

(** returns the set of the existencially quantified locvar of the heap,
that are not pointed at by any pointer variables. *)

let garbage_exists_lvar_heap ( sslf : ssl_formula ) =
  let ret = Hashtbl.create SSL_lex.size_hash in
  let garb_iterator lvar _ =
    if (not (free_var sslf lvar)) && (not (is_locvar_pointed_at lvar sslf.pure ))
    then Hashtbl.add ret lvar ()
  in
    match sslf.space with
	Space (space_table ) ->
	  Hashtbl.iter garb_iterator space_table; ret
      | Top_heap -> ret
  

let fresh_locvar_name_from_etp (etp : entail_problem ) =
  let max_varloc_name_lex_fold_affect_table lvar () lvar_param =
    if Ssl.cmp_lex_lvar lvar lvar_param
    then lvar
    else lvar_param
  in
  let max_varloc_name_lex_fold_table _ loctable lvar_param =
    let maxtbl = Hashtbl.fold max_varloc_name_lex_fold_affect_table loctable lvar_param  in
    if Ssl.cmp_lex_lvar maxtbl lvar_param then
      maxtbl
    else
      lvar_param
  in
  let max_varloc_name_space_fold  lvar _ lvar_param =
    if cmp_lex_lvar lvar lvar_param
    then lvar
    else lvar_param
  in
  let var_max = Hashtbl.fold max_varloc_name_lex_fold_table etp.left.pure.affectations (LVar("")) in
  let var_max = Hashtbl.fold max_varloc_name_lex_fold_table etp.right.pure.affectations var_max in
  let  var_max = Hashtbl.fold  max_varloc_name_lex_fold_affect_table etp.left.quant_vars var_max  in
  let  var_max = Hashtbl.fold  max_varloc_name_lex_fold_affect_table etp.right.quant_vars var_max in
  match etp.left.space , etp.right.space with
      ( Space(tableg) , Space(tabled) ) ->
	begin
	  let  var_max =  Hashtbl.fold max_varloc_name_space_fold tableg var_max in
	  let  var_max =  Hashtbl.fold max_varloc_name_space_fold tabled var_max in
	  match var_max with
	      LVar ( vname ) -> FLVar( vname, 1)
	end
    | (_,_) -> raise Top_heap_exception
  

(** Used to compute the biggest location variabl in a (locvar, unit ) t 
Hash table.*)
  
let varname_folder lvar () lvar_arg =
  if cmp_lex_lvar lvar lvar_arg then
    lvar 
  else lvar_arg
 
let entail_r1  ( etp : entail_problem ) = 
  let r1_iterator pvar loctable  =
    if Hashtbl.mem etp.right.pure.affectations pvar then
    let lvar_rel = Hashtbl.fold varname_folder loctable (LVar("")) in
    let pvar_right = Hashtbl.find etp.right.pure.affectations pvar in
    if Hashtbl.mem pvar_right lvar_rel then
      if  ( not ( Hashtbl.mem etp.right.quant_vars lvar_rel )  ) &&  ( not ( Hashtbl.mem etp.left.quant_vars lvar_rel ) = false ) 
      then
	begin
	  Hashtbl.remove etp.right.pure.affectations pvar; 
	  Hashtbl.remove etp.left.pure.affectations pvar
	end
    else ()
  in
  Hashtbl.iter r1_iterator etp.left.pure.affectations
  

let pick_first_lvar ( loctable : ( locvar , unit ) t) =
  let it_table lvar () =
    raise ( Get_a_locvar ( lvar ) )
  in
  try 
    Hashtbl.iter it_table loctable; (LVar(""))
  with
      Get_a_locvar ( lvar ) -> lvar
 

let entail_r4 ( etp : entail_problem ) =
  let r4_iterator pvar loctable  =
    if Hashtbl.mem etp.right.pure.affectations pvar then
    (*let lvar_rel = Hashtbl.fold varname_folder loctable (LVar("")) in*)
    (*begin match lvar_rel with 
	LVar(varname ) ->
	  Format.printf " lvar_rel = %s \n" varname 
    end;*)
    let pvar_right_table = Hashtbl.find etp.right.pure.affectations pvar in
    let locv_left =  pick_first_lvar (Hashtbl.find etp.left.pure.affectations pvar) in
    let locv_right =  pick_first_lvar (pvar_right_table) in
      if  ( Hashtbl.mem etp.right.quant_vars locv_right ) && (  Hashtbl.mem etp.left.quant_vars locv_left )
      then
	let fresh_flvar = fresh_locvar_name_from_etp etp in
	let fresh_lvar = flvar_to_locvar fresh_flvar in

	let subst_table = Hashtbl.create SSL_lex.size_hash in
	Hashtbl.add subst_table locv_left fresh_lvar;
	Hashtbl.add subst_table locv_right fresh_lvar;
	let subst = Subst ( subst_table ) in
	Hashtbl.remove etp.right.pure.affectations pvar; 
	Hashtbl.remove etp.left.pure.affectations pvar ;
	subst_against_ssl subst etp.right;
	subst_against_ssl subst etp.left 
  in
  Hashtbl.iter r4_iterator etp.left.pure.affectations;
  var_elim etp.left;
  var_elim etp.right


let entail_r2 ( etp : entail_problem ) =
  let r2_iterator lvar occurence  =
    if occurence != 1 then ()
    else
      match etp.left.space ,  etp.right.space with
	  (Space (space_table_left) , Space (space_table_right )) ->
	    if Hashtbl.mem space_table_right lvar then
	      let occurence_right = Hashtbl.find space_table_right lvar in
	      (* Both locvar are free vars in both equations *)   
	      if  (free_var  etp.left lvar ) && ( free_var etp.right lvar ) && (occurence_right == 1)
	      then
		begin
		  Hashtbl.remove space_table_left lvar;
		  Hashtbl.remove space_table_right lvar
		end
	      else ()
	| (_,_) -> raise Top_heap_exception
  in
  try
    match  etp.left.space with
	Space( space_table_left ) -> Hashtbl.iter r2_iterator space_table_left
      | Top_heap -> raise Top_heap_exception
  with
      Top_heap_exception -> raise Top_heap_exception




let entail_r6 (etp : entail_problem ) =
  let del_garbage_iterator table_g table_d garbage_d lvar () =
      (*if not ( Hashtbl.mem table_g lvar) then ()*)
      (* else *)
    let occurences = Hashtbl.find table_g lvar in
    if occurences != 1 then ()
    else 
      if ( Hashtbl.length table_d ) == 0
      then raise No_more_vars
      else 
	let lvar_d = pick_first_lvar garbage_d in
	begin
	  match lvar_d with 
	      LVar("") -> ()
	    | LVar(value) ->
	      let occ_lvard = Hashtbl.find table_d lvar_d in
	      if occ_lvard == 1 then
		begin
		  Hashtbl.remove table_g lvar;
		  Hashtbl.remove table_d lvar_d;
		  Hashtbl.remove garbage_d lvar_d
		end
	      else ()
	end
	  
  in
  let garb_left = garbage_exists_lvar_heap etp.left in
  let garb_right = garbage_exists_lvar_heap etp.right in
 
  match etp.left.space , etp.right.space with
      (Space (table_g) , Space (table_d)) ->
	begin
	  try
	    Hashtbl.iter (del_garbage_iterator table_g table_d garb_right) garb_left; Ssl_normalization.var_elim etp.left; Ssl_normalization.var_elim etp.right
	  with
	      No_more_vars -> ()
	end
    | (_,_) -> ()





    
