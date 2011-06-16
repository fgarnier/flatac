(** This files contains the fonctions and the definitions required
to decide the entailement property. *)

open Hashtbl
open List
open Ssl_types 
open Ssl
open Ssl_normalization
open SSL_lex

exception Get_a_locvar of locvar

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
    raise  Get_a_locvar ( lvar )
  in
  try 
    Hashtbl.iter it_table loctable; (LVar(""))
  with
      Get_a_locvar ( lvar ) -> lvar
 

let entail_r4 ( etp : entail_problem ) =
  let r4_iterator pvar loctable  =
    if Hashtbl.mem etp.right.pure.affectations pvar then
    let lvar_rel = Hashtbl.fold varname_folder loctable (LVar("")) in
    let pvar_right = Hashtbl.find etp.right.pure.affectations pvar in
    if Hashtbl.mem pvar_right lvar_rel then
      if  ( Hashtbl.mem etp.right.quant_vars lvar_rel ) && (  Hashtbl.mem etp.left.quant_vars lvar_rel )
      then
	let fresh_flvar = fresh_locvar_name_from_etp etp in
	let fresh_lvar = flvar_to_locvar flvar in
	let locv_left =  (Hashtbl.find etp.left.pure.affectations pvar) in
	let locv_right =  (Hashtbl.find etp.right.pure.affectations pvar) in
	let subst_table = Hashtbl.create Ssl.size_hash in
	Hashtbl.add subst_table locv_left fresh_lvar;
	Hashtbl.add subst_table locv_right fresh_lvar;
	let subst = Subst ( subst_table) in
	Hashtbl.remove etp.right.pure.affectations pvar; 
	Hashtbl.remove etp.left.pure.affectations pvar;
	subst_against_ssl subst etp.right;
	subst_against_ssl subst etp.left
	
	  
