(** This files contains the functions that allows to pretty print
any part of a SSL formula into a string of charcater.
*)

open Ssl_types
open Ssl
open SSL_lex
open Printf
open List

(** Prints an equation in a string*)
let pprint_ssl_eq eq =
  match  eq with
      Eqloc ( LVar (lg) , LVar(ld)) ->
	"("+lg+"=="+ld+")"

(** Prints an equation list into a string *)
let  pprint_ssl_eqlist ( eqlist : eq list ) =
  let accu = "Eq [" in 
  let rec pprint_list_eq_fold l accu =
    match l with 
	[] -> accu+"]"
      |  h::l' ->  pprint_list_eq_fold  l' (accu+";"+(pprint_ssl_eq h)) 
  in
  pprint_list_eq_fold eqlist accu


let pprint_ssl_affectation table_aff =
  let accu = "Aff [" in
  let locvar_table_fold pvar lvar () accu =
    match pvar ,  lvar with 
	(PVar(pv) , LVar (lv)) ->  (accu+";"+pv+"->"+lv)
  in
  let aff_table_fold pvar lvar_table accu =
    let ret_str = Hashtbl.fold (locvar_table_fold pvar) lavr_table accu
    in ret_str
  in
  let str_print =
    Hashtbl.fold aff_table_fold table_aff accu in
  ( str_print+ "]")

let pprint_ssl_ptnil  ptnil_table =
  let accu = "Ptnil [" in
  let ptnil_folder pvar () accu =
    match pvar with 
	PVar ( vname ) ->   ( accu +";"+ vname+"-> Nil")
  in
  let ret_string = Hashtbl.fold ptnil_folder ptnil_table accu in
  ( ret_string + "]")


let pprint_space_formula sformula =
  let heap_print_folder lvar occurence accu =
    match lvar with  
	LVar (vname ) -> accu +"*"+( sprintf "Alloc(%s,%i)" vname occurence)
  in
  match sformula with 
      Space (heap_table) -> 
	if Hashtbl.lenght heap_table == 0
	then
	  "Emp"
	else
	  let ret_string = Hashtbl.fold heap_print_folder heap_table "Space ["
	  in 
	  (ret_string + "]")

    | Top_heap -> "Top_heap" 
  

let pprint_quant_vars quant_vars_table = 
  let accu = "Exists [ " in
  let locvar_table_fold lvar () accu =
    match   lvar with 
	LVar (lv) ->  ( accu + ";" + lv)
  in
  let ret_string = (Hashtbl.fold locvar_table_fold quant_vars_table accu)
  in
  (ret_string + "]")

let pprint_ssl_formula (sslf : ssl_formula ) =
  let ret_str =
    (pprint_quant_vars sslf.quant_vars)+"Pure{"+
    (pprint_ssl_eqlist sslf.pure.equations)
    +(pprint_ssl_affectation sslf.pure.affectations)
    +(pprint_ssl_ptnil sslf.pure.ptnil)
    +"} || Space{"+(pprint_space_formul sslf.space)+"}" in
  ret_str
