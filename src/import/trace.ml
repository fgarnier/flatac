(*


Written by Florent Garnier, at Verimag Labs  2012 
Contact florent dot garnier at gmail dot com for  further informations.

This files is released under the terms of the LGPL v2.1 Licence.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor,
 Boston, MA  02110-1301  USA




*)

open Trace_types
open Lexing


let print_sys_control s =
  match s with 
    Sys_control(sname,cname) -> "("^sname^";"^cname^")"

let print_trace_l_folder prefix s =
  if String.length prefix = 0 then
    print_sys_control s
  else
    prefix^","^(print_sys_control s)

let print_trace t =
  List.fold_left print_trace_l_folder "" t
  
let pprint_esid e =
  match e with ESID(id) -> Format.sprintf "%d" id

let pprint_sid s =
   match s with SID(id) -> Format.sprintf "%d" id

let pprint_folder_esid_sid_map key map prefix =
  Format.sprintf "%s %s>>%s\n" prefix ( pprint_esid key) (pprint_sid map)

let pprint_esidsid_map tbl = 
  let ret_str = 
    Hashtbl.fold pprint_folder_esid_sid_map  tbl "ESID_TO_SID_MAP{{"
  in
  ret_str^"}};;"



let pprint_position (lex_b,lex_e) =
  Format.sprintf "POSINFOS{{pos_fname =%s;pos_lnum=%d;pos_bol=%d;pos_cnum=%d;pos_fname =%s;pos_lnum=%d;pos_bol=%d;pos_cnum=%d;}}" lex_b.pos_fname lex_b.pos_lnum lex_b.pos_bol lex_b.pos_cnum  lex_e.pos_fname lex_e.pos_lnum lex_e.pos_bol lex_e.pos_cnum 


let pprint_folder_sid_code_map key code prefix =
  match code with 
    (ins,None)->
   Format.sprintf "%s sid : %s ; C-Code%c{{%c%s%c}}%c;;\n" prefix ( pprint_sid key) '@' '@' ins '@' '@' 
  | (ins, Some(lex_b,lex_e)) ->
    begin
      let code_pos = pprint_position (lex_b,lex_e) in
      Format.sprintf "%s sid : %s ; C-Code%c{{%c%s%c}}%c %s ;;\n" prefix ( pprint_sid key) '@' '@' ins '@' '@'  code_pos
    end

let pprint_sid_to_code_info tbl = 
  let ret_str = 
    Hashtbl.fold pprint_folder_sid_code_map  tbl "SID_TO_CODE_MAP{{"
  in
  ret_str^"}};;"


let pprint_map_2_fcinfo m =
  let esid_sid = pprint_esidsid_map m.esid_to_sid_map in
  let code_map = pprint_sid_to_code_info m.esid_to_statement_infos in
  Format.sprintf "{{Function = %s ;; %s \n %s }}" 
      m.tr_sysname esid_sid code_map


let pprint_tr_subsystem_table tbl =
  let print_folder _ map pre =
    Format.sprintf "%s %s\n" pre (pprint_map_2_fcinfo map)
  in
  Hashtbl.fold print_folder  tbl ""
