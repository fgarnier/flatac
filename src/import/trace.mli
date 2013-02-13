(*
This files contains the implementation of the Numerical Transition Library 
--see http://richmodels.epfl.ch/ntscomp-- main objects, namely :

_ Numerical transitions subsystems, (i.e. parametric counter automaton
  with return values upon return)
_ Hyerarchical transistions subsystems .


Plus a parser, a pretty printer as well as cleanup functions.
A type checker will be added.



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



(**
Trace defines a set of function that allows to pretty print 
traces and printing Lexing informations from control state
identifier --called sid, using Frama-C terminology.

This is helpfull to map a counter example trace back to
source code -- e.g. C source code from witch a nts based
model has been extraced using the flata-c plugin.
*)

val print_sys_control : Trace_types.sys_control -> string
val print_trace_l_folder : string -> Trace_types.sys_control -> string
val print_trace : Trace_types.sys_control list -> string
val pprint_esid : Trace_types.esid -> string
val pprint_sid : Trace_types.sid -> string
val pprint_folder_esid_sid_map :
  Trace_types.esid -> Trace_types.sid -> string -> string
val pprint_esidsid_map :
  (Trace_types.esid, Trace_types.sid) Hashtbl.t -> string
val pprint_position : Lexing.position * Lexing.position -> string
val pprint_folder_sid_code_map :
  Trace_types.sid ->
  string * (Lexing.position * Lexing.position) option -> string -> string
val pprint_sid_to_code_info :
  (Trace_types.sid, string * (Lexing.position * Lexing.position) option)
  Hashtbl.t -> string
val pprint_map_2_fcinfo : Trace_types.map_2_fcinfos -> string
val pprint_tr_subsystem_table :
  ('a, Trace_types.map_2_fcinfos) Hashtbl.t -> string
