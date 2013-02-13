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

type control_type = FC_ESID of int | FLATAC_SINTER_SID of int | FLATAC_SINIT
exception UnknownESidDescriptor of string
exception Unreferenced_esid_in of Trace_types.esid *
            (Trace_types.esid, Trace_types.sid) Hashtbl.t
exception NoSubsystem of string * Trace_types.tr_subsystem_table
val type_statename_1 : string -> control_type
val type_statename_5 : string -> control_type
val type_statename_6 : string -> control_type
val type_statename : string -> control_type
val get_esid_of_statename : string -> Trace_types.esid option
val get_sid_statement_of_esid :
  Trace_types.esid ->
  Trace_types.map_2_fcinfos ->
  Trace_types.sid * (string * (Lexing.position * Lexing.position) option)
val sid_anot_info_of_opt_esid :
  Trace_types.esid option ->
  Trace_types.map_2_fcinfos ->
  Trace_types.sid * (string * (Lexing.position * Lexing.position) option)
val sid_infos_of_syscontrol :
  ?annot_less_callee:(string, 'a) Hashtbl.t option ->
  Trace_types.tr_subsystem_table ->
  Trace_types.sys_control ->
  Trace_types.sid * (string * (Lexing.position * Lexing.position) option)
