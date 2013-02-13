(*

Florent Garnier
(C) Verimag 2012
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

(** Type definition for execution traces.*)

type sys_control = Sys_control of string * string
type trace = sys_control list
type esid = ESID of int
type sid = SID of int
type esidtosidrel = esid * sid
type map_2_fcinfos = {
  tr_sysname : string;
  esid_to_sid_map : (esid, sid) Hashtbl.t;
  esid_to_statement_infos :
    (sid, string * (Lexing.position * Lexing.position) option) Hashtbl.t;
}
type tr_subsystem_table = (string, map_2_fcinfos) Hashtbl.t
