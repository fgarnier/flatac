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


open Lexing
open Trace_types
open Sid_table_info_parser 
open Sid_table_info_lexer
open Trace


let _ = 
  if ( Array.length  Sys.argv ) != 2 then
    begin
      Format.printf "  A file name is expected as argument \n Aborting \n %!"
    end
  else
    begin
      let filename = Sys.argv.(1) in
      let input_channel =
	( 
	  try
	    ( open_in filename )
	  with
	      Sys_error(a) -> 
		begin
		  Format.printf "Can't open file  %s, aborting \n %!" filename;
		  raise ( Sys_error(a) )
		end
	)
      in
      
      try
	let buf = Lexing.from_channel input_channel in
	let sid_infos =  Sid_table_info_parser.mapextract Sid_table_info_lexer.token buf  in
	let print_out =  
	   Trace.pprint_tr_subsystem_table sid_infos
	in
	Format.printf "%s" print_out 


      with
	| _ -> 
	  begin
	    prerr_string "Parse error \n";
	    exit 1
	  end
    end
   


