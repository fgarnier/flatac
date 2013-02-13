(*Written by Florent Garnier, at Verimag Labs  2012 
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
open Ntl_lexer
open Ntl_parser
open Nts_generic
open Nts_functor
open Ntsint



module Dotofintnts = Dot_driver.Make(Ntsint.P)


let openfile_with_guard fname =
  let input_channel =
    ( 
      try
	( open_in fname )
      with
	Sys_error(a) -> 
	  begin
	    Format.printf "Can't open file  %s, aborting \n %!" fname;
	    raise ( Sys_error(a) )
	  end
    ) 
  in
  input_channel

let create_output_file dump_name =
  let dump_file_descr = ( 
    try
      ( open_out dump_name )
    with
      Sys_error(a) -> 
	begin
	  Format.printf "Can't create file  %s, aborting \n %!" dump_name;
	  raise ( Sys_error(a) )
	end
  )
  in
  dump_file_descr
      




(* Main program *)

let _ =
  let num_args =  Array.length  Sys.argv in
  if ( num_args != 2 &&  num_args!=4 ) 
  then
    begin
      Format.printf "Syntax : %s File.nts \n  
 Aborting \n %!" Sys.argv.(0)
    end
  else
    
    begin
      let filename = Sys.argv.(1) in
      let input_channel = openfile_with_guard filename in
      let base_file_name = Filename.basename filename in
      let dir_name = Filename.dirname filename in
      let dump_name = dir_name^"/"^base_file_name^".dot" in
      let dump_channel = create_output_file dump_name in
      let dump_formatter = Format.formatter_of_out_channel dump_channel in
      let buf = Lexing.from_channel input_channel in
      let nt_system = Ntl_parser.ntldescr Ntl_lexer.token buf in
      let nt_system = Nts_int.nt_system_var_cleaner nt_system in
      let output_string = Dotofintnts.dot_of_all_subsystem_of_nts 
	nt_system in
      Format.fprintf dump_formatter "%s" output_string;
      close_out dump_channel;
      close_in input_channel;
      
    end


