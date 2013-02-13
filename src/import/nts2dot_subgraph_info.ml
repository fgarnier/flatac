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
open Ntl_lexer
open Ntl_parser
open Ntsint
open Nts_generic



exception BadOption of string

module Dotofintnts = Dot_driver.Make(Ntsint.P)



let _ =
  let num_args =  Array.length  Sys.argv in
  if ( num_args != 2 &&  num_args!=4 ) 
  then
    begin
      Format.printf "Syntax : nts2dot_subgraph File.nts \n nts2dot -interval File.nts trace_file \n  
 Aborting \n %!"
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
      (*Format.printf "I opened the file %s \n %!" filename;*)
      let base_file_name = Filename.basename filename in
      let dir_name = Filename.dirname filename in
      let dump_name = dir_name^"/"^base_file_name^".dot" in
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
      (*Format.printf "I created the file %s \n %!" dump_name;*)
      
      (*try*)
	let buf = Lexing.from_channel input_channel in
	let nt_system = Ntl_parser.ntldescr Ntl_lexer.token buf in
	let nt_system = Nts_int.nt_system_var_cleaner nt_system in
	(*let output_reparsed = Nts_int.pprint_nts nt_system in*)
      
	let dot_string = 
	  ( if num_args=2 
	    then
	      Dotofintnts.dot_of_nts nt_system 
	    else
	      begin
		if ((String.compare (Sys.argv.(2)) "-interval")!=0)
		then 
		  begin
		    Format.printf "-interval option required \n Instead of %s %!" Sys.argv.(2);  
		    raise (BadOption(Sys.argv.(2)))
		  end
		else ();
		let trace_file_name = Sys.argv.(3) in
		let trace_file_channel = (open_in trace_file_name) in
		let buf = Lexing.from_channel trace_file_channel 
		in
		let trace_l = Trace_eldarica_parser.gettrace 
		  Trace_eldarica_lexer.token  buf
		in
		Dotofintnts.dot_of_subcfg_of_nts nt_system trace_l

	      end
	  )
	in
	let dump_channel = Format.formatter_of_out_channel dump_file_descr 
	in
	Format.fprintf dump_channel "%s\n %!" dot_string;
	close_out dump_file_descr;
	
	exit 0
     (* with
	| _ -> 
	  begin
	    prerr_string "Parse error \n";
	    exit 1
	end *)
    end
   
