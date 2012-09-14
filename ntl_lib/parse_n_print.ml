open Lexing
open Ntl_lexer
open Ntl_parser
open Ntsint
open Nts_generic



  


let _ = 
  if ( Array.length  Sys.argv ) != 2 then
    begin
      Format.printf " parse_n_print File.nts. \n Aborting \n %!"
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
      let dump_name = dir_name^"/"^base_file_name^"_dump" in
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
      
      try
	let buf = Lexing.from_channel input_channel in
	let nt_system = Ntl_parser.ntldescr Ntl_lexer.token buf in
	let nt_system = Nts_int.nt_system_var_cleaner nt_system in
	let output_reparsed = Nts_int.pprint_nts nt_system in
      (*Format.printf "%s \n %!" output_reparsed;*)
	let dump_channel = Format.formatter_of_out_channel dump_file_descr 
	in
	Format.fprintf dump_channel "%s\n %!" output_reparsed;
	close_out dump_file_descr;
	exit 0
      with
	| _ -> 
	  begin
	    prerr_string "Parse error \n";
	    exit 1
	  end
    end
   

    
    
