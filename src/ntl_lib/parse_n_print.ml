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
      Format.printf "I opened the file %s \n %!" filename;
      let buf = Lexing.from_channel input_channel in
      let nt_system = Ntl_parser.ntldescr Ntl_lexer.token buf in
      let out_put_reparsed = Nts_int.pprint_nts nt_system in
      Format.printf "%s \n %!" out_put_reparsed
    end
   

    
    
