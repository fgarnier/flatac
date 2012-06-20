open Lexing
open Ntl_lexer
open Ntl_parser
open Ntsint



  


let _ =
  if ( Array.length  Sys.argv ) != 2 then
    begin
      Format.printf " parse_n_print File.nts. \n Aborting \n"
    end
  else
    let filename = Sys.argv.(1) in
    let input_channel =
      ( 
	try
	 ( open_in filename )
	with
	    Sys_error(a) -> 
	      begin
		Format.printf "Can't open file  %s, aborting \n" filename;
		raise ( Sys_error(a) )
	      end
      )
    in
    let buf = Lexing.from_channel input_channel in
    let nt_system = Ntl_parser.ntldescr Ntl_lexer.token buf in
    let out_put_reparsed = Nts_int.pprint_nts nt_system in
    Format.printf "%s \n %!" out_put_reparsed
    
    
    
    
