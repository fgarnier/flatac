open Lexing
open Trace_types
open Trace
open Trace_eldarica_lexer
open Trace_eldarica_parser


let pprint_sys_control s =
	match s with
	    Trace_types.Sys_control(sname,cname) ->
	      sname^"_"^cname 

let pprint_trace_tansitions tr =
  let pprint_trace_transitions_folder (prefix_printer,previous_state) 
      curr_control =
    match previous_state with 
      None -> 
	("",Some(curr_control))
    | Some(prev) -> 
      begin
	let out_string = Format.sprintf "%s %s -> %s \n" prefix_printer 
	  (pprint_sys_control prev) (pprint_sys_control curr_control) in
	(out_string,Some(curr_control))
      end
  in
  let  (ret_string, _ ) = 
    List.fold_left pprint_trace_transitions_folder ("",None) tr 
  in
  ret_string


let print_str_l_folder prefix s =
  if String.length prefix = 0 then
     s
  else
    prefix^","^s


let _ = if ( Array.length  Sys.argv ) != 2 then
    begin
      Format.printf " You need to provide a trace filename as an argument \n Aborting \n %!"
    end
  else ();
   
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
  
      
  let buf = Lexing.from_channel input_channel 
  in
  
   let trace_l = Trace_eldarica_parser.gettrace Trace_eldarica_lexer.token  buf
   in
   
   Format.printf "Trace is %s \n" (pprint_trace_tansitions trace_l)
      
      
