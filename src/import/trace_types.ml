type sys_control = Sys_control of string * string (* Name of subsystem
							name of the control
						   state*)

type trace = sys_control list

type esid = ESID of int
type sid = SID of int
type esidtosidrel =  esid * sid



type map_2_fcinfos = 
	{
	tr_sysname : string ;
	 esid_to_sid_map : (esid,sid) Hashtbl.t;	
	esid_to_statement_infos : (sid, string * (Lexing.position *Lexing.position) option) Hashtbl.t;
	}	

type tr_subsystem_table = (string, map_2_fcinfos ) Hashtbl.t

	


