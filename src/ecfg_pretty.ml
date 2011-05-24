open Ecfg

module Ecfg_pretty =
struct
	let stmtToString stmt =
		Buffer.reset stdbuf;
		Cil.printStmt Cil.defaultCilPrinter str_formatter stmt;
		String.escaped (Buffer.contents stdbuf)
	
	let printDot foc node =
		match node with
		| Leaf (sid, Op (stmtData), _ ) -> ()
		| Node (sid, Op (stmtData), _, _, l) -> 
							List.iter 	
							( fun e -> 
								match e with
								| Leaf (childSid, Op ( childStmtData ), _) -> 
									Format.fprintf foc "%d [label=\"%d - %s\"]\n" sid sid (stmtToString stmtData);
									Format.fprintf foc "%d [label=\"%d - %s\"]\n" childSid childSid (stmtToString childStmtData);
									Format.fprintf foc "%d -> %d\n\n" sid childSid
								| Node (childSid, Op ( childStmtData ), _ , _, _) -> 
									Format.fprintf foc "%d [label=\"%d - %s\"]\n" sid sid (stmtToString stmtData);
									Format.fprintf foc "%d [label=\"%d - %s\"]\n" childSid childSid (stmtToString childStmtData);
									Format.fprintf foc "%d -> %d\n\n" sid childSid;
								| _ -> ()
							) l
		| _ -> ()
	
	let exportDot () = 
		let oc = open_out "output.dot" in
		let foc = formatter_of_out_channel( oc ) in
			Format.fprintf foc "digraph G {\n";
				visiteCFGs (printDot foc);
			Format.fprintf foc "\n}";
			Self.feedback ~level:0 "Graph exported!"
end;;
