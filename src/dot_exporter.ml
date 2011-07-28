open Self

open Cil
open Cil_types
open Visitor

open Ecfg
open Sem_and_logic_front_end

open Format
open Buffer

module Dot_exporter =
  functor ( A : sig type abstract_type type label_type end ) ->
struct
  type semantic_abstraction = A.abstract_type
  type counter_expression = A.label_type

  type semantic = Semantic of stmt * semantic_abstraction

  type ecfg_node_id = int
  type ecfg_edge = counter_expression
  type ecfg_node = Node of semantic * (ecfg_node_id, ecfg_edge) Hashtbl.t

  let stmt_to_string stmt =
    Buffer.reset stdbuf;
    match stmt.skind with
      | Instr ( Set ( (Var ( lvalueInfo ), _) , expression, _ ) ) ->
          let vname = lvalueInfo.vname in
            add_string stdbuf vname; add_string stdbuf " = ";
            Cil.printExp Cil.defaultCilPrinter str_formatter expression; 
            String.escaped (flush_str_formatter ())
      | If ( expression, _, _, _) -> 
          add_string stdbuf " IF ";
          Cil.printExp Cil.defaultCilPrinter str_formatter expression; 
          String.escaped (flush_str_formatter ())
      | Loop ( _, _, _, _, _ ) ->
          add_string stdbuf " WHILE ( 1 )";
          String.escaped (flush_str_formatter ())
      | Return ( Some ( expression ), _ ) ->
          add_string stdbuf " RETURN ";
          Cil.printExp Cil.defaultCilPrinter str_formatter expression; 
          String.escaped (flush_str_formatter ())
      | Return ( None, _ ) ->
          add_string stdbuf " RETURN ;";
          String.escaped (flush_str_formatter ())
      | UnspecifiedSequence _ ->
          add_string stdbuf " UNSPECIFIED :  ";
          Cil.printStmt Cil.defaultCilPrinter str_formatter stmt; 
          String.escaped (flush_str_formatter ())
      | Block (_) ->
          add_string stdbuf " BLOCK ";
          String.escaped (flush_str_formatter ())
      | _ -> 
          Cil.printStmt Cil.defaultCilPrinter str_formatter stmt; 
          String.escaped (flush_str_formatter ())

  let replace_chars (f : (char -> string)) (s : string) =
    let len = String.length s in
    let tlen = ref 0 in
    let rec loop i acc =
      if i = len then
        acc
      else 
        let s = f (String.get s i) in
          tlen := !tlen + String.length s;
          loop (i+1) (s :: acc)
    in
    let strs = loop 0 [] in
    let sbuf = String.create !tlen in
    let pos = ref !tlen in
    let rec loop2 = function
      | [] -> ()
      | s :: acc ->
          let len = String.length s in
            pos := !pos - len;
            String.blit s 0 sbuf !pos len;
            loop2 acc
    in
      loop2 strs;
      sbuf

  let print_dot foc front_end _ uid node =
    match node with
      | Node (Semantic ( statement, abstraction ), listOfEdges) -> 
          Format.fprintf foc 
            "\t\t%d [texlbl=\"\\begin{minipage}{16cm}\\centering %d\\\\ \
            \\lstinline{%s}\\\\ %s\\end{minipage}\"]\n" 
            uid statement.sid (replace_chars (fun c -> 
                                                if c = '{' then "["
                                                else if c = '}' then "]"
                                                else if c = '"' then "'"
                                                else if c = '%' then ""
                                                else let newStr = String.create 1 in
                                                  newStr.[0]<- c;
                                                  newStr
            )
                                 (stmt_to_string statement)) 
            (front_end#pretty abstraction); 
          Hashtbl.iter ( fun toUid counterValue  -> 
                           Format.fprintf foc 
                             "\t\t%d -> %d [texlbl=\"%s\"]\n\n" uid toUid
                             (front_end#pretty_label counterValue)
          ) listOfEdges

  (** This function export the given ecfg in dot format into the given file.
    If the file does not exist, it is created. It is overwritten otherwise. *)
  let export_dot visitor_fun ecfgs filename front_end= 
    let oc = open_out filename in
    let foc = formatter_of_out_channel( oc ) in
      Format.fprintf foc "digraph G {\n";
      visitor_fun ecfgs 
        ( fun fname -> Format.fprintf foc 
                         "\tsubgraph cluster_%s {\n \
                         \t\tnode [style=filled,shape=box,color=white]; \n \
                         \t\tstyle=filled; \n \
                         \t\tcolor=lightgray; \n \
                         \t\tlabel = \"%s\"; \n \
                         \t\tfontsize=40; \n\n" fname fname )
        ( fun _ -> Format.fprintf foc "\t}\n" ) 
        (print_dot foc front_end);
      Format.fprintf foc "\n}";
      Self.feedback ~level:0 "Graph exported!"

end;;
