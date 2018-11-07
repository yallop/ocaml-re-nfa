(** Top-level program for building DOT representations of NFAs from regexes *)

let die fmt = Printf.kprintf (fun s -> prerr_endline s; exit 1) fmt

let () =
  match Array.length Sys.argv with
  | 2 ->
     begin match Regex.parse (Sys.argv.(1)) with
     | exception Regex.Parse_error s -> die "Invalid regular expression: %S\n" s
     | re -> let nfa = Regex.compile re in
             let digraph = Nfa_dot.digraph_of_nfa nfa in
             Format.printf "%a@." Nfa_dot.format_digraph digraph
     end
  | n -> die "Usage:\n\tre-nfa <regex>"

