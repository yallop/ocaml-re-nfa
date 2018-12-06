(** Top-level program for building DOT representations of NFAs from regexes *)

let die fmt = Printf.kprintf (fun s -> prerr_endline s; exit 1) fmt

let typ = ref "nfa"

let spec = 
  [("-type",
    Arg.Symbol (["nfa"; "dfa"; "dfa-minimized"],
                (:=) typ),
    "Output type")]

let usage = "re-nfa [options] regex"

let parse_re r = 
  try Regex.parse r
  with Regex.Parse_error s -> die "Invalid regular expression: %S\n" s

let () =
  let r = ref None in
  let collect s =
    match !r with None -> r := Some s
                | Some _ -> ()
  in
  Arg.parse spec collect usage;
  match !r, !typ with
  | None, _ -> ()
  | Some r, "nfa" -> 
     let nfa = Regex.compile (parse_re r) in
     let digraph = Nfa_dot.digraph_of_nfa nfa in
     Format.printf "%a@." Nfa_dot.format_digraph digraph;
  | Some r, "dfa" ->
     let nfa = Regex.compile (parse_re r) in
     let dfa = Dfa.determinize nfa in
     let digraph = Nfa_dot.digraph_of_nfa (Dfa.inject dfa) in
     Format.printf "%a@." Nfa_dot.format_digraph digraph;
  | Some r, "dfa-minimized" ->
     let nfa = Regex.compile (parse_re r) in
     let dfa = Dfa.minimize (Dfa.determinize nfa) in
     let digraph = Nfa_dot.digraph_of_nfa (Dfa.inject dfa) in
     Format.printf "%a@." Nfa_dot.format_digraph digraph;
  | _ -> Arg.usage spec usage
