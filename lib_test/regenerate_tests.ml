open Tests_common

let regenerate_tests compile = 
  let repeat i j re =
    let r = ref Regex.eps in
    for _ = 1 to i do r := Regex.seq !r re done;
    (match j with
    | None -> r := Regex.seq !r (Regex.star re)
    | Some j -> for _ = succ i to j do r := Regex.seq !r (Regex.opt re) done);
    !r
  in
  let alphabet = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'] in
  let allof cs = List.fold_right (fun c r -> Regex.(alt (chr c) r)) cs Regex.empty in
  let noneof cs = allof (List.filter (fun c -> not (List.mem c cs)) alphabet) in

  let rec to_re = let open Regenerate.Regex in function
    | One -> Regex.eps
    | Set (true, cs) -> allof cs
    | Set (false, cs) -> noneof cs
    | Seq (r1, r2) -> Regex.seq (to_re r1) (to_re r2)
    | Or (r1, r2) -> Regex.alt (to_re r1) (to_re r2) 
    | And _ | Not _ -> (* No support for intersection or complement at present *)
       QCheck.assume_fail ()
    | Rep (i,j,re) -> repeat i j (to_re re)
  in
  let check (re, pos, neg) =
    let cre = compile (to_re re) in
    List.for_all cre pos && not (List.exists cre neg)
  in
  let generator =
    Regenerate.arbitrary
      (module Regenerate.Word.String)
      (module Segments.ThunkList(Regenerate.Word.String))
      ~compl:false ~pp:Fmt.char ~samples:10 alphabet
  in
  ignore (QCheck_runner.run_tests [QCheck.Test.make generator check])

let () =
  begin
    print_string "regenerate tests (NFA)..."; flush stdout;
    regenerate_tests (combinator_matcher
                        ~compile:Regex.compile
                        ~accept:nfa_accept);
    print_endline "OK!";

    print_string "regenerate tests (DFA)..."; flush stdout;
    regenerate_tests (combinator_matcher
                        ~compile:dfa_compile
                        ~accept:dfa_accept);
    print_endline "OK!";

    print_string "regenerate tests (DFA-minimized)..."; flush stdout;
    regenerate_tests (combinator_matcher
                        ~compile:dfa_minimize_compile
                        ~accept:dfa_accept);
    print_endline "OK!";
  end
