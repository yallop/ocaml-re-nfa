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


let test compile =
  begin
    let a = compile "a" in
    assert (a "a");
    assert (not (a "b"));
    assert (not (a ""));
    assert (not (a "aa"));

    let ab = compile "ab" in
    assert (not (ab "a"));
    assert (not (ab "b"));
    assert (not (ab ""));
    assert (ab "ab");

    let aab = compile "aab" in
    assert (not (aab "a"));
    assert (not (aab "b"));
    assert (not (aab ""));
    assert (aab "aab");
    assert (not (aab "aaab"));

    let aorb = compile "a|b" in
    assert (aorb "a");
    assert (aorb "b");
    assert (not (aorb "c"));
    assert (not (aorb "ab"));
    assert (not (aorb ""));

    let opta = compile "a?" in
    assert (opta "a");
    assert (opta "");
    assert (not (opta "b"));
    assert (not (opta "aa"));

    let astar = compile "a*" in
    assert (astar "");
    assert (astar "a");
    assert (astar "aaaa");
    assert (not (astar "aaaab"));
    assert (not (astar "a*"));

    let abstar = compile "(ab)*" in
    assert (abstar "");
    assert (abstar "ab");
    assert (abstar "ababab");
    assert (not (abstar "ababa"));
    
    let aorbstar = compile "(a|b)*" in
    assert (aorbstar "");
    assert (aorbstar "a");
    assert (aorbstar "b");
    assert (aorbstar "babababa");
    assert (not (aorbstar "bababacba"));

    let adotstarb = compile "a.*b" in
    assert (adotstarb "ab");
    assert (adotstarb "aasdfasdfasdb");
    assert (not (adotstarb "aasdfasdfasdbc"));
    assert (not (adotstarb "caasdfasdfasdb"));

    (* Some tests for precedence *)
    (* a(bc|de|fg)h should mean a((bc)|(de)|(fg))h, not a(b(c|d)(e|f)g)h *)
    let abcedfgh = compile "a(bc|de|fg)h" in
    assert (abcedfgh "abch");
    assert (abcedfgh "adeh");
    assert (abcedfgh "afgh");
    assert (not (abcedfgh "abgh"));
    assert (not (abcedfgh "abcegh"));

    (* abc⋆d should mean ab(c⋆)d *)
    let abcstard = compile "abc*d" in
    assert (abcstard "abd");
    assert (abcstard "abcd");
    assert (abcstard "abcccd");
    assert (not (abcstard "abcabcd"));

    (* a|b*c should mean a|(b*c) *)
    let aorbstarc = compile "a|b*c" in
    assert (aorbstarc "a");
    assert (aorbstarc "c");
    assert (aorbstarc "bc");
    assert (aorbstarc "bbbbc");
    assert (not (aorbstarc "aac"));

    (* a| should mean a|ε *)
    let aornothing = compile "a|" in
    assert (aornothing "a");
    assert (aornothing "");

    (* a(||b|)c should mean a(ε|ε|b|ε)c *)
    let anothingnothingbnothingc = compile "a(||b|)c" in
    assert (anothingnothingbnothingc "ac");
    assert (anothingnothingbnothingc "abc");
    assert (not (anothingnothingbnothingc "a|c"));
  end


let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let combinator_matcher ~compile ~accept =
  fun r -> let c = compile r in fun s -> accept c s

let string_matcher ~compile ~accept =
  fun r -> combinator_matcher (Regex.parse r) ~compile ~accept 

let nfa_accept r s = Nfa.accept r (explode s)
let dfa_accept r s = Dfa.accept r (explode s)

let dfa_compile r = Dfa.determinize (Regex.compile r)

let () =
  begin
    print_string "testing NFA acceptance..."; flush stdout;
    test (string_matcher
            ~compile:Regex.compile
            ~accept:nfa_accept);
    print_endline "OK!";

    print_string "testing DFA acceptance..."; flush stdout;
    test (string_matcher
            ~compile:dfa_compile
            ~accept:dfa_accept);
    print_endline "OK!";

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
  end
