open Tests_common

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

    (* Character sets *)
    let rbracket = compile "a[]]b" in
    assert (rbracket "a]b");
    assert (not (rbracket "ab"));
    assert (not (rbracket "a[b"));

    let notrbracket = compile "a[^]]b" in
    assert (not (notrbracket "a]b"));
    assert (notrbracket "a.b");
    assert (notrbracket "a-b");
    assert (notrbracket "a[b");

    let alphanum = compile "[a-zA-Z0-9]+" in
    assert (alphanum "abcDEF789");
    assert (alphanum "Zz0");
    assert (not (alphanum "abcDE?F789"));
    assert (not (alphanum ""));

    let hyphen = compile "[--]" in
    assert (hyphen "-");
    assert (not (hyphen "--"));
    assert (not (hyphen ""));
    assert (not (hyphen "a"));

    let acaret = compile "[a^]" in
    assert (acaret "a");
    assert (acaret "^");
    assert (not (acaret "b"));

    let acarethyphen = compile "c[a^-]d" in
    assert (acarethyphen "cad");
    assert (acarethyphen "c^d");
    assert (acarethyphen "c-d");
    assert (not (acarethyphen "cbd"));

    let nonalphanum = compile "[^0-9A-Za-z]+" in
    assert (nonalphanum "?.%");
    assert (not (nonalphanum "a0e"));

    let metas = compile "a[]\\d^-]b" in
    assert (metas "a]b");
    assert (metas "a\\b");
    assert (metas "adb");
    assert (metas "a^b");
    assert (metas "a-b");
    assert (not (metas "ab"));
    assert (not (metas "a?b"));
  end


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

    print_string "testing DFA-minimzied acceptance..."; flush stdout;
    test (string_matcher
            ~compile:dfa_minimize_compile
            ~accept:dfa_accept);
    print_endline "OK!";
  end
