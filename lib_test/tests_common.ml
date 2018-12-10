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
let dfa_minimize_compile r = Dfa.minimize (dfa_compile r)
