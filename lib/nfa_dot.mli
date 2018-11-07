type digraph

val format_digraph : Format.formatter -> digraph -> unit

val digraph_of_nfa : Nfa.nfa -> digraph
