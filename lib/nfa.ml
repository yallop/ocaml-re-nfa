type state = int32
module StateSet = Set.Make(Int32)
module CharMap = Map.Make(Char)
type transitions = StateSet.t CharMap.t

type nfa = {
  start : state;
  (** the start state *)

  finals: StateSet.t;
  (** the final (or "accept") states *)

  next: state -> transitions;
  (** the transition function, that maps a state and a character to a
      set of states *)
}

let find_states sym nfa m =
  try CharMap.find sym (nfa.next m)
  with Not_found -> StateSet.empty

let flat_map f ss = StateSet.fold (fun s -> StateSet.union (f s)) ss StateSet.empty
let nextss curs sym nfa = flat_map (find_states sym nfa) curs

(** A simple NFA interpreter. *)
let accept nfa inp =
  (** cur is the set of all the current states -- i.e. those states at
      which we have arrived by examining the input up to this point.
      Since the automaton is non-deterministic, encountering a character
      in a given state can cause transitions to multiple different
      states *)
  let rec step cur = function
    | [] -> StateSet.(not (is_empty (inter cur nfa.finals)))
    | c :: cs -> step (nextss cur c nfa) cs
  in step (StateSet.singleton nfa.start) inp
