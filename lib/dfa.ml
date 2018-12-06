(** Deterministic finite automata *)

type state = Nfa.state
module StateSet = Nfa.StateSet
module StateMap = Map.Make(Int32)
module CharMap = Nfa.CharMap

type dfa = {
  start : state;
  (** the start state *)

  finals: StateSet.t;
  (** the final (or "accept") states *)

  next: state -> state CharMap.t;
  (** the transition function, that maps a state and a character to the
      next state *)
}

(** Add src--c-->dst to the transition set, replacing any existing src--c-->dst' *)
let add_transition (src, c, dst) trans =
  match StateMap.find src trans with
  | exception Not_found -> StateMap.add src (CharMap.singleton c dst) trans
  | cm ->                  StateMap.add src (CharMap.add c dst cm)    trans

(** Available transitions from a set of states *)
let transitions states nfa =
  StateSet.fold (fun s m ->
      let m' = nfa.Nfa.next s in
      CharMap.union (fun _ s s' -> Some (StateSet.union s s')) m m')
    states
    CharMap.empty

(** Conversion to DFA via the powerset construction *)
let determinize : Nfa.nfa -> dfa =
  let module M = Map.Make(StateSet) in
  fun nfa ->
  let fresh = let r = ref 0l in fun () -> (r := Int32.succ !r; !r) in
  let rec build states (map, ts, finals) =
    match M.find states map with
    | state -> (state, map, ts, finals)
    | exception Not_found ->
       let state = fresh () in
       let finals = if not (StateSet.is_empty (StateSet.inter states nfa.Nfa.finals))
                    then StateSet.add state finals
                    else finals in
       let map = M.add states state map in
       let tsn = transitions states nfa in
       let map, ts, finals =
         CharMap.fold
           (fun c ss (map, ts, finals) ->
             let dst, map, ts, finals = build ss (map, ts, finals) in
             let ts = add_transition (state, c, dst) ts in
             (map, ts, finals))
           tsn
           (map, ts, finals)
       in 
       state, map, ts, finals
  in
  let start, _, trans, finals =
    build nfa.Nfa.start (M.empty, StateMap.empty, StateSet.empty)
  in
  let next s = try StateMap.find s trans with Not_found -> CharMap.empty in
  { start; finals; next }

let inject { start; finals; next } =
  { Nfa.start = Nfa.StateSet.singleton start;
    finals;
    next = fun s -> CharMap.map StateSet.singleton (next s) }

(** A simple DFA interpreter. *)
let accept dfa inp =
  let rec step cur = function
    | [] -> StateSet.mem cur dfa.finals
    | c :: cs ->
       match CharMap.find_opt c (dfa.next cur) with
       | None -> false
       | Some s -> step s cs in
  step dfa.start inp
