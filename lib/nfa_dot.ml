(* Representation of DOT graphs, and conversion from NFAs to DOT

   The DOT specification is here:
   https://graphviz.gitlab.io/_pages/doc/info/lang.html

   The code here supports a slightly restricted subset without
   subgraphs, multi-node edges, or "ports". *)

module Digraph :
sig
  type t

  module Node :
  sig
    type t
    val make : id:string -> t
    val with_attrs : t -> (string * string) list -> t
  end

  val empty : t
  val with_name : t -> string -> t
  val with_node : t -> Node.t -> t
  val with_edge : t -> ?attrs:(string * string) list -> (Node.t * Node.t) -> t
  val with_attrs : t -> (string * string) list -> t
  val format : Format.formatter -> t -> unit
end =
struct
  type id = string
  type attr = id * id

  let format_attrs formatter = function
    | [] -> ()
    | attrs ->
       Format.fprintf formatter "[@ @[";
       List.iter (fun (k, v) -> Format.fprintf formatter "%S@ =@ %S;" k v) attrs;
       Format.fprintf formatter "]@]"

  module Node =
  struct
    type t = id * attr list

    let make ~id = (id, [])
    let with_attrs (id, attrs) attrs' = (id, attrs @ attrs')
    let format formatter (id, attrs) =
      Format.fprintf formatter "%a@ %S" format_attrs attrs id
    let id (id, _) = id
  end
  type stmt = Node of Node.t
            | Edge of Node.t * Node.t * attr list
            | Attr of id * id
  type t = id option * stmt list

  let empty = (None, [])
  let with_attrs (id, stmts) attrs =
    (id, stmts @ List.map (fun (k,v) -> Attr (k,v)) attrs)
  let with_node (id, stmts) node = (id, stmts @ [Node node])
  let with_edge (id, stmts) ?attrs (n1, n2) =
    match attrs with None -> (id, stmts @ [Edge (n1, n2, [])])
                   | Some attrs -> (id, stmts @ [Edge (n1, n2, attrs)])
  let with_name (_, s) n = (Some n, s)

  let format_stmt formatter = function
      | Node node -> Format.fprintf formatter "node@ @[%a@]" Node.format node
      | Edge (n1, n2, attrs) -> Format.fprintf formatter "@[@[%S@ ->@ %S@]@ %a@]"
                              (Node.id n1)
                              (Node.id n2)
                              format_attrs attrs
      | Attr (k, v) -> Format.fprintf formatter "@[%S@ =@ %S@];" k v

  let format formatter (id, stmts) =
    let pr fmt = Format.fprintf formatter fmt in
    begin match id with
      None -> pr "@[digraph {@\n"
    | Some id -> pr "@]digraph %S{@[" id
    end;
    List.iter (pr "@ @ @[%a@]@\n" format_stmt) stmts;
    pr "}@]"
end

type digraph = Digraph.t
let format_digraph = Digraph.format

module CharSet = Set.Make(Char)

let edge_name s =
  match CharSet.cardinal s with
  | 0 -> "{}"
  | 1 -> String.make 1 (CharSet.choose s)
  | 256 -> "."
  | _ -> "{" ^ String.concat " " (List.map (String.make 1) (CharSet.elements s)) ^ "}"


let digraph_of_nfa : Nfa.nfa -> Digraph.t =
  fun nfa ->
  let states = Hashtbl.create 10 in
  let edges = Hashtbl.create 10 in
  let make_node =
    let counter = ref 0 in
    fun n ->
    let name = string_of_int !counter in
    incr counter;
    let node = Digraph.Node.make ~id:name in
    let shape = if Nfa.StateSet.mem n nfa.Nfa.finals then "doublecircle"
                else "circle" in
    Digraph.Node.with_attrs node ["shape", shape]
  in
  let add_edge source c target =
    Hashtbl.replace edges (source, target) @@
      match Hashtbl.find edges (source, target) with
      | exception Not_found -> CharSet.singleton c
      | set -> CharSet.add c set
  in
  let rec step state =
    (* Accumulate nodes and edges, using the states/edges tables as
       'seen lists' to ensure each node and edge is only visited once *)
    if not (Hashtbl.mem states state) then begin
        Hashtbl.add states state (make_node state);
        Nfa.CharMap.iter
          (fun c targets ->
            Nfa.StateSet.iter
              (fun target ->
                add_edge state c target;
                step target)
              targets)
          (nfa.Nfa.next state);
      end
  in
  Nfa.StateSet.iter step nfa.start;
  (** Empty node to the left of the start state *)
  let input =
    Digraph.Node.with_attrs (Digraph.Node.make ~id:"")
      ["shape", "none"; "width", "0"] in
  (** Initial empty digraph *)
  let dg = Digraph.with_node
             (Digraph.with_attrs Digraph.empty ["rankdir", "LR"])
             input in
  (** Add the state nodes *)
  let dg =
    Hashtbl.fold
      (fun _ node dg -> Digraph.with_node dg node)
      states
      dg
  in
  (** Add the initial edges *)
  let dg =
    Nfa.StateSet.fold
      (fun s dg ->
        Digraph.with_edge dg (input, Hashtbl.find states s))
      nfa.start 
      dg
  in
  (** Add the other edges *)
  Hashtbl.fold
    (fun (source, target) s dg ->
      Digraph.with_edge dg ~attrs:["label", edge_name s]
        (Hashtbl.find states source,
         Hashtbl.find states target))
    edges
    dg
