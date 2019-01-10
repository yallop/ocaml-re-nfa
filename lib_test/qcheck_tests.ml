open Tests_common

module C = Set.Make(Char)

let complement set =
  let rec loop acc i =
    if i > 255 then acc else
      let c = Char.chr i in
      if C.mem c set then loop acc (succ i)
      else loop (C.add c acc) (succ i)
  in loop C.empty 0

let check_all set f = C.for_all f set && not (C.exists f (complement set))

let showset set = C.fold (Printf.sprintf "%c%s") set ""

let all_chars = C.elements (complement C.empty)
let take i l =
  let rec loop acc i l =
    match i, l with
    | 0, _ |  _, [] -> List.rev acc
    | i, h :: t -> loop (h :: acc) (pred i) t
  in loop [] i l

let charset_gen =
  let open QCheck.Gen in
  int_bound 255 >>= fun cardinal ->
  map (take cardinal) (shuffle_l all_chars) >>= fun l ->
  return (C.of_list l)

let charset = QCheck.make charset_gen

let check_essential_property set : bool =
  let unparsed = (Regex.unparse_charset set) in
  check_all set @@ fun c ->
  nfa_accept (Regex.compile (Regex.parse unparsed)) (String.make 1 c)

let roundtrip_test =
  QCheck.Test.make ~count:10000 ~name:"parse (unparse s) = oneof s" charset check_essential_property

let roundtrip_test0 () =
  assert (check_essential_property C.empty)

let roundtrip_test1 () =
  for i = 0 to 255 do
    assert (check_essential_property (C.singleton (Char.chr i)))
  done

let roundtrip_test2 () =
  for i = 0 to 255 do
    for j = 0 to 255 do
      assert (check_essential_property (C.of_list [Char.chr i;
                                                   Char.chr j]))
    done
  done

let () =
  begin
    print_string "character range size 0 tests..."; flush stdout;
    roundtrip_test0 ();
    print_endline "OK!";

    print_string "character range size 1 tests..."; flush stdout;
    roundtrip_test1 ();
    print_endline "OK!";

    print_string "character range size 2 tests..."; flush stdout;
    roundtrip_test2 ();
    print_endline "OK!";

    print_string "character range qcheck tests..."; flush stdout;
    QCheck.Test.check_exn roundtrip_test;
    print_endline "OK!";
  end
