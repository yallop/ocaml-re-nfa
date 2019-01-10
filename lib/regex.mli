(** Regular expression combinators *)

type t
(** The type of regular expressions *)

type charset = Set.Make(Char).t
(** Sets of characters *)

val empty : t
(** [empty] rejects every string *)

val eps : t
(** [eps] accepts only the empty string *)

val any : t
(** [any] accepts any single character *)

val oneof : charset -> t
(** [oneof cs] accepts any character in cs *)

val range : char -> char -> t
(** [range l h] accepts any character in the range [l]..[h] *)

val chr : char -> t
(** [chr c] accepts exactly the character [c] *)

val seq : t -> t -> t
(** [seq x y] accepts strings [rs] where [x] accepts [r] and [y]
    accepts [s] *)

val alt : t -> t -> t
(** [alt x y] accepts any string accepted by either [x] or [y] *)

val opt : t -> t
(** [opt r] accepts any string accepted by [r], and the empty string *)

val star : t -> t
(** [star r] accepts any string consisting of zero or more copies of
    a string accepted by [r] *)

val plus : t -> t
(** [star r] accepts any string consisting of one or more copies of
    a string accepted by [r] *)

val parse : string -> t
(** Parse a regular expression using the following grammar:

      r ::= (r)          (parenthesized regex)
            .            (match any character)
            rr           (sequencing)
            r|r          (alternation)
            r?           (zero or one)
            r*           (zero or more)
            r+           (one or more)
            c            (literal character)
            [b]          (POSIX bracket expression)

   Raises [Parse_error] on parse error
*)

val unparse_charset : charset -> string
(** [unparse_charset cs] is a string denoting a regular expression
    that accepts any character in [cs], and nothing else *)

val compile : t -> Nfa.nfa
(** [compile r] translates [r] to an NFA that succeeds on exactly
    those strings matched by [r] *)

exception Parse_error of string
(** Raised when [parse] is given an invalid regex *) 
