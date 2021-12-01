open Base

(* A collection of string frequency counts. *)
type t

(* The empty set of frequency counts. *)
val empty : t

(* Bump the frequency count for the given string. *)
val touch : t -> string -> t

(* Converts the set of frequency counts to an association list. *)
val to_list : t -> (string * int) list

(* Represents the median computed from a set of strings. *)
type median =
  | Median of string
  | Before_and_after of string * string

val median : t -> median
