open Base

module type Counter_sig = sig
  type t

  val empty : t

  val touch : t -> string -> t

  val to_list : t -> (string * int) list
end

module Counter : Counter_sig = struct
  type t = (string, int, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let to_list t = Map.to_alist t

  let touch t s =
    let count =
      match Map.find t s with
      | None -> 0
      | Some x -> x
    in
    Map.set t ~key:s ~data:(count + 1)
end

let digit_alist =
  [ 0, "zero"; 1, "one"; 2, "two"  ; 3, "three"; 4, "four"
  ; 5, "five"; 6, "six"; 7, "seven"; 8, "eight"; 9, "nine" ]

module Book = struct
  module T = struct
    type t = { title : string; isbn : string }

    let compare t1 t2 =
      let cmp_title = String.compare t1.title t2.title in
      if cmp_title <> 0 then cmp_title
      else String.compare t1.isbn t2.isbn

    let sexp_of_t t : Sexp.t =
      List [ Atom t.title; Atom t.isbn ]
  end

  include T
  include Comparator.Make(T)
end

let some_programming_books =
  Set.of_list (module Book)
    [ { title = "Real World OCaml"; isbn = "978-1449323912" }
      ; { title = "Structure and Interpretation of Computer Programs"; isbn = "978-0262510875" }
      ; { title = "The C Programming Language"; isbn = "978-0131101630" } ]
