open Core

type t = {
  foo : int;
  bar : float;
}

let sexp_of_t t =
  let a x = Sexp.Atom x
  and l x = Sexp.List x in
  l [
    l [ a "foo"; Int.sexp_of_t t.foo];
    l [ a "bar"; Float.sexp_of_t t.bar];
  ]
