type _ value =
  | Int : int -> int value
  | Bool : bool -> bool value

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

let eval_value : type a. a value -> a = function
  | Int x -> x
  | Bool x -> x

let rec eval : type a. a expr -> a = function
  | Value v -> eval_value v
  | Eq (e1, e2) -> (eval e1) = (eval e2)
  | Plus (e1, e2) -> (eval e1) + (eval e2)
  | If (cond, th, el) -> if eval cond then eval th else eval el

(* let eval_value (type a) (v : a value) : a =
  match v with
  | Int x -> x
  | Bool x -> xn

let rec eval : 'a. 'a expr -> 'a =
  fun (type a) (x : a expr) ->
    match x with
    | Value v -> eval_value v
    | Eq (e1, e2) -> (eval e1) = (eval e2)
    | Plus (e1, e2) -> (eval e1) + (eval e2)
    | If (cond, th, el) -> if eval cond then eval th else eval el *)

open Stdio

let () =
  let i1 = Value (Int 3) in
  let i2 = Value (Int 5) in
  let b1 = Value (Bool true) in
  let b2 = Eq (i1, i2) in
  let expr_plus = Plus (i1, i2) in
  let expr_if1 = If (b1, expr_plus, i2) in (* if true then 3 + 5 else 5 *)
  let expr_if2 = If (b2, i1, i2) in (* if 3 = 5 then 3 else 5 *)
  let tests = [(eval expr_if1 = 8); (eval expr_if2 = 5)] in
  List.iter (fun b -> printf "test: %s\n" (if b then "OK" else "NG")) tests
