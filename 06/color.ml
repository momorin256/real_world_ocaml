open Base
open Stdio

type color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let color_to_int = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7

let color_by_number num text =
  Printf.sprintf "\027[38;5;%dm%s\027[0m" num text

let () =
  printf "color: %s\n" (color_by_number (color_to_int Blue) "Blue")

type 'a expr =
  | Base of 'a
  | Const of bool
  | And of 'a expr list
  | Or of 'a expr list
  | Not of 'a expr

type mail_field =
  | To | From | CC | Date | Subject

type mail_predicate =
{
  field: mail_field;
  contains: string;
}

let to_predicate field contains = Base { field; contains }

let sample = 
  And
  [
    Or
    [
      to_predicate To "alice";
      to_predicate CC "bob";
    ];
  ]

let rec eval expr base_eval =
  let eval' expr = eval expr base_eval in
  match expr with
  | Base base -> base_eval base
  | Const b -> b
  | And exprs -> List.for_all exprs ~f:eval'
  | Or exprs -> List.exists exprs ~f:eval'
  | Not expr -> not (eval' expr)

let to_alice { field; contains } =
  match field with
  | To -> String.(=) contains "alice"
  | _ -> false

(*
  1. if l contains "Const false" then false.
  2. filter "Const true"
*)
let and_ l =
  if List.exists l ~f:(function | Const false -> true | _ -> false)
    then Const false
  else
    match List.filter l ~f:(function | Const true -> false | _ -> true) with
    | [] -> Const true
    | [x] -> x
    | l -> And l

(*
  1. if l contains "Const true" then true.
  2. filter "Const false"
*)
let or_ l =
  if List.exists l ~f:(function | Const true -> true | _ -> false)
    then Const true
  else
    match List.filter l ~f:(function | Const false -> false | _ -> true) with
    | [] -> Const false
    | [x] -> x
    | l -> Or l

(* 
  1. not "Const true" is false, not "Const false" is true
  2. not not expr is expr
*)
let not_ = function
  | Const b -> Const (not b)
  | Not e -> e
  | (Base _ | And _ | Or _) as e -> Not e

let rec simplify = function
  | (Base _ | Const _) as x -> x
  | And exprs -> and_ (List.map ~f:simplify exprs)
  | Or exprs -> or_ (List.map ~f:simplify exprs)
  | Not expr -> not_ (simplify expr)

let simple1 = simplify (
  Not (
    And [
      Or [
        Base "text1";
        Const true;
      ];
      Base "text2";
    ]
  )
)

let simple2 = simplify (
  Not (
    And [
      Or [
        Base "text";
        Const true;
      ];
      Not (Not (Not (Base "text2")))
    ]
  )
)
