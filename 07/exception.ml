open Base

exception Key_not_found of string

let rec find_exn list key =
  match list with
  | [] -> raise (Key_not_found key)
  | (key', data) :: tl -> if String.(=) key' key then data else find_exn tl key

let find list key =
  Option.try_with (fun () -> find_exn list key)

let find list key =
  Or_error.try_with (fun () -> find_exn list key)
