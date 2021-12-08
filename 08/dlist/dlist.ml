open Base
open Stdio

type 'a element =
{
  value: 'a;
  mutable next: 'a element option;
  mutable prev: 'a element option;
}

type 'a t = 'a element option ref

let create () = ref None

let is_empty t = Option.is_none !t

let first t = !t

let next el = el.next

let prev el = el.prev

let value el = el.value

let insert_first t value =
  let new_el = { prev = None; next = !t; value } in
  begin match !t with
  | Some old_first -> old_first.prev <- Some new_el
  | None -> ()
  end;
  t := Some new_el;
  new_el

let insert_after el value =
  let new_el = { prev = Some el; next = el.next; value} in
  (
    match el.next with
    | Some old_next -> old_next.prev <- Some new_el
    | None -> ()
  );
  el.next <- Some new_el;
  new_el

let remove t el =
  let { prev; next; _ } = el in
  (
    match prev with
    | Some prev -> prev.next <- next
    | None -> t := next
  );
  (
    match next with
    | Some next -> next.prev <- prev
    | None -> ()
  );
  el.prev <- None;
  el.next <- None

let iter t ~f =
  let rec sub = function
  | None -> ()
  | Some el -> f (value el); sub (next el)
  in
  sub !t

let find_el t ~f =
  let rec sub = function
  | None -> None
  | Some el as x -> if f (value el) then x else sub (next el)
  in
  sub !t

let () =
  let hd = create () in
  let el1 = insert_first hd 5 in
  let el2 = insert_first hd 3 in
  let el3 = insert_after el1 4 in
  let _ = insert_after el2 2 in
  let _ = insert_after el3 1 in
  iter hd ~f:(fun x -> printf "%d\n" x) (* 3 2 5 4 1 *)
