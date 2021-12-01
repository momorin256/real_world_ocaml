open Base
open Stdio

let build_counts () =
  In_channel.fold_lines In_channel.stdin ~init:Counter.empty ~f:Counter.touch

let print_median m =
  let module C = Counter in
  match m with
  | C.Median str -> printf "True median: %s\n" str
  | C.Before_and_after (bef, aft) -> printf "Before and after madian: %s, %s\n" bef aft

let () =
  build_counts ()
  |> Counter.to_list
  |> List.sort ~compare:(fun (_, x) (_, y) -> Int.descending x y)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line, count) -> printf "%3d: %s\n" count line)
