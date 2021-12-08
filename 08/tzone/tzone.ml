open Core
open Stdio

let () =
  Out_channel.output_string stdout "Pick a timezone: ";
  Out_channel.flush stdout;
  match In_channel.(input_line stdin) with
  | None -> failwith "No timezone provided"
  | Some zone_str ->
    let zone = Time.Zone.find_exn zone_str in
    let time_str = Time.to_string_abs (Time.now()) ~zone in
    printf "The time in %s is %s\n%!" (Time.Zone.to_string zone) time_str
