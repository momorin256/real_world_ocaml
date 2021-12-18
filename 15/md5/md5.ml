open Core

let checksum_from_string hash_length buf =
  Md5.digest_string buf
  |> Md5.to_hex
  |> (fun s -> String.prefix s hash_length)
  |> print_endline

let checksum_from_file hash_length filename =
  let get_contents = function
    | "-" -> In_channel.(input_all stdin)
    | filename -> In_channel.read_all filename
  in
  get_contents filename
  |> Md5.digest_string
  |> Md5.to_hex
  |> (fun s -> String.prefix s hash_length)
  |> print_endline

let regular_file =
  Command.Arg_type.create (fun filename ->
    match Sys.is_file filename with
    | `Yes -> filename
    | `No -> failwith "Not a regular file"
    | `Unknown -> failwith "Could not determine if this was a regular file")

let command =
  Command.basic
    ~summary:"Generato and MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
      let%map_open
        use_string = flag "-s" (optional string) ~doc:"string Checksum the given string"
        and trial = flag "-t" no_arg ~doc:" run a built-in time trial"
        and hash_length = anon ("hash_length" %: int)
        and filename = anon (maybe_with_default "-" ("filename" %: regular_file))
      in
      fun () ->
        if trial then printf "Running time trial\n"
        else match use_string with
        | Some buf -> checksum_from_string hash_length buf
        | None -> checksum_from_file hash_length filename)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
