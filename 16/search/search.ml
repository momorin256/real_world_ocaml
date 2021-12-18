open Core
open Async

let query_uri query =
  let base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json" in
  Uri.add_query_param base_uri ("q", [query])

let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list -> (
    let find key =
      match List.Assoc.find ~equal:String.equal kv_list key with
      | None | Some (`String "") -> None
      | Some s -> Some (Yojson.Safe.to_string s)
    in
    match find "Abstract" with
    | None -> find "Definition"
    | Some _ as x -> x)
  | _ -> None

let get_definition word =
  Cohttp_async.Client.get (query_uri word)
  >>= fun (_, body) -> Cohttp_async.Body.to_string body
  >>| fun str -> (word, get_definition_from_json str)

let print_result (word, definition) =
  printf "%s\n%s\n\n%s\n\n"
    word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
    | None -> "No definition found"
    | Some def -> String.concat ~sep:"\n" (Wrapper.wrap (Wrapper.make 70) def))

let search_and_print words =
  Deferred.all (List.map words ~f:get_definition)
  >>| fun results ->
  List.iter results ~f:print_result

let () =
  Command.async ~summary:"Retrieve definitions from duckduckgo search engine"
    Command.Let_syntax.(
      let%map_open words = anon (sequence ("word" %: string)) in
      fun () -> search_and_print words)
    |> Command.run
