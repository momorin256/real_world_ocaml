open Base
open Stdio

module type Query_handler = sig
  type config

  val sexp_of_config : config -> Sexp.t

  val config_of_sexp : Sexp.t -> config

  type t

  val name : string

  val create : config -> t

  val eval : t -> Sexp.t -> Sexp.t Or_error.t
end

module Unique = struct
  type config = int [@@deriving sexp]

  type t = { mutable next_id : int }

  let name = "unique"

  let create start_at = { next_id = start_at }

  let eval t sexp =
    match Or_error.try_with (fun () -> unit_of_sexp sexp) with
    | Error _ as err -> err
    | Ok () ->
      let res = Ok (Int.sexp_of_t t.next_id) in
      t.next_id <- t.next_id + 1;
      res
end

module List_dir = struct
  type config = string [@@deriving sexp]

  type t = { cwd : string }

  let name = "ls"

  let create cwd = { cwd }

  let eval t sexp =
    match Or_error.try_with (fun () -> string_of_sexp sexp) with
    | Error _ as err -> err
    | Ok dir ->
      let is_abs = (String.length dir > 0) && (Char.(=) dir.[0] '/') in
      let dir = if is_abs then dir else Core.Filename.concat t.cwd dir in
      Ok (Array.sexp_of_t String.sexp_of_t (Core.Sys.readdir dir))
end

module type Query_handler_instance = sig
  module Query_handler : Query_handler
  val this : Query_handler.t
end

let build_instance (type a) (module Q : Query_handler with type config = a) config =
  (module struct
    module Query_handler = Q
    let this = Q.create config
  end : Query_handler_instance)

let build_dispatch_table handlers =
  let table = Hashtbl.create (module String) in
  List.iter handlers
    ~f:(fun ((module I : Query_handler_instance) as instance) ->
      Hashtbl.set table ~key:I.Query_handler.name ~data:instance);
  table

let dispatch dispatch_table name_and_query =
  match name_and_query with
  | Sexp.List [Sexp.Atom name; query] ->
    (match Hashtbl.find dispatch_table name with
    | None -> Or_error.error "Could not find matching handler" name String.sexp_of_t
    | Some (module I : Query_handler_instance) -> I.Query_handler.eval I.this query)
  | e -> printf "%s, %s\n" (Sexp.to_string_hum e) (Sexp.to_string_hum name_and_query); Or_error.error_string "malformed query"

let rec cli dispatch_table =
  printf ">>> %!";
  let result =
    match In_channel.(input_line stdin) with
    | None -> `Stop
    | Some line ->
      (match Or_error.try_with (fun () -> Core.Sexp.of_string line) with
      | Error e -> `Continue (Error.to_string_hum e)
      | Ok (Sexp.Atom "quit") -> `Stop
      | Ok name_and_query -> (match dispatch dispatch_table name_and_query with
        | Error e -> `Continue (Error.to_string_hum e)
        | Ok s -> `Continue (Sexp.to_string_hum s)))
  in
  match result with
  | `Stop -> ()
  | `Continue msg -> printf "%s\n%!" msg; cli dispatch_table

let () =
  let unique_instance = build_instance (module Unique) 0 in
  let list_dir_instance = build_instance (module List_dir) "/var" in
  cli (build_dispatch_table [unique_instance; list_dir_instance])
