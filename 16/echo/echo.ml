open Core
open Async

let run ~uppercase ~port =
  let host_and_port =
    Tcp.Server.create ~on_handler_error: `Raise
    (Tcp.Where_to_listen.of_port port)
    (fun _ r w ->
      Pipe.transfer (Reader.pipe r) (Writer.pipe w)
      ~f:(if uppercase then String.uppercase else Fn.id))
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let () =
  Command.async
    ~summary:"Start an echo server"
    Command.Let_syntax.(
      let%map_open uppercase =
        flag "-uppercase" no_arg
        ~doc:"Convert to uppercase before echoing back"
      and port =
        flag "-port" (optional_with_default 9876 int)
        ~doc:"Port to listen on (default 9876)"
      in
      fun () -> run ~uppercase ~port)
    |> Command.run

(* let rec copy_blocks buf r w =
  Reader.read r buf
  >>= function
  | `Eof -> return ()
  | `Ok bytes_read ->
    Writer.write w (Bytes.to_string buf) ~len:bytes_read;
    Writer.flushed w
    >>= fun () -> copy_blocks buf r w

let run () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error: `Raise
      (Tcp.Where_to_listen.of_port 9876)
      (fun _ r w ->
        let buf = Bytes.create (16 * 1024) in
        copy_blocks buf r w)
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)

let () =
  run ();
  never_returns (Scheduler.go ()) *)
