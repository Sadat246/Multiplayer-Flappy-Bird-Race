(** The game server. For this stage it is only a static file server for the
    client bundle — but it already listens through [Rpc_websocket.Rpc.serve],
    so when Stage 4 adds the race RPCs they plug into [implementations] and
    nothing else moves (same skeleton as the jsip-exchange dashboard server).

    The referee state (latest positions, event log, race state machine)
    arrives in Stage 4; per the context doc §4 anti-goal, no physics or
    collision will ever live here. *)

open! Core
open! Async

let content_type file =
  match snd (Filename.split_extension file) with
  | Some "html" -> "text/html; charset=utf-8"
  | Some "js" -> "text/javascript"
  | _ -> "application/octet-stream"
;;

(* Serve exactly two files from [static_dir], re-read per request so a fresh
   [dune build] shows up on browser refresh. The explicit whitelist keeps
   this from becoming a path-traversal hole. *)
let static_handler
  ~static_dir
  ~body:(_ : Cohttp_async.Body.t)
  (_ : Socket.Address.Inet.t)
  request
  =
  let path = Uri.path (Cohttp_async.Request.uri request) in
  let file =
    match path with
    | "" | "/" | "/index.html" -> Some "index.html"
    | "/main.bc.js" -> Some "main.bc.js"
    | _ -> None
  in
  match file with
  | None ->
    Cohttp_async.Server.respond_string ~status:`Not_found "not found\n"
  | Some file ->
    (match%bind
       Monitor.try_with (fun () -> Reader.file_contents (static_dir ^/ file))
     with
     | Ok contents ->
       let headers =
         Cohttp.Header.init_with "content-type" (content_type file)
       in
       Cohttp_async.Server.respond_string ~headers contents
     | Error _ ->
       Cohttp_async.Server.respond_string
         ~status:`Not_found
         [%string "missing %{file} under %{static_dir}\n"])
;;

let main ~port ~static_dir () =
  let implementations =
    (* No RPCs yet — Stage 4 adds join and sync here. *)
    Rpc.Implementations.create_exn
      ~implementations:[]
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let%bind (_ : (_, _) Cohttp_async.Server.t) =
    Rpc_websocket.Rpc.serve
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ~implementations
      ~initial_connection_state:
        (fun
          ()
          (_ : Rpc_websocket.Rpc.Connection_initiated_from.t)
          (_ : Socket.Address.Inet.t)
          (_ : Rpc.Connection.t)
        -> ())
      ~http_handler:(fun () -> static_handler ~static_dir)
      ()
  in
  printf
    "flappy-racer on http://localhost:%d  (static %s)\n%!"
    port
    static_dir;
  Deferred.never ()
;;

let command =
  Command.async
    ~summary:"Serve the Flappy Racer client (game RPCs arrive in Stage 4)."
    (let%map_open.Command port =
       flag
         "-port"
         (optional_with_default 8080 int)
         ~doc:"PORT port to serve on (default 8080)"
     and static_dir =
       flag
         "-static-dir"
         (optional_with_default "_build/default/app/client/site" string)
         ~doc:
           "DIR directory holding index.html and main.bc.js (default: the \
            dune-assembled site dir, valid when run from the project root)"
     in
     fun () -> main ~port ~static_dir ())
    ~behave_nicely_in_pipeline:false
;;

let () = Command_unix.run command
