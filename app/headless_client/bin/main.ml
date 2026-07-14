(** A headless test client: joins the server like a browser would, streams a
    scripted position at {!Flappy_game.Config.sync_hz}, and logs what it
    receives. Two uses:

    - end-to-end smoke testing of the protocol/server without a browser;
    - a practice opponent when testing multiplayer with only one human — run
      it against your server and race its ghost.

    The scripted "bot" just cruises forward at a constant speed with a gentle
    sine-wave altitude. It never dies and ignores the course — it exercises
    the network, not the game. *)

open! Core
open! Async
module Protocol = Flappy_protocol
module Config = Flappy_game.Config

let run ~host ~port ~duration ~verbose () =
  let uri = Uri.make ~scheme:"ws" ~host ~port ~path:"/" () in
  let%bind conn = Rpc_websocket.Rpc.client uri >>| Or_error.ok_exn in
  let%bind player =
    Rpc.Rpc.dispatch Protocol.join_rpc conn "headless-bot"
    >>| Or_error.ok_exn
    >>| Or_error.ok_exn
  in
  printf !"joined as %{sexp: Protocol.Player_id.t}\n%!" player;
  let ticks = ref 0 in
  let races_seen = ref [] in
  let tick_span = Time_ns.Span.of_sec (1. /. Config.sync_hz) in
  let deadline = Time_ns.add (Time_ns.now ()) duration in
  let rec loop () =
    if Time_ns.( >= ) (Time_ns.now ()) deadline
    then (
      printf
        !"done: %d syncs, races seen %{sexp: int list}\n%!"
        !ticks
        (List.rev !races_seen);
      Rpc.Connection.close conn)
    else (
      incr ticks;
      let t = Float.of_int !ticks /. Config.sync_hz in
      let pos =
        (* Cruise forward, bob vertically around mid-height. *)
        { Protocol.Pos.x = Config.bird_start_x +. (Config.cruise_speed *. t)
        ; y = 240. +. (120. *. Float.sin (t /. 1.5))
        }
      in
      match%bind
        Rpc.Rpc.dispatch
          Protocol.sync_rpc
          conn
          { Protocol.Update.player; pos }
      with
      | Error err | Ok (Error err) ->
        printf !"sync failed: %{Error#hum}\n%!" err;
        Rpc.Connection.close conn
      | Ok (Ok view) ->
        (match view.race with
         | Waiting_for_players -> ()
         | Race { seed } ->
           if not (List.mem !races_seen seed ~equal:Int.equal)
           then (
             races_seen := seed :: !races_seen;
             printf "race started, seed %d\n%!" seed));
        if verbose
        then
          printf
            !"tick %d: race %{sexp: Protocol.Race_state.t}, opponent \
              %{sexp: Protocol.Pos.t option}\n\
              %!"
            !ticks
            view.race
            view.opponent;
        let%bind () = Clock_ns.after tick_span in
        loop ())
  in
  loop ()
;;

let command =
  Command.async
    ~summary:
      "Headless test client: join, stream scripted positions, log views. \
       Doubles as a practice opponent."
    (let%map_open.Command host =
       flag
         "-host"
         (optional_with_default "localhost" string)
         ~doc:"HOST server host (default localhost)"
     and port =
       flag
         "-port"
         (optional_with_default 8080 int)
         ~doc:"PORT server port (default 8080)"
     and duration =
       flag
         "-for"
         (optional_with_default
            (Time_ns.Span.of_sec 10.)
            (Arg_type.create Time_ns.Span.of_string))
         ~doc:"SPAN how long to stay connected (default 10s)"
     and verbose = flag "-verbose" no_arg ~doc:" log every sync response" in
     fun () -> run ~host ~port ~duration ~verbose ())
    ~behave_nicely_in_pipeline:false
;;

let () = Command_unix.run command
