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
  let last_seen = ref (-1) in
  let start_requested = ref false in
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
          { Protocol.Update.player; pos; last_seen_event = !last_seen }
      with
      | Error err | Ok (Error err) ->
        printf !"sync failed: %{Error#hum}\n%!" err;
        Rpc.Connection.close conn
      | Ok (Ok view) ->
        List.iter view.events ~f:(fun (e : Protocol.Stamped_event.t) ->
          last_seen := Int.max !last_seen e.seq;
          if verbose
          then printf !"event: %{sexp: Protocol.Event.t}\n%!" e.event);
        (match view.race with
         | Waiting_for_players -> start_requested := false
         | Ready_to_start ->
           (* A human would press the start button; the bot just does. *)
           if not !start_requested
           then (
             start_requested := true;
             printf "ready - pressing start\n%!";
             don't_wait_for
               (Rpc.Rpc.dispatch Protocol.new_race_rpc conn ()
                >>| (ignore : (unit Or_error.t, Error.t) Result.t -> unit)))
         | Race { seed } ->
           if not (List.mem !races_seen seed ~equal:Int.equal)
           then (
             races_seen := seed :: !races_seen;
             printf "race started, seed %d\n%!" seed;
             (* Exercise the power-up protocol: claim the course's first item
                box, then immediately use whatever we won. *)
             don't_wait_for
               (let course = Flappy_game.Course.generate ~seed in
                match List.hd course.item_boxes with
                | None -> return ()
                | Some box ->
                  (match%bind
                     Rpc.Rpc.dispatch
                       Protocol.pickup_request_rpc
                       conn
                       (player, box.id)
                   with
                   | Error err | Ok (Error err) ->
                     printf !"pickup failed: %{Error#hum}\n%!" err;
                     return ()
                   | Ok (Ok None) ->
                     printf "pickup: opponent beat me to box %d\n%!" box.id;
                     return ()
                   | Ok (Ok (Some item)) ->
                     printf
                       !"pickup: won %{sexp: Flappy_game.Item.t} from box %d\n\
                         %!"
                       item
                       box.id;
                     (match item with
                      | Boost | Shield -> return () (* local-only items *)
                      | Volley ->
                        Rpc.Rpc.dispatch
                          Protocol.use_powerup_rpc
                          conn
                          (player, Fire_volley { x = box.x; y = box.y })
                        >>| (ignore
                             : (unit Or_error.t, Error.t) Result.t -> unit)
                      | Swap ->
                        Rpc.Rpc.dispatch
                          Protocol.use_powerup_rpc
                          conn
                          (player, Swap)
                        >>| (ignore
                             : (unit Or_error.t, Error.t) Result.t -> unit))))));
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
