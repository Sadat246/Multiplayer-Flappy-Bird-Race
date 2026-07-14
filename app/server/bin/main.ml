(** The game server: static files for the client bundle, plus the referee.

    Referee means exactly and only (context doc §4): who holds which race
    slot, the current race's seed, and a latest-position slot per player
    relayed to the other on every {!Flappy_protocol.sync_rpc}. All physics,
    collision and death detection live in the clients; no verification
    happens here, ever (the trusted-2-player anti-goal).

    Same serving skeleton as the jsip-exchange dashboard server:
    [Rpc_websocket.Rpc.serve] carries both plain HTTP (the whitelisted static
    handler) and Async-RPC-over-WebSocket on one port. *)

open! Core
open! Async
module Protocol = Flappy_protocol

module Referee = struct
  type t =
    { mutable race : Protocol.Race_state.t
    ; occupied : bool array (* both indexed by Player_id.index *)
    ; latest : Protocol.Pos.t option array
    ; random : Random.State.t
    ; mutable events : Protocol.Stamped_event.t list (* newest first *)
    ; mutable next_seq : int
    ; claimed_boxes : Int.Hash_set.t (* this race's taken box ids *)
    }

  let create () =
    { race = Waiting_for_players
    ; occupied = Array.create ~len:2 false
    ; latest = Array.create ~len:2 None
    ; random = Random.State.make_self_init ()
    ; events = []
    ; next_seq = 0
    ; claimed_boxes = Int.Hash_set.create ()
    }
  ;;

  let slot t player = t.occupied.(Protocol.Player_id.index player)

  let current_seed t =
    match t.race with
    | Waiting_for_players -> None
    | Race { seed } -> Some seed
  ;;

  (* Stamp a fact into the log. [seq] never resets (so client acks stay
     monotonic across races); events are tagged with the race seed so clients
     drop leftovers from a previous race. *)
  let emit t event =
    match current_seed t with
    | None -> ()
    | Some race_seed ->
      t.events
      <- { Protocol.Stamped_event.seq = t.next_seq; race_seed; event }
         :: t.events;
      t.next_seq <- t.next_seq + 1
  ;;

  let events_after t ~seq =
    List.take_while t.events ~f:(fun e -> e.seq > seq) |> List.rev
  ;;

  (* A joining second player always starts a FRESH race (new seed), even if a
     previous race was underway — simplest rule that also covers "my opponent
     refreshed their tab". *)
  let start_new_race t =
    t.race <- Race { seed = Random.State.int t.random 1_000_000 };
    Hash_set.clear t.claimed_boxes
  ;;

  let join t ~name =
    let free =
      List.find Protocol.Player_id.all ~f:(fun player -> not (slot t player))
    in
    match free with
    | None ->
      Or_error.error_s
        [%message "race is full - two players already connected"]
    | Some player ->
      t.occupied.(Protocol.Player_id.index player) <- true;
      let both = Array.for_all t.occupied ~f:Fn.id in
      if both then start_new_race t;
      printf
        !"join: %{sexp: Protocol.Player_id.t} (%s)%s\n%!"
        player
        name
        (if both then " - race starts" else " - waiting for opponent");
      Ok player
  ;;

  let leave t player =
    t.occupied.(Protocol.Player_id.index player) <- false;
    t.latest.(Protocol.Player_id.index player) <- None;
    if Array.for_all t.occupied ~f:(fun o -> not o)
    then t.race <- Waiting_for_players;
    printf !"leave: %{sexp: Protocol.Player_id.t}\n%!" player
  ;;

  let sync t ({ player; pos; last_seen_event } : Protocol.Update.t) =
    if not (slot t player)
    then
      Or_error.error_s
        [%message
          "not joined (server restarted?) - refresh the page to rejoin"]
    else (
      t.latest.(Protocol.Player_id.index player) <- Some pos;
      Ok
        { Protocol.View.race = t.race
        ; opponent =
            t.latest.(Protocol.Player_id.index
                        (Protocol.Player_id.other player))
        ; events = events_after t ~seq:last_seen_event
        })
  ;;

  (* First request wins — the one true race condition, arbitrated here and
     nowhere else (context doc §3). The random item pick is also the server's
     so both players can't "roll their own luck"; weighting the odds for the
     trailing player is the Stage-8 rubber-band hook. *)
  let pickup_request t player ~box_id =
    if not (slot t player)
    then Or_error.error_s [%message "not joined"]
    else if Hash_set.mem t.claimed_boxes box_id
    then Ok None
    else (
      Hash_set.add t.claimed_boxes box_id;
      let item =
        List.nth_exn
          Flappy_game.Item.all
          (Random.State.int t.random (List.length Flappy_game.Item.all))
      in
      emit t (Powerup_claimed { box_id; by = player; item });
      Ok (Some item))
  ;;

  let use_powerup t player (use : Protocol.Use.t) =
    if not (slot t player)
    then Or_error.error_s [%message "not joined"]
    else (
      (match use with
       | Fire_volley { x; y } -> emit t (Volley_fired { by = player; x; y })
       | Swap_blocked -> emit t Swap_blocked
       | Swap ->
         (* Last-known positions at the moment of use; missing slots (never
            synced yet) fall back to the start position. *)
         let pos i =
           Option.value
             t.latest.(i)
             ~default:
               { Protocol.Pos.x = Flappy_game.Config.bird_start_x
               ; y = Flappy_game.Config.canvas_height /. 2.
               }
         in
         emit t (Swapped { p1 = pos 0; p2 = pos 1 }));
      Ok ())
  ;;

  let new_race t =
    if Array.for_all t.occupied ~f:Fn.id
    then (
      start_new_race t;
      printf "new race requested - starting\n%!";
      Ok ())
    else Or_error.error_s [%message "need two players for a new race"]
  ;;
end

(* Per-connection state: which slot (if any) this connection claimed, so the
   slot frees when the socket closes (tab refresh, disconnect). *)
module Session = struct
  type t = { mutable player : Protocol.Player_id.t option }
end

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

let implementations referee =
  Rpc.Implementations.create_exn
    ~implementations:
      [ Rpc.Rpc.implement'
          Protocol.join_rpc
          (fun (session : Session.t) name ->
             let result = Referee.join referee ~name in
             (match result with
              | Ok player -> session.player <- Some player
              | Error _ -> ());
             result)
      ; Rpc.Rpc.implement' Protocol.sync_rpc (fun (_ : Session.t) update ->
          Referee.sync referee update)
      ; Rpc.Rpc.implement'
          Protocol.pickup_request_rpc
          (fun (_ : Session.t) (player, box_id) ->
             Referee.pickup_request referee player ~box_id)
      ; Rpc.Rpc.implement'
          Protocol.use_powerup_rpc
          (fun (_ : Session.t) (player, use) ->
             Referee.use_powerup referee player use)
      ; Rpc.Rpc.implement' Protocol.new_race_rpc (fun (_ : Session.t) () ->
          Referee.new_race referee)
      ]
    ~on_unknown_rpc:`Close_connection
    ~on_exception:Log_on_background_exn
;;

let main ~port ~static_dir () =
  let referee = Referee.create () in
  let%bind (_ : (_, _) Cohttp_async.Server.t) =
    Rpc_websocket.Rpc.serve
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ~implementations:(implementations referee)
      ~initial_connection_state:
        (fun
          ()
          (_ : Rpc_websocket.Rpc.Connection_initiated_from.t)
          (_ : Socket.Address.Inet.t)
          (conn : Rpc.Connection.t)
        ->
        let session = { Session.player = None } in
        don't_wait_for
          (let%map () = Rpc.Connection.close_finished conn in
           Option.iter session.player ~f:(Referee.leave referee);
           session.player <- None);
        session)
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
    ~summary:
      "Serve the Flappy Racer client and referee 2-player races (slots, \
       seed, position relay - no physics)."
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
