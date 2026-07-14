open! Core
open Async_kernel
open Async_js
module Protocol = Flappy_protocol
module Config = Flappy_game.Config

type status =
  | Connecting
  | Waiting_for_opponent
  | In_race
  | Failed of string

(* --- Latest-value state, read by Game_loop each frame. --- *)

let status = ref Connecting
let my_pos = ref { Protocol.Pos.x = 0.; y = 0. }
let seed = ref None
let opponent_pos = ref None
let opponent_updated_at = ref None
let connection = ref None
let player_id = ref None

(* Events queue newest-last; [last_seen] is the ack we send with every sync.
   Stale-race events are dropped at drain time but still acked — acking is
   about the log position, not about caring. *)
let event_queue : Protocol.Stamped_event.t Queue.t = Queue.create ()
let last_seen = ref (-1)
let pickup_results : Flappy_game.Item.t option Queue.t = Queue.create ()
let pickups_in_flight : Int.Hash_set.t = Int.Hash_set.create ()
let me () = !player_id
let race_seed () = !seed
let opponent () = !opponent_pos

let drain_events ~current_seed =
  let all = Queue.to_list event_queue in
  Queue.clear event_queue;
  List.filter_map all ~f:(fun { seq = _; race_seed; event } ->
    Option.some_if (race_seed = current_seed) event)
;;

let drain_pickup_results () =
  let all = Queue.to_list pickup_results in
  Queue.clear pickup_results;
  all
;;

let ms_since_opponent_update () =
  Option.map !opponent_updated_at ~f:(fun at ->
    Time_ns.diff (Time_ns.now ()) at |> Time_ns.Span.to_int_ms)
;;

let status_line () =
  match !status with
  | Connecting -> "connecting..."
  | Waiting_for_opponent -> "waiting for another player to join..."
  | In_race -> "in race"
  | Failed reason ->
    [%string "connection lost: %{reason} - refresh to rejoin"]
;;

let apply_view (view : Protocol.View.t) =
  (match view.race with
   | Waiting_for_players ->
     seed := None;
     status := Waiting_for_opponent
   | Race { seed = s } ->
     (* New race: box ids restart from 0, so in-flight pickup tracking from
        the previous course must not suppress requests. *)
     if not ([%equal: int option] (Some s) !seed)
     then (
       Hash_set.clear pickups_in_flight;
       Queue.clear pickup_results);
     seed := Some s;
     status := In_race);
  List.iter view.events ~f:(fun stamped ->
    last_seen := Int.max !last_seen stamped.seq;
    Queue.enqueue event_queue stamped);
  match view.opponent with
  | None ->
    opponent_pos := None;
    opponent_updated_at := None
  | Some pos ->
    let changed =
      not
        (Option.value_map
           !opponent_pos
           ~default:false
           ~f:(Protocol.Pos.equal pos))
    in
    opponent_pos := Some pos;
    if changed then opponent_updated_at := Some (Time_ns.now ())
;;

let sync_loop conn player =
  Clock_ns.every'
    (Time_ns.Span.of_sec (1. /. Config.sync_hz))
    (fun () ->
      match%map
        Rpc.Rpc.dispatch
          Protocol.sync_rpc
          conn
          { Protocol.Update.player
          ; pos = !my_pos
          ; last_seen_event = !last_seen
          }
      with
      | Ok (Ok view) -> apply_view view
      | Ok (Error err) ->
        (* Domain error: our slot is gone (server restart). Stop pretending
           we're connected; the banner tells the player to refresh. *)
        status := Failed (Error.to_string_hum err)
      | Error (_ : Error.t) ->
        (* Transport hiccup on one tick: fire-and-forget semantics, the next
           tick supersedes it. *)
        ())
;;

let start () =
  don't_wait_for
    (match%bind Rpc.Connection.client () with
     | Error err ->
       status := Failed (Error.to_string_hum err);
       return ()
     | Ok conn ->
       connection := Some conn;
       (match%map Rpc.Rpc.dispatch Protocol.join_rpc conn "anon" with
        | Ok (Ok player) ->
          player_id := Some player;
          status := Waiting_for_opponent;
          sync_loop conn player
        | Ok (Error err) -> status := Failed (Error.to_string_hum err)
        | Error err -> status := Failed (Error.to_string_hum err)))
;;

let request_pickup ~box_id =
  match !connection, !player_id with
  | Some conn, Some player when not (Hash_set.mem pickups_in_flight box_id)
    ->
    Hash_set.add pickups_in_flight box_id;
    don't_wait_for
      (match%map
         Rpc.Rpc.dispatch Protocol.pickup_request_rpc conn (player, box_id)
       with
       | Ok (Ok result) -> Queue.enqueue pickup_results result
       | Ok (Error (_ : Error.t)) | Error (_ : Error.t) ->
         (* Failed request: allow a retry on next touch. *)
         Hash_set.remove pickups_in_flight box_id)
  | _ -> ()
;;

let send_use use =
  match !connection, !player_id with
  | Some conn, Some player ->
    don't_wait_for
      (Rpc.Rpc.dispatch Protocol.use_powerup_rpc conn (player, use)
       >>| (ignore : (unit Or_error.t, Error.t) Result.t -> unit))
  | _ -> ()
;;

let request_new_race () =
  match !connection with
  | None -> ()
  | Some conn ->
    don't_wait_for
      (match%map Rpc.Rpc.dispatch Protocol.new_race_rpc conn () with
       | Ok (Ok ()) | Ok (Error _) | Error _ ->
         (* Success shows up as a seed change on the next sync; errors
            (opponent missing) just mean nothing happens. *)
         ())
;;
