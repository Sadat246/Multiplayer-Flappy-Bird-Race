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
let race_seed () = !seed
let opponent () = !opponent_pos

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
     seed := Some s;
     status := In_race);
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
          { Protocol.Update.player; pos = !my_pos }
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
