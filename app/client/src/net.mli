(** The client's network layer: WebSocket-RPC connection + the
    {!Flappy_game.Config.sync_hz} state-exchange loop. No game logic lives
    here (build-plan rule 5) — {!Game_loop} reads the accessors below each
    frame and decides what they mean.

    Flow: connect to the origin that served the page → [join] → poll [sync]
    forever (my position out, opponent position + race state in). All state
    is latest-value only; there is nothing to ack or replay. *)

open! Core

(** Connect and start the sync loop. Call exactly once at startup. *)
val start : unit -> unit

(** Where my bird is — written by {!Game_loop} every frame, sent to the
    server on the next sync tick. *)
val my_pos : Flappy_protocol.Pos.t ref

(** The current race's seed, once the server has started one. A change of
    seed means a new race. [None] = waiting in the lobby. *)
val race_seed : unit -> int option

(** Opponent's last received position (raw, unsmoothed — {!Game_loop}
    interpolates). [None] until an opponent is connected and racing. *)
val opponent : unit -> Flappy_protocol.Pos.t option

(** Milliseconds since [opponent] last changed, for the debug overlay
    (build-plan rule 6). [None] if no update has ever arrived. *)
val ms_since_opponent_update : unit -> int option

(** One-line connection status for the overlay / lobby screen. *)
val status_line : unit -> string

(** Ask the server to start a fresh race on a new seed (needs both players
    present; errors are surfaced in {!status_line}). *)
val request_new_race : unit -> unit
