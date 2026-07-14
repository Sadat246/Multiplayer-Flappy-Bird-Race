(** The client's network layer: WebSocket-RPC connection + the
    {!Flappy_game.Config.sync_hz} state-exchange loop. No game logic lives
    here (build-plan rule 5) — {!Game_loop} reads the accessors below each
    frame and decides what they mean.

    Flow: connect to the origin that served the page → [join] → poll [sync]
    forever (my position + event-log ack out; opponent position, race state
    and new events in). Positions are latest-value only; events are the
    reliable channel for discrete facts (claims, volleys, swaps) and are
    queued here until {!Game_loop} drains them. *)

open! Core

(** Connect and start the sync loop. Call exactly once at startup. *)
val start : unit -> unit

(** My race slot, once joined. Needed to interpret events ("which side of a
    swap am I?"). *)
val me : unit -> Flappy_protocol.Player_id.t option

(** Where my bird is — written by {!Game_loop} every frame, sent to the
    server on the next sync tick. *)
val my_pos : Flappy_protocol.Pos.t ref

(** The current race's seed, once the server has started one. A change of
    seed means a new race. [None] = waiting in the lobby. *)
val race_seed : unit -> int option

(** Opponent's last received position (raw, unsmoothed — {!Game_loop}
    interpolates). *)
val opponent : unit -> Flappy_protocol.Pos.t option

(** Take all events received since the last call, oldest first, already
    filtered to the given race seed (stale events from a previous race are
    dropped, but still acknowledged). *)
val drain_events : current_seed:int -> Flappy_protocol.Event.t list

(** Ask to claim an item box (server-arbitrated, first request wins). The
    result arrives via {!drain_pickup_results}; at most one request per box
    is ever in flight. *)
val request_pickup : box_id:int -> unit

(** Results of my pickup requests: [Some item] = mine, [None] = opponent got
    there first. *)
val drain_pickup_results : unit -> Flappy_game.Item.t option list

(** Tell the server I used a volley/swap (boost and shield never hit the
    wire). *)
val send_use : Flappy_protocol.Use.t -> unit

(** Milliseconds since [opponent] last changed, for the debug overlay
    (build-plan rule 6). [None] if no update has ever arrived. *)
val ms_since_opponent_update : unit -> int option

(** One-line connection status for the overlay / lobby screen. *)
val status_line : unit -> string

(** Ask the server to start a fresh race on a new seed (needs both players
    present; errors are surfaced in {!status_line}). *)
val request_new_race : unit -> unit
