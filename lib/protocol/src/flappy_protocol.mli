(** Wire types and RPC definitions shared by the browser client and the
    native server — the whole network protocol in one place, finalized before
    anything speaks it (the jsip-exchange discipline; shapes mirror
    [lib/gateway/src/rpc_protocol.ml] there).

    Architecture reminder (context doc §4): fat clients, thin referee.
    Clients own all physics and only exchange positions and race-lifecycle
    facts through these RPCs. Browsers cannot subscribe to server-push pipes
    (no [Rpc_effect.Pipe_rpc]), so the position stream is a poll-based state
    exchange: each client calls {!sync_rpc} at {!Flappy_game.Config.sync_hz}
    and the server answers from its latest-value slots — no queues anywhere. *)

open! Core
open Async_rpc_kernel

module Player_id : sig
  (** Which of the two race slots a client occupies. *)
  type t =
    | P1
    | P2
  [@@deriving bin_io, sexp_of, equal, enumerate]

  val other : t -> t

  (** 0 for [P1], 1 for [P2] — for array-backed server slots. *)
  val index : t -> int
end

module Pos : sig
  (** A bird's top-left corner in world coordinates (same convention as
      {!Flappy_game.Bird}). *)
  type t =
    { x : float
    ; y : float
    }
  [@@deriving bin_io, sexp_of, equal]
end

module Update : sig
  (** What a client tells the server every sync tick: who I am and where my
      bird is. Fire-and-forget semantics — losing one is fine, the next tick
      supersedes it. *)
  type t =
    { player : Player_id.t
    ; pos : Pos.t
    }
  [@@deriving bin_io, sexp_of]
end

module Race_state : sig
  (** The server's race lifecycle, as seen by clients. Stage 5 adds
      finished/winner states; for now a race just runs. *)
  type t =
    | Waiting_for_players
    | Race of { seed : int }
    (** Both slots filled: generate the course from [seed] and run the local
        countdown. A CHANGED seed means a new race — rebuild the world (this
        is how "new race" reaches both players). *)
  [@@deriving bin_io, sexp_of, equal]
end

module View : sig
  (** What the server answers every sync tick: the race lifecycle and the
      opponent's last-known position (server keeps a single latest-value slot
      per player — a slow client can never build up a backlog). *)
  type t =
    { race : Race_state.t
    ; opponent : Pos.t option
    }
  [@@deriving bin_io, sexp_of]
end

(** Claim a race slot. The string is a display name (unused so far, logged
    server-side). Errors when both slots are taken — a third visitor on a
    public server is expected, not exceptional. When the second player joins,
    the server starts a fresh race. *)
val join_rpc : (string, Player_id.t Or_error.t) Rpc.Rpc.t

(** The tick-rate state exchange described above. Errors if the player id is
    not currently joined (e.g. the server restarted): the client should
    surface "refresh to rejoin" rather than retry. *)
val sync_rpc : (Update.t, View.t Or_error.t) Rpc.Rpc.t

(** Ask the server to start a new race on a fresh seed (both players' result
    screens offer this). Errors unless two players are present. Stage 5 turns
    this into a proper ready-up. *)
val new_race_rpc : (unit, unit Or_error.t) Rpc.Rpc.t
