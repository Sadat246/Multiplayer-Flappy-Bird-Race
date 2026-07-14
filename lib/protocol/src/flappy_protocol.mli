(** Wire types and RPC definitions shared by the browser client, the native
    Graphics client and the server — the whole network protocol in one place
    (the jsip-exchange discipline; shapes mirror
    [lib/gateway/src/rpc_protocol.ml] there).

    Architecture reminder (context doc §4): fat clients, thin referee.
    Clients own all physics; the server owns exactly three things — race
    slots + seed, item-box claim arbitration (the one true race condition),
    and relaying facts between clients. Browsers cannot subscribe to
    server-push pipes, so everything rides the poll-based {!sync_rpc} state
    exchange: positions as latest-value slots, and discrete facts (claims,
    volleys, swaps) as a sequence-numbered event log that each client
    acknowledges with [last_seen_event]. *)

open! Core
open Async_rpc_kernel
module Item := Flappy_game.Item

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
  (** What a client tells the server every sync tick. Fire-and-forget
      semantics for the position; [last_seen_event] is the event-log
      acknowledgment — the server answers with everything newer. *)
  type t =
    { player : Player_id.t
    ; pos : Pos.t
    ; last_seen_event : int
    (** highest event seq already processed; -1 = none *)
    }
  [@@deriving bin_io, sexp_of]
end

module Event : sig
  (** A discrete race fact the server relays to both players (the context
      doc's broadcast messages, log-shaped because clients poll). Every event
      carries the seed of the race it belongs to; clients drop events from a
      race they're no longer in. *)
  type t =
    | Powerup_claimed of
        { box_id : int
        ; by : Player_id.t
        ; item : Item.t
        } (** arbitration result: despawn the box; winner holds the item *)
    | Volley_fired of
        { by : Player_id.t
        ; x : float
        ; y : float
        }
    (** both clients spawn the 5 deterministic bullets locally; the victim's
        own world detects any hit on itself *)
    | Swapped of
        { p1 : Pos.t
        ; p2 : Pos.t
        }
    (** both clients teleport their own bird to the other's position
        simultaneously (last-known positions at the moment of use) *)
    | Swap_blocked
    (** the swap target's shield absorbed it: the initiator teleports back
        (accepted brief flicker, build plan 7d) *)
  [@@deriving bin_io, sexp_of, equal]
end

module Stamped_event : sig
  type t =
    { seq : int (** monotonically increasing across the server's lifetime *)
    ; race_seed : int (** which race this belongs to *)
    ; event : Event.t
    }
  [@@deriving bin_io, sexp_of]
end

module Race_state : sig
  (** The server's race lifecycle, as seen by clients. *)
  type t =
    | Waiting_for_players
    | Race of { seed : int }
    (** Both slots filled: generate the course from [seed] and run the local
        countdown. A CHANGED seed means a new race — rebuild the world (this
        is how "new race" reaches both players). *)
  [@@deriving bin_io, sexp_of, equal]
end

module View : sig
  (** What the server answers every sync tick. *)
  type t =
    { race : Race_state.t
    ; opponent : Pos.t option (** latest-value slot; no queues *)
    ; events : Stamped_event.t list
    (** everything newer than the update's [last_seen_event] *)
    }
  [@@deriving bin_io, sexp_of]
end

module Use : sig
  (** Item activations that need the server (boost and shield are purely
      local and never hit the wire). *)
  type t =
    | Fire_volley of
        { x : float
        ; y : float
        }
    | Swap
    | Swap_blocked (** self-report: my shield ate the swap I just received *)
  [@@deriving bin_io, sexp_of]
end

(** Claim a race slot. The string is a display name (logged server-side).
    Errors when both slots are taken. When the second player joins, the
    server starts a fresh race. *)
val join_rpc : (string, Player_id.t Or_error.t) Rpc.Rpc.t

(** The tick-rate state exchange described above. Errors if the player id is
    not currently joined (e.g. the server restarted): the client should
    surface "refresh to rejoin" rather than retry. *)
val sync_rpc : (Update.t, View.t Or_error.t) Rpc.Rpc.t

(** Claim an item box — the served-arbitrated race condition (≈ the
    exchange's [submit_order_rpc]). First request wins: [Some item] to the
    winner (server picks uniformly at random), [None] if somebody got there
    first. Either way a {!Event.Powerup_claimed} enters the log so both
    clients despawn the box. *)
val pickup_request_rpc
  : (Player_id.t * int, Item.t option Or_error.t) Rpc.Rpc.t

(** Activate a held item that affects the other player: the server stamps the
    corresponding {!Event.t} into the log for both clients. *)
val use_powerup_rpc : (Player_id.t * Use.t, unit Or_error.t) Rpc.Rpc.t

(** Ask the server to start a fresh race on a new seed (both players' result
    screens offer this). Errors unless two players are present. *)
val new_race_rpc : (unit, unit Or_error.t) Rpc.Rpc.t
