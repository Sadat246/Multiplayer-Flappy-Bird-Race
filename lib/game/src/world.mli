(** The whole single-player-side race simulation: pre-race countdown, bird
    physics, collision against a seeded {!Course}, death →
    respawn-with-i-frames, the finish line — and the power-up layer: held
    item, boost, shield, incoming volley bullets, swaps.

    Pure and deterministic. The client drives it with fixed timesteps and key
    state; expect tests drive it the same way. Networked effects follow the
    context doc's client-owns-own-fate rule: THIS world only ever hurts its
    OWN bird (bullets are checked against my bird only; my death is detected
    by my simulation). The client glue translates network events into the
    [receive_*] / [apply_*] calls below and my outgoing actions
    ({!use_held_item}, {!touching_unclaimed_box}) into RPCs — no networking
    happens in here. *)

open! Core

module Phase : sig
  type t =
    | Countdown of { time_left : float }
    (** Pre-race: bird frozen at the start, clock not running. Becomes
        [Racing] at 0. *)
    | Racing
    | Dead of
        { time_left : float
        ; died_at : float (** world x of the death, for the respawn snap *)
        }
    (** Tumbling/paused; respawns when [time_left] reaches 0 at the nearest
        safe x, mid-height, with i-frames. *)
    | Finished of { time : float (** race time in seconds *) }
  [@@deriving sexp_of, equal]
end

module Bullet : sig
  (** One volley bullet, flying rightward at {!Config.bullet_speed}.
      [hostile] bullets (fired by the opponent) can hit my bird; my own
      volley is display-only here — the opponent's world hurts them. *)
  type t =
    { x : float
    ; y : float (** center *)
    ; hostile : bool
    ; traveled : float (** despawns past {!Config.bullet_max_range} *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { bird : Bird.t
  ; phase : Phase.t
  ; invuln_left : float
  (** seconds of invulnerability remaining (respawn, swap landing, or shield
      break); 0 = vulnerable. While positive, pipe hits and bullets are
      ignored (ground still kills). *)
  ; crashes : int (** deaths this race, for the overlay *)
  ; elapsed : float (** race clock; keeps running while dead *)
  ; seed : int (** the seed this race's course was generated from *)
  ; course : Course.t
  ; held_item : Item.t option (** one item max (context doc §3) *)
  ; boost_left : float (** seconds of raised speed cap remaining *)
  ; shielded : bool
  ; stunned : bool
  (** knocked down by a bullet: flaps disabled until the ground death — the
      knockdown IS the physics (context doc §2) *)
  ; swap_revert : (float * float) option
  (** my pre-swap position, kept in case the opponent's shield blocks the
      swap and we must jump back *)
  ; bullets : Bullet.t list
  ; boxes_taken : Int.Set.t (** despawned item-box ids *)
  }
[@@deriving sexp_of, equal]

(** A fresh race on the course generated from [seed], starting with the
    {!Config.countdown_duration} countdown. *)
val create : seed:int -> t

(** A NEW race on a NEW course (seed + 1) — single-player/dev flow; in
    multiplayer the server hands out the next seed instead. *)
val new_race : t -> t

(** Flap, if racing and not knocked down. *)
val flap : t -> t

(** Advance one fixed timestep. Beyond the Stage-3 behavior (physics, finish,
    collision, death/respawn), this also ticks boost, moves bullets (hostile
    ones can knock my bird down: flaps cut, vertical velocity slammed to
    terminal — the existing ground death finishes the job), and lets an
    active shield absorb what would otherwise be the death, granting
    {!Config.shield_break_invuln} to escape. *)
val step : t -> dt:float -> speed_input:Bird.Speed_input.t -> t

(** {2 My outgoing actions (client turns these into RPCs)} *)

(** The unclaimed box my bird is overlapping right now, if any — and only if
    my hands are free (one item max; further boxes are ignored while
    holding). The client sends [pickup_request] for it; the box only despawns
    when the server's claim event comes back. *)
val touching_unclaimed_box : t -> int option

(** Use the held item. [`Applied] = it was local (boost, shield) and is
    already in effect. [`Fire_volley]/[`Request_swap] = the client must send
    [use_powerup] so the server broadcasts it; the world has already dropped
    the item. [`Nothing] = no item held or not racing. *)
val use_held_item
  :  t
  -> t
     * [ `Applied
       | `Fire_volley of float * float
       | `Request_swap
       | `Nothing
       ]

(** {2 Incoming network events (client calls these when they arrive)} *)

(** The server awarded me this item (my pickup_request won). *)
val receive_pickup : t -> Item.t -> t

(** Somebody claimed this box (me or the opponent): despawn it. *)
val box_claimed : t -> box_id:int -> t

(** A volley was fired at [x, y]. [hostile] = the opponent fired it (its
    bullets can hit me); my own volley is spawned for display only. *)
val receive_volley : t -> x:float -> hostile:bool -> t

(** A swap happened; [other] is the position I'm teleported to. [`Swapped]: I
    moved (safe-snapped if the destination overlaps a pipe, with i-frames —
    someone may materialize inside geometry, context doc §3). [`Blocked]: my
    shield absorbed it — I didn't move, the shield is consumed, and the
    client must broadcast [Swap_blocked] so the initiator reverts. *)
val receive_swap
  :  t
  -> other:float * float
  -> [ `Swapped of t | `Blocked of t ]

(** The opponent's shield blocked my swap: teleport back to where I was (the
    accepted brief flicker — build plan 7d). *)
val receive_swap_blocked : t -> t
