(** The whole single-player race and its step function: pre-race countdown,
    bird physics, collision against a seeded {!Course}, death →
    respawn-with-i-frames, and the finish line.

    Pure and deterministic — the client's render loop drives it with fixed
    timesteps and key state; expect tests drive it the same way. In Stage 4
    this same state machine runs on each client with its own bird; the server
    never sees any of it (context doc §4). *)

open! Core

module Phase : sig
  type t =
    | Countdown of { time_left : float }
    (** Pre-race: bird frozen at the start, clock not running. Becomes
        [Racing] at 0. Stage 5 syncs this across players via the server's
        [start] message. *)
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

type t =
  { bird : Bird.t
  ; phase : Phase.t
  ; invuln_left : float
  (** seconds of post-respawn invulnerability remaining; 0 = vulnerable.
      While positive, pipe hits are ignored (ground still kills — you have to
      dive at it deliberately for 1.5s). *)
  ; crashes : int (** deaths this race, for the overlay *)
  ; elapsed : float (** race clock; runs from GO, keeps running while dead *)
  ; seed : int (** the seed this race's course was generated from *)
  ; course : Course.t
  }
[@@deriving sexp_of, equal]

(** A fresh race on the course generated from [seed], starting with the
    {!Config.countdown_duration} countdown. *)
val create : seed:int -> t

(** A NEW race on a NEW course: the seed advances deterministically (seed +
    1), so every race is a fresh map yet any race can be reproduced from the
    seed shown in the debug overlay. The result screen's "press R";
    multiplayer's ready-up (Stage 5) will call the server-supplied
    equivalent. *)
val new_race : t -> t

(** Flap, if racing. Frozen, dead and finished birds don't flap. *)
val flap : t -> t

(** Advance one fixed timestep.

    [Countdown]: tick toward GO; bird frozen, clock not running.

    [Racing]: physics, then finish-line check, then collision — ground always
    kills; pipes kill unless invulnerable. Death kills any upward velocity
    (the tumble IS the physics, context doc §2) and starts the
    {!Config.respawn_pause} countdown.

    [Dead]: gravity keeps pulling the bird down to the ground (the visible
    tumble), x frozen; on expiry the bird respawns at
    {!Course.safe_respawn_x}, mid-height, with {!Config.invuln_duration} of
    i-frames.

    [Finished]: frozen. *)
val step : t -> dt:float -> speed_input:Bird.Speed_input.t -> t
