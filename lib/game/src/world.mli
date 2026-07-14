(** The whole single-player game state and its step function: bird physics
    plus collision against {!Course} and the ground, with a short frozen
    pause after a crash and then a restart from the beginning.

    Pure and deterministic — the client's render loop drives it with fixed
    timesteps and key state; expect tests drive it the same way. *)

open! Core

module Phase : sig
  type t =
    | Racing
    | Crashed of { time_left : float }
    (** Frozen where the crash happened; restarts when it reaches 0. *)
  [@@deriving sexp_of, equal]
end

type t =
  { bird : Bird.t
  ; phase : Phase.t
  ; crashes : int (** total crashes since page load, for the overlay *)
  }
[@@deriving sexp_of, equal]

val initial : t

(** Flap, if racing. Dead birds don't flap. *)
val flap : t -> t

(** Advance one fixed timestep. While [Racing]: physics, then collision — any
    pipe overlap or touching the ground crashes (pauses
    {!Config.crash_pause}, then restarts from {!Bird.initial}). While
    [Crashed]: count down only. *)
val step : t -> dt:float -> speed_input:Bird.Speed_input.t -> t
