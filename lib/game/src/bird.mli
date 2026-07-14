(** The player's bird: position, vertical velocity, and horizontal speed.

    Pure — no Async, no browser types — so it runs identically under
    js_of_ocaml (the client) and native (expect tests). Positions are the
    bird's top-left corner in world coordinates; the square extends
    {!Config.bird_size} right and down from there. *)

open! Core

module Speed_input : sig
  (** What the player's arrow keys are asking of the horizontal speed this
      instant. *)
  type t =
    | Accelerate (** right arrow held: ramp toward {!Config.speed_cap} *)
    | Brake (** left arrow held: ramp toward {!Config.speed_floor} *)
    | Coast
    (** no arrow held: hold current speed (control-scheme decision deferred
        to the Stage 2 checkpoint) *)
  [@@deriving sexp_of, equal]
end

type t =
  { x : float (** world x of the left edge; increases rightward *)
  ; y : float (** world y of the top edge; increases downward *)
  ; vy : float (** vertical velocity; positive = falling *)
  ; speed : float (** horizontal speed, always within [floor, cap] *)
  }
[@@deriving sexp_of, equal]

(** Bird at the race start position: mid-height, at rest vertically, at
    {!Config.speed_initial}. *)
val initial : t

(** Set vertical velocity to {!Config.flap_impulse} (an upward kick).
    Position is untouched — the kick shows up on the next {!step}. *)
val flap : t -> t

(** Advance one fixed timestep: apply gravity (clamped to
    {!Config.terminal_velocity}), move by current velocities, ramp [speed]
    toward what [speed_input] asks. The ceiling clamps rather than kills
    (classic flappy); collisions are {!World}'s business, not ours. *)
val step : t -> dt:float -> speed_input:Speed_input.t -> t
