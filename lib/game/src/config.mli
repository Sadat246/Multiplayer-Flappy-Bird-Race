(** Every tunable number in the game, in one place (build-plan rule 4).

    Units are pixels and seconds throughout; the y axis points down (canvas
    convention), so a negative [flap_impulse] is an upward kick. When a feel
    change is requested, change these constants first — logic only if
    constants can't do it. *)

(** {2 Canvas and world geometry} *)

val canvas_width : float
val canvas_height : float

(** Side length of the (square) bird. The collision box IS the drawn shape
    (build-plan rule 3). *)
val bird_size : float

(** World x where the bird starts a race (also the anchor for the first
    respawn candidate). *)
val bird_start_x : float

(** Screen x at which the bird is drawn; the world scrolls past it (~1/3 from
    the left edge). *)
val bird_screen_x : float

val ground_height : float

(** {2 Vertical physics} *)

val gravity : float

(** Vertical velocity set by a flap. Negative = upward. *)
val flap_impulse : float

(** Fastest the bird may fall. *)
val terminal_velocity : float

(** {2 Horizontal speed control} *)

module Control_scheme : sig
  (** The two candidate control schemes from the context doc §1 — both are
      implemented; playtesting picks one (Stage 2 checkpoint). *)
  type t =
    | Hold
    (** arrows are momentary: releasing them decays speed back toward
        [cruise_speed] at [cruise_decay_rate] *)
    | Set (** arrows adjust a persistent speed; releasing holds it *)
  [@@deriving sexp_of, equal]
end

(** Which scheme the client uses. Flip and rebuild to compare. *)
val control_scheme : Control_scheme.t

(** Slowest allowed forward speed (~45% of [speed_cap]); never zero, never
    reverse. *)
val speed_floor : float

val speed_cap : float

(** Speed at race start, between floor and cap. *)
val speed_initial : float

(** Speed the [Hold] scheme relaxes toward when no arrow is held. *)
val cruise_speed : float

(** How fast held arrows move the speed toward floor/cap. Derived from the
    ~0.4s ramp the context doc calls for. *)
val accel_rate : float

(** How fast the [Hold] scheme relaxes toward [cruise_speed] — gentler than
    an actively held arrow. *)
val cruise_decay_rate : float

(** {2 Course generation}

    The course is generated deterministically from a seed. Spacing between
    consecutive pipes NEVER goes below [spacing_normal_min] (the baseline
    difficulty); with probability [breather_probability] a gap is instead
    drawn from the much wider breather range — deliberate easy stretches, and
    where Stage 6 will prefer to place item boxes so grabbing a power-up
    leaves room to recover. *)

val pipe_width : float

(** Vertical clearance of each pipe pair's gap. *)
val pipe_gap : float

(** Number of pipe pairs in a race. *)
val course_pipes : int

(** World x of the first pipe (the runway before it). *)
val first_pipe_x : float

val spacing_normal_min : float
val spacing_normal_max : float
val spacing_breather_min : float
val spacing_breather_max : float
val breather_probability : float

(** Distance from the last pipe to the finish line. *)
val finish_after_last_pipe : float

(** Minimum distance from the ceiling / the ground to a gap edge. *)
val gap_margin : float

(** Conservative sustained climb rate (px/s) a player can hold by flapping
    rhythmically — the input to the fairness rule: consecutive gap centers
    may differ by at most the height change achievable at [speed_cap]. *)
val climb_rate : float

(** Extra safety factor (< 1) applied on top of [climb_rate] so the fairness
    bound is comfortably, not marginally, achievable. *)
val fairness_margin : float

(** Seed of the FIRST race after page load (fixed until Stage 7, per
    build-plan rule 7, so tuning runs are comparable). Each "new race"
    advances the seed by one — fresh course per race, still reproducible
    from the seed shown in the debug overlay. *)
val debug_seed : int

(** {2 Death, respawn, race flow} *)

(** Pre-race countdown: the bird hangs frozen this long before control
    begins. Single-player ticks it locally; Stage 5's server drives the same
    countdown for both players from the [start] message. *)
val countdown_duration : float

(** How long the bird stays dead before respawning. *)
val respawn_pause : float

(** Invulnerability window after a respawn (flashing sprite; pipe collisions
    ignored). *)
val invuln_duration : float

(** Fixed timestep of the simulation. The render loop may run at any monitor
    rate; physics always steps by exactly this. *)
val sim_dt : float
