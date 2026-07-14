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

(** Slowest allowed forward speed (~45% of [speed_cap]); never zero, never
    reverse. *)
val speed_floor : float

val speed_cap : float

(** Speed at race start, between floor and cap. *)
val speed_initial : float

(** How fast held arrows move the speed toward floor/cap. Derived from the
    ~0.4s ramp the context doc calls for. *)
val accel_rate : float

(** {2 Obstacles} *)

val pipe_width : float

(** Vertical clearance of each pipe pair's gap. *)
val pipe_gap : float

(** {2 Game flow} *)

(** How long the bird stays frozen after a crash before restarting. *)
val crash_pause : float

(** Fixed timestep of the simulation. The render loop may run at any monitor
    rate; physics always steps by exactly this. *)
val sim_dt : float
