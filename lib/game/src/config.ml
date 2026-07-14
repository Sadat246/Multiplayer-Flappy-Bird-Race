open! Core

let canvas_width = 960.
let canvas_height = 540.
let bird_size = 30.
let bird_start_x = 100.
let bird_screen_x = canvas_width /. 3.
let ground_height = 60.
let gravity = 2400.
let flap_impulse = -620.
let terminal_velocity = 950.

module Control_scheme = struct
  type t =
    | Hold
    | Set
    | Drift
  [@@deriving sexp_of, equal]
end

let control_scheme = Control_scheme.Drift
let speed_cap = 420.
let speed_floor = 190.
let speed_initial = 260.
let cruise_speed = speed_initial

(* Full brake-to-floor or floor-to-cap ramp takes (cap - floor) / accel_rate
   ~= 0.4s, per the context doc's "short acceleration ramps". *)
let accel_rate = 575.
let cruise_decay_rate = 200.
let pipe_width = 80.
let pipe_gap = 190.
let course_pipes = 30
let first_pipe_x = 900.

(* Baseline spacing (the current difficulty) is the FLOOR: gaps never get
   tighter than this. Breathers are deliberately much wider easy stretches. *)
let spacing_normal_min = 550.
let spacing_normal_max = 650.
let spacing_breather_min = 800.
let spacing_breather_max = 1000.
let breather_probability = 0.3
let finish_after_last_pipe = 800.
let gap_margin = 40.

(* Sustained flapping climbs ~310 px/s at this gravity/impulse; 250 is the
   conservative figure the fairness rule budgets with. *)
let climb_rate = 250.
let fairness_margin = 0.7
let debug_seed = 42
let item_box_size = 26.
let boost_duration = 3.0

(* One third over the normal cap: decisively faster, still steerable. *)
let boost_speed_cap = 560.

(* ~1.8x speed_cap: outruns everyone, but a trailing opponent ~1000px back
   still gets a >1s reaction window. *)
let bullet_speed = 756.
let bullet_radius = 6.

(* Playfield is 0..480 (ground_top). Five bands at 90px pitch leave ~78px
   clear between bullet edges - over two bird-heights per gap. *)
let volley_heights = [ 60.; 150.; 240.; 330.; 420. ]
let bullet_max_range = 2600.
let shield_break_invuln = 0.8
let countdown_duration = 5.0
let respawn_pause = 2.0
let invuln_duration = 1.5
let sim_dt = 1. /. 120.
let sync_hz = 25.
