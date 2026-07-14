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
  [@@deriving sexp_of, equal]
end

let control_scheme = Control_scheme.Set
let speed_cap = 420.
let speed_floor = 190.
let speed_initial = 260.
let cruise_speed = 280.

(* Full brake-to-floor or floor-to-cap ramp takes (cap - floor) / accel_rate
   ~= 0.4s, per the context doc's "short acceleration ramps". *)
let accel_rate = 575.
let cruise_decay_rate = 250.
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
let countdown_duration = 5.0
let respawn_pause = 2.0
let invuln_duration = 1.5
let sim_dt = 1. /. 120.
