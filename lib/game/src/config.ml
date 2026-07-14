let canvas_width = 960.
let canvas_height = 540.
let bird_size = 30.
let bird_screen_x = canvas_width /. 3.
let ground_height = 60.
let gravity = 2400.
let flap_impulse = -620.
let terminal_velocity = 950.
let speed_cap = 420.
let speed_floor = 190.
let speed_initial = 260.

(* Full brake-to-floor or floor-to-cap ramp takes (cap - floor) / accel_rate
   ~= 0.4s, per the context doc's "short acceleration ramps". *)
let accel_rate = 575.
let pipe_width = 80.
let pipe_gap = 190.
let crash_pause = 0.6
let sim_dt = 1. /. 120.
