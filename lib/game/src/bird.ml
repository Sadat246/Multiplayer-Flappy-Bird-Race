open! Core

module Speed_input = struct
  type t =
    | Accelerate
    | Brake
    | Coast
  [@@deriving sexp_of, equal]
end

type t =
  { x : float
  ; y : float
  ; vy : float
  ; speed : float
  }
[@@deriving sexp_of, equal]

let initial =
  { x = Config.bird_start_x
  ; y = (Config.canvas_height -. Config.bird_size) /. 2.
  ; vy = 0.
  ; speed = Config.speed_initial
  }
;;

let flap t = { t with vy = Config.flap_impulse }

(* Move [speed] toward [target] by at most [rate *. dt], without
   overshooting. *)
let ramp_speed speed ~target ~rate ~dt =
  let max_delta = rate *. dt in
  let delta =
    Float.clamp_exn (target -. speed) ~min:(-.max_delta) ~max:max_delta
  in
  speed +. delta
;;

let step
  t
  ~dt
  ~(speed_input : Speed_input.t)
  ~(scheme : Config.Control_scheme.t)
  ~speed_cap
  =
  let speed =
    match speed_input, scheme with
    | Accelerate, _ ->
      ramp_speed t.speed ~target:speed_cap ~rate:Config.accel_rate ~dt
    | Brake, _ ->
      ramp_speed
        t.speed
        ~target:Config.speed_floor
        ~rate:Config.accel_rate
        ~dt
    | Coast, Set -> t.speed
    | Coast, Hold ->
      ramp_speed
        t.speed
        ~target:Config.cruise_speed
        ~rate:Config.cruise_decay_rate
        ~dt
    | Coast, Drift ->
      (* Asymmetric: above cruise drifts back down (boost is temporary);
         at/below cruise holds (braking is persistent). *)
      if Float.( > ) t.speed Config.cruise_speed
      then
        ramp_speed
          t.speed
          ~target:Config.cruise_speed
          ~rate:Config.cruise_decay_rate
          ~dt
      else t.speed
  in
  (* Over the cap (a boost just expired): ramp back down, never snap. *)
  let speed =
    if Float.( > ) speed speed_cap
    then ramp_speed speed ~target:speed_cap ~rate:Config.accel_rate ~dt
    else speed
  in
  let vy =
    Float.min (t.vy +. (Config.gravity *. dt)) Config.terminal_velocity
  in
  let y = t.y +. (vy *. dt) in
  (* Ceiling clamps instead of killing: hitting the top just stops the climb,
     like the classic. *)
  let y, vy = if Float.( < ) y 0. then 0., 0. else y, vy in
  let x = t.x +. (speed *. dt) in
  { x; y; vy; speed }
;;
