open! Core
open Flappy_game

(* Print with one decimal so trajectories read cleanly; the physics is
   deterministic, the noise beyond 0.1px is not interesting. *)
let show (bird : Bird.t) =
  let r = Float.round_decimal ~decimal_digits:1 in
  print_s
    [%message
      ""
        ~x:(r bird.x : float)
        ~y:(r bird.y : float)
        ~vy:(r bird.vy : float)
        ~speed:(r bird.speed : float)]
;;

(* Step [n] times with the same input, printing every [every] steps. *)
let trajectory ?(n = 60) ?(every = 15) bird ~speed_input =
  let (_ : Bird.t) =
    List.fold
      (List.range 0 (n + 1))
      ~init:bird
      ~f:(fun bird i ->
        if i % every = 0 then show bird;
        Bird.step bird ~dt:Config.sim_dt ~speed_input)
  in
  ()
;;

let%expect_test "gravity pulls the bird down, capped at terminal velocity" =
  (* At gravity 2400 and terminal 950, the fall saturates within ~0.4s: every
     sample after the first is already at terminal velocity, and y advances
     by a constant 475 (= 950 * 0.5s) per 60-step sample. *)
  trajectory Bird.initial ~n:240 ~every:60 ~speed_input:Coast;
  [%expect
    {|
    ((x 100) (y 255) (vy 0) (speed 260))
    ((x 230) (y 545.9) (vy 950) (speed 260))
    ((x 360) (y 1020.9) (vy 950) (speed 260))
    ((x 490) (y 1495.9) (vy 950) (speed 260))
    ((x 620) (y 1970.9) (vy 950) (speed 260))
    |}]
;;

let%expect_test "flap kicks the bird upward, then gravity takes over" =
  (* vy runs -620 -> +180 over the first 40 steps (1/3s of gravity 2400): the
     bird rises ~70px, tops out, and is falling again by sample 2. *)
  let bird = Bird.flap Bird.initial in
  trajectory bird ~n:120 ~every:40 ~speed_input:Coast;
  [%expect
    {|
    ((x 100) (y 255) (vy -620) (speed 260))
    ((x 186.7) (y 185) (vy 180) (speed 260))
    ((x 273.3) (y 381.3) (vy 950) (speed 260))
    ((x 360) (y 698) (vy 950) (speed 260))
    |}]
;;

let%expect_test "speed ramps toward cap and floor, never beyond either" =
  let rec hold bird ~speed_input ~seconds =
    if Float.( <= ) seconds 0.
    then bird
    else
      hold
        (Bird.step bird ~dt:Config.sim_dt ~speed_input)
        ~speed_input
        ~seconds:(seconds -. Config.sim_dt)
  in
  let flooring = hold Bird.initial ~speed_input:Brake ~seconds:1. in
  print_s [%sexp (flooring.speed : float)];
  let capped = hold flooring ~speed_input:Accelerate ~seconds:1. in
  print_s [%sexp (capped.speed : float)];
  [%expect {|
    190
    420
    |}]
;;

let%expect_test "ceiling clamps instead of killing" =
  (* One step at vy -600 rises ~5px: starting at y 1 crosses the ceiling,
     which must clamp position and velocity to zero, not crash. *)
  let bird = { Bird.initial with y = 1.; vy = -600. } in
  let bird = Bird.step bird ~dt:Config.sim_dt ~speed_input:Coast in
  print_s [%sexp (bird.y : float), (bird.vy : float)];
  [%expect {| (0 0) |}]
;;
