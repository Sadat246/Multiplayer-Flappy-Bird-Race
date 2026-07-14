open! Core
open Flappy_game

let step_seconds world ~seconds =
  let steps = Float.to_int (Float.round_up (seconds /. Config.sim_dt)) in
  List.fold (List.range 0 steps) ~init:world ~f:(fun world (_ : int) ->
    World.step world ~dt:Config.sim_dt ~speed_input:Coast)
;;

(* Phase constructor + crash count + rounded y: enough to pin behavior
   without depending on brittle countdown remainders. *)
let show (world : World.t) =
  let phase =
    match world.phase with Racing -> "racing" | Crashed _ -> "crashed"
  in
  print_s
    [%message
      phase
        ~crashes:(world.crashes : int)
        ~y:(Float.round_decimal ~decimal_digits:1 world.bird.y : float)]
;;

let%expect_test "falling to the ground crashes, pauses, then restarts" =
  (* Free fall from mid-height reaches the ground in ~0.41s (gravity 2400,
     ground at bird-y 450). At 0.5s we're mid-pause; by 1.2s the pause (0.6s)
     has elapsed and the bird is racing again from the start. *)
  let crashed = step_seconds World.initial ~seconds:0.5 in
  show crashed;
  let restarted = step_seconds crashed ~seconds:0.7 in
  show restarted;
  [%expect
    {|
    (crashed (crashes 1) (y 450.9))
    (racing (crashes 1) (y 305))
    |}]
;;

let%expect_test "flying into a pipe crashes" =
  (* Start 10px left of the first pipe's face (x 700), level with its top
     section: at speed 260 the bird hits within ~0.05s, barely fallen. *)
  let bird = { Bird.initial with x = 660.; y = 100.; vy = 0. } in
  let world = { World.initial with bird } in
  let crashed = step_seconds world ~seconds:0.3 in
  show crashed;
  [%expect {| (crashed (crashes 1) (y 102.5)) |}]
;;

let%expect_test "dead birds don't flap" =
  let crashed = { World.initial with phase = Crashed { time_left = 0.5 } } in
  let after = World.flap crashed in
  print_s [%sexp (World.equal crashed after : bool)];
  [%expect {| true |}]
;;

let%expect_test "course sanity: every rect sits between ceiling and ground" =
  let ok =
    List.for_all Course.rects ~f:(fun { x = _; y; w = _; h } ->
      Float.( >= ) y 0. && Float.( <= ) (y +. h) Course.ground_top)
  in
  print_s [%message (List.length Course.rects : int) (ok : bool)];
  [%expect {| (("List.length Course.rects" 16) (ok true)) |}]
;;
