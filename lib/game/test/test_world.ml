open! Core
open Flappy_game

let step_seconds world ~seconds =
  let steps = Float.to_int (Float.round_up (seconds /. Config.sim_dt)) in
  List.fold (List.range 0 steps) ~init:world ~f:(fun world (_ : int) ->
    World.step world ~dt:Config.sim_dt ~speed_input:Coast)
;;

(* A world already past the countdown, for tests about racing behavior. *)
let racing_world () =
  { (World.create ~seed:Config.debug_seed) with phase = Racing }
;;

(* Phase constructor + counters + rounded position: enough to pin behavior
   without depending on brittle countdown remainders. *)
let show (world : World.t) =
  let r = Float.round_decimal ~decimal_digits:1 in
  let phase =
    match world.phase with
    | Countdown _ -> "countdown"
    | Racing when Float.( > ) world.invuln_left 0. -> "racing+invuln"
    | Racing -> "racing"
    | Dead _ -> "dead"
    | Finished _ -> "finished"
  in
  print_s
    [%message
      phase
        ~crashes:(world.crashes : int)
        ~x:(r world.bird.x : float)
        ~y:(r world.bird.y : float)]
;;

let%expect_test "races start with a countdown: bird frozen, clock stopped" =
  let world = World.create ~seed:Config.debug_seed in
  show world;
  (* 4.9s in: still counting down, bird exactly where it started, no race
     time elapsed. *)
  let almost = step_seconds world ~seconds:4.9 in
  show almost;
  print_s [%sexp (almost.elapsed : float)];
  (* Flaps during the countdown are ignored. *)
  print_s [%sexp (World.equal almost (World.flap almost) : bool)];
  (* Past 5s: racing (and falling — flap or die!). *)
  let racing =
    step_seconds world ~seconds:(Config.countdown_duration +. 0.1)
  in
  show racing;
  [%expect
    {|
    (countdown (crashes 0) (x 100) (y 255))
    (countdown (crashes 0) (x 100) (y 255))
    0
    true
    (racing (crashes 0) (x 126) (y 268))
    |}]
;;

let%expect_test "free fall: dies on the ground, respawns safely with \
                 i-frames"
  =
  (* No flapping: the bird free-falls into the ground in ~0.41s. At 0.5s it
     is dead and tumbled to rest on the ground. After the 2s respawn pause it
     is racing again at the nearest safe x (the runway — it never reached the
     first pipe), mid-height, invulnerable. Left to itself it free-falls to
     death again — hence crashes 2 by the last sample. *)
  let dead = step_seconds (racing_world ()) ~seconds:0.5 in
  show dead;
  let respawned = step_seconds dead ~seconds:Config.respawn_pause in
  show respawned;
  let later = step_seconds respawned ~seconds:Config.invuln_duration in
  show later;
  [%expect
    {|
    (dead (crashes 1) (x 204) (y 450))
    (racing+invuln (crashes 1) (x 123.8) (y 236))
    (dead (crashes 2) (x 212.7) (y 450))
    |}]
;;

let%expect_test "pipe overlap kills when vulnerable, passes through with \
                 i-frames"
  =
  let world = racing_world () in
  (* Place the bird inside the first pipe's top rectangle. *)
  let first_pipe = List.hd_exn world.course.pipes in
  let inside = { Bird.initial with x = first_pipe.x +. 10.; y = 10. } in
  let vulnerable =
    World.step
      { world with bird = inside }
      ~dt:Config.sim_dt
      ~speed_input:Coast
  in
  show vulnerable;
  let invulnerable =
    World.step
      { world with bird = inside; invuln_left = 1.0 }
      ~dt:Config.sim_dt
      ~speed_input:Coast
  in
  show invulnerable;
  [%expect
    {|
    (dead (crashes 1) (x 912.2) (y 10.2))
    (racing+invuln (crashes 0) (x 912.2) (y 10.2))
    |}]
;;

let%expect_test "respawn snaps to the nearest gap between pipes" =
  (* Kill the bird midway between pipes 3 and 4: the respawn x must be clear
     of both, and mid-height. *)
  let world = racing_world () in
  let p3 = List.nth_exn world.course.pipes 2 in
  let p4 = List.nth_exn world.course.pipes 3 in
  let died_at = (p3.x +. p4.x) /. 2. in
  let dead =
    { world with
      phase = Dead { time_left = Config.sim_dt /. 2.; died_at }
    ; bird = { Bird.initial with x = died_at }
    }
  in
  let respawned = World.step dead ~dt:Config.sim_dt ~speed_input:Coast in
  let clear =
    not
      (List.exists world.course.rects ~f:(fun rect ->
         Course.Rect.hits_bird
           rect
           ~bird_x:respawned.bird.x
           ~bird_y:respawned.bird.y))
  in
  print_s
    [%message
      (clear : bool)
        ~between:
          (Float.( < ) p3.x respawned.bird.x
           && Float.( < ) respawned.bird.x p4.x
           : bool)];
  [%expect {| ((clear true) (between true)) |}]
;;

let%expect_test "crossing the finish line ends the race and freezes the \
                 world"
  =
  let world = racing_world () in
  let near_finish =
    { world with
      bird = { Bird.initial with x = world.course.finish_x -. 5.; y = 200. }
    ; elapsed = 61.5
    }
  in
  let finished = step_seconds near_finish ~seconds:0.1 in
  show finished;
  (match finished.phase with
   | Finished { time } ->
     print_s [%sexp (Float.round_decimal time ~decimal_digits:1 : float)]
   | Countdown _ | Racing | Dead _ -> print_s [%sexp "not finished!"]);
  (* Frozen: further steps change nothing. *)
  let later = step_seconds finished ~seconds:1. in
  print_s [%sexp (World.equal finished later : bool)];
  [%expect
    {|
    (finished (crashes 0) (x 22594.1) (y 201))
    61.5
    true
    |}]
;;

let%expect_test "new race: fresh countdown on a DIFFERENT course, seed + 1" =
  let world = World.create ~seed:Config.debug_seed in
  let messy =
    { world with
      phase = Finished { time = 99. }
    ; crashes = 7
    ; elapsed = 99.
    ; bird = { Bird.initial with x = 12345. }
    }
  in
  let fresh = World.new_race messy in
  print_s
    [%message
      ""
        ~seed:(fresh.seed : int)
        ~new_course:(not (Course.equal fresh.course world.course) : bool)
        ~same_as_create:
          (World.equal fresh (World.create ~seed:(world.seed + 1)) : bool)];
  show fresh;
  [%expect
    {|
    ((seed 43) (new_course true) (same_as_create true))
    (countdown (crashes 0) (x 100) (y 255))
    |}]
;;

let%expect_test "dead and finished birds don't flap" =
  let world = racing_world () in
  let dead = { world with phase = Dead { time_left = 0.5; died_at = 0. } } in
  let finished = { world with phase = Finished { time = 1. } } in
  print_s
    [%sexp
      (World.equal dead (World.flap dead) : bool)
      , (World.equal finished (World.flap finished) : bool)];
  [%expect {| (true true) |}]
;;
