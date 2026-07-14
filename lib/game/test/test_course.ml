open! Core
open Flappy_game

let seeds = List.range 0 25

let%expect_test "generation is deterministic: same seed, same course" =
  let all_equal =
    List.for_all seeds ~f:(fun seed ->
      Course.equal (Course.generate ~seed) (Course.generate ~seed))
  in
  print_s [%sexp (all_equal : bool)];
  [%expect {| true |}]
;;

let%expect_test "the debug course, so tuning diffs are visible" =
  let course = Course.generate ~seed:Config.debug_seed in
  print_s [%sexp (List.take course.pipes 6 : Course.Pipe.t list)];
  print_s [%message (course.finish_x : float)];
  [%expect
    {|
    (((x 900) (gap_center 240))
     ((x 1513.7725271926724) (gap_center 137.498581320959))
     ((x 2391.6389656826955) (gap_center 228.54554940852316))
     ((x 3358.0424510851171) (gap_center 162.55278395775343))
     ((x 4258.0787403597278) (gap_center 320.8972771003742))
     ((x 4838.114124711451) (gap_center 278.56081521318731)))
    (course.finish_x 22592.563423708641)
    |}]
;;

let spacings (course : Course.t) =
  match course.pipes with
  | [] -> []
  | _ :: tail ->
    List.map2_exn
      (List.drop_last_exn course.pipes)
      tail
      ~f:(fun (a : Course.Pipe.t) (b : Course.Pipe.t) -> b.x -. a.x)
;;

let%expect_test "spacing never drops below the baseline minimum" =
  (* The user's difficulty rule: variable spacing may only ADD room, never
     tighten below the baseline. *)
  let ok =
    List.for_all seeds ~f:(fun seed ->
      List.for_all
        (spacings (Course.generate ~seed))
        ~f:(fun s -> Float.( >= ) s Config.spacing_normal_min))
  in
  print_s [%sexp (ok : bool)];
  [%expect {| true |}]
;;

let%expect_test "breather sections exist: some gaps are much wider" =
  (* Statistical but deterministic (fixed seeds): across 25 courses, plenty
     of spacings should land in the breather range. *)
  let all =
    List.concat_map seeds ~f:(fun seed -> spacings (Course.generate ~seed))
  in
  let breathers =
    List.count all ~f:(fun s -> Float.( >= ) s Config.spacing_breather_min)
  in
  let total = List.length all in
  print_s [%message (total : int) (breathers > total / 6 : bool)];
  [%expect {| ((total 725) ("breathers > (total / 6)" true)) |}]
;;

let%expect_test "fairness: gap-center jumps stay within the full-speed bound"
  =
  (* Consecutive gap centers may differ by at most what a bird at speed_cap
     can climb in that spacing WITHOUT the safety margin — i.e. the
     generator's margined bound holds with real room to spare. *)
  let ok =
    List.for_all seeds ~f:(fun seed ->
      let course = Course.generate ~seed in
      match course.pipes with
      | [] -> false
      | _ :: tail ->
        List.for_all2_exn
          (List.drop_last_exn course.pipes)
          tail
          ~f:(fun (a : Course.Pipe.t) (b : Course.Pipe.t) ->
            let physically_achievable =
              (b.x -. a.x) /. Config.speed_cap *. Config.climb_rate
            in
            Float.( <= )
              (Float.abs (b.gap_center -. a.gap_center))
              physically_achievable))
  in
  print_s [%sexp (ok : bool)];
  [%expect {| true |}]
;;

let%expect_test "every gap stays fully on the playfield" =
  let ok =
    List.for_all seeds ~f:(fun seed ->
      let course = Course.generate ~seed in
      List.for_all course.pipes ~f:(fun { Course.Pipe.x = _; gap_center } ->
        let half = Config.pipe_gap /. 2. in
        Float.( >= ) (gap_center -. half) 0.
        && Float.( <= ) (gap_center +. half) Course.ground_top))
  in
  print_s [%sexp (ok : bool)];
  [%expect {| true |}]
;;

let%expect_test "safe respawn: never inside geometry, for any death point" =
  (* Sweep death positions across each course in 50px increments and check
     the respawned bird (mid-height at the snapped x) clears every pipe
     rectangle. This is the "never respawn inside geometry" spec. *)
  let bird_y = (Course.ground_top -. Config.bird_size) /. 2. in
  let ok =
    List.for_all seeds ~f:(fun seed ->
      let course = Course.generate ~seed in
      let max_x = Float.to_int course.finish_x in
      List.for_all (List.range 0 max_x ~stride:50) ~f:(fun died_at ->
        let x =
          Course.safe_respawn_x course ~died_at:(Float.of_int died_at)
        in
        not
          (List.exists course.rects ~f:(fun rect ->
             Course.Rect.hits_bird rect ~bird_x:x ~bird_y))))
  in
  print_s [%sexp (ok : bool)];
  [%expect {| true |}]
;;
