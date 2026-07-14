open! Core
open Flappy_game

let racing_world () =
  { (World.create ~seed:Config.debug_seed) with phase = Racing }
;;

let step_seconds ?(speed_input = Bird.Speed_input.Coast) world ~seconds =
  let steps = Float.to_int (Float.round_up (seconds /. Config.sim_dt)) in
  List.fold (List.range 0 steps) ~init:world ~f:(fun world (_ : int) ->
    World.step world ~dt:Config.sim_dt ~speed_input)
;;

(* Like [step_seconds] but flapping whenever the bird starts to fall — keeps
   it bouncing along the ceiling so multi-second tests don't end in an
   accidental ground death. *)
let step_seconds_alive ?(speed_input = Bird.Speed_input.Coast) world ~seconds
  =
  let steps = Float.to_int (Float.round_up (seconds /. Config.sim_dt)) in
  List.fold (List.range 0 steps) ~init:world ~f:(fun world (_ : int) ->
    let world =
      if Float.( > ) world.World.bird.vy 0. then World.flap world else world
    in
    World.step world ~dt:Config.sim_dt ~speed_input)
;;

(* --- Item boxes on the course. --- *)

let%expect_test "boxes live in breather stretches, clear of all pipes" =
  let ok =
    List.for_all (List.range 0 25) ~f:(fun seed ->
      let course = Course.generate ~seed in
      List.for_all course.item_boxes ~f:(fun box ->
        (* No pipe rectangle may overlap a box (a bird-sized probe at the
           box's corner suffices: boxes sit mid-corridor by construction,
           this guards against regressions). *)
        not
          (List.exists course.rects ~f:(fun rect ->
             Course.Rect.hits_bird rect ~bird_x:box.x ~bird_y:box.y))))
  in
  let counts =
    List.map (List.range 0 5) ~f:(fun seed ->
      List.length (Course.generate ~seed).item_boxes)
  in
  print_s [%message (ok : bool) (counts : int list)];
  [%expect {| ((ok true) (counts (8 7 6 7 4))) |}]
;;

let%expect_test "pickup: touch a box empty-handed; one item max" =
  let w = racing_world () in
  let box = List.hd_exn w.course.item_boxes in
  let at_box = { w with bird = { w.bird with x = box.x; y = box.y } } in
  print_s [%sexp (World.touching_unclaimed_box at_box : int option)];
  (* Holding something: boxes are ignored. *)
  let holding = World.receive_pickup at_box Item.Shield in
  print_s [%sexp (World.touching_unclaimed_box holding : int option)];
  (* A second pickup cannot overwrite the held item. *)
  let still = World.receive_pickup holding Item.Swap in
  print_s [%sexp (still.held_item : Item.t option)];
  (* Claimed boxes stop registering. *)
  let claimed = World.box_claimed at_box ~box_id:box.id in
  print_s [%sexp (World.touching_unclaimed_box claimed : int option)];
  [%expect {|
    (0)
    ()
    (Shield)
    ()
    |}]
;;

(* --- Boost. --- *)

let%expect_test "boost: raised cap while active, ramps back down after" =
  let w = { (racing_world ()) with held_item = Some Item.Boost } in
  let w, action = World.use_held_item w in
  print_s
    [%sexp
      (action
       : [ `Applied
         | `Fire_volley of float * float
         | `Request_swap
         | `Nothing
         ])];
  (* Mid-boost (flapping to stay alive): the nitro pushes speed to the raised
     cap on its own. *)
  let mid = step_seconds_alive w ~seconds:2. in
  let r = Float.round_nearest in
  print_s [%message "" ~mid_boost_speed:(r mid.bird.speed : float)];
  (* Well after expiry (coasting, Drift scheme): back down to cruise. *)
  let after = step_seconds_alive mid ~seconds:3. in
  print_s
    [%message
      ""
        ~boost_left:(after.boost_left : float)
        ~speed:(r after.bird.speed : float)];
  [%expect
    {|
    Applied
    (mid_boost_speed 560)
    ((boost_left 0) (speed 260))
    |}]
;;

let%expect_test "boost: brake control is kept during the boost" =
  let w = { (racing_world ()) with held_item = Some Item.Boost } in
  let ( w
      , (_ :
          [ `Applied
          | `Fire_volley of float * float
          | `Request_swap
          | `Nothing
          ]) )
    =
    World.use_held_item w
  in
  let braking = step_seconds w ~speed_input:Brake ~seconds:1. in
  print_s [%sexp (Float.round_nearest braking.bird.speed : float)];
  [%expect {| 190 |}]
;;

(* --- Shield. --- *)

let%expect_test "shield: absorbs one pipe hit, breaks, grants escape \
                 i-frames"
  =
  let w = racing_world () in
  let pipe = List.hd_exn w.course.pipes in
  let inside = { w.bird with x = pipe.x +. 10.; y = 10. } in
  let shielded = { w with bird = inside; shielded = true } in
  let after = World.step shielded ~dt:Config.sim_dt ~speed_input:Coast in
  let phase =
    match after.phase with
    | Racing -> "racing"
    | Dead _ -> "dead"
    | Countdown _ | Finished _ -> "?"
  in
  print_s
    [%message
      phase
        ~shielded:(after.shielded : bool)
        ~invuln:(Float.( > ) after.invuln_left 0. : bool)
        ~crashes:(after.crashes : int)];
  [%expect {| (racing (shielded false) (invuln true) (crashes 0)) |}]
;;

(* --- Volley. --- *)

let%expect_test "volley heights leave bird-sized gaps" =
  let sorted = List.sort Config.volley_heights ~compare:Float.compare in
  let gaps =
    List.map2_exn
      (List.drop_last_exn sorted)
      (List.tl_exn sorted)
      ~f:(fun a b -> b -. a -. (2. *. Config.bullet_radius))
  in
  let bird_fits = List.count gaps ~f:(Float.( < ) Config.bird_size) in
  print_s [%message (List.length gaps : int) (bird_fits >= 2 : bool)];
  [%expect {| (("List.length gaps" 4) ("bird_fits >= 2" true)) |}]
;;

let%expect_test "hostile volley: knockdown (no flaps) then the ground kills" =
  let w = racing_world () in
  (* Fire from just behind my bird, level with one of the bullet bands. *)
  let bird =
    { w.bird with x = 2000.; y = 240. -. (Config.bird_size /. 2.) }
  in
  let w = { w with bird } in
  let w = World.receive_volley w ~x:(bird.x -. 200.) ~hostile:true in
  print_s [%message "" ~bullets:(List.length w.bullets : int)];
  (* Bullets fly at 756 px/s from ~170px behind the bird's face: hit lands
     within ~0.3s. Flapping madly must NOT save the stunned bird. *)
  let rec flap_til_dead w ~steps_left =
    match (w.World.phase : World.Phase.t) with
    | Dead _ -> w, true
    | Countdown _ | Racing | Finished _ ->
      if steps_left = 0
      then w, false
      else (
        let w = World.flap w in
        let w = World.step w ~dt:Config.sim_dt ~speed_input:Coast in
        flap_til_dead w ~steps_left:(steps_left - 1))
  in
  let dead, died = flap_til_dead w ~steps_left:240 in
  print_s
    [%message
      "" ~died:(died : bool) ~stunned_cleared:(not dead.stunned : bool)];
  [%expect
    {|
    (bullets 5)
    ((died true) (stunned_cleared true))
    |}]
;;

let%expect_test "my own volley never hits me" =
  let w = racing_world () in
  let bird =
    { w.bird with x = 2000.; y = 240. -. (Config.bird_size /. 2.) }
  in
  let w = { w with bird } in
  (* Same geometry as above but the volley is MINE (display only). *)
  let w = World.receive_volley w ~x:(bird.x -. 200.) ~hostile:false in
  let later = step_seconds_alive w ~seconds:0.6 in
  let racing =
    match later.phase with
    | Racing -> true
    | Countdown _ | Dead _ | Finished _ -> false
  in
  print_s
    [%message
      "" ~still_racing:(racing : bool) ~stunned:(later.stunned : bool)];
  [%expect {| ((still_racing true) (stunned false)) |}]
;;

let%expect_test "shield stops a bullet" =
  let w = racing_world () in
  let bird =
    { w.bird with x = 2000.; y = 240. -. (Config.bird_size /. 2.) }
  in
  let w = { w with bird; shielded = true } in
  let w = World.receive_volley w ~x:(bird.x -. 200.) ~hostile:true in
  let later = step_seconds w ~seconds:0.6 in
  print_s
    [%message
      ""
        ~shield_spent:(not later.shielded : bool)
        ~stunned:(later.stunned : bool)];
  [%expect {| ((shield_spent true) (stunned false)) |}]
;;

(* --- Swap. --- *)

let%expect_test "swap teleports with i-frames; into-geometry snaps safe" =
  let w = racing_world () in
  (* Clean destination: mid-air between pipes. *)
  (match World.receive_swap w ~other:(3000., 200.) with
   | `Blocked (_ : World.t) -> print_s [%sexp "blocked?!"]
   | `Swapped s ->
     print_s
       [%message
         ""
           ~x:(s.bird.x : float)
           ~y:(s.bird.y : float)
           ~invuln:(Float.( > ) s.invuln_left 0. : bool)]);
  (* Destination inside the first pipe: must snap to a safe x. *)
  let pipe = List.hd_exn w.course.pipes in
  (match World.receive_swap w ~other:(pipe.x +. 5., 5.) with
   | `Blocked (_ : World.t) -> print_s [%sexp "blocked?!"]
   | `Swapped s ->
     let clear =
       not
         (List.exists w.course.rects ~f:(fun rect ->
            Course.Rect.hits_bird rect ~bird_x:s.bird.x ~bird_y:s.bird.y))
     in
     print_s [%message "" ~snapped_clear:(clear : bool)]);
  [%expect
    {|
    ((x 3000) (y 200) (invuln true))
    (snapped_clear true)
    |}]
;;

let%expect_test "shield blocks a swap; initiator reverts on Swap_blocked" =
  let w = { (racing_world ()) with shielded = true } in
  (match World.receive_swap w ~other:(3000., 200.) with
   | `Swapped (_ : World.t) -> print_s [%sexp "swapped?!"]
   | `Blocked b ->
     print_s
       [%message
         ""
           ~moved:(not (Float.equal b.bird.x w.bird.x) : bool)
           ~shield_spent:(not b.shielded : bool)]);
  (* Initiator: swap then blocked -> back to the original spot. *)
  let init = racing_world () in
  (match World.receive_swap init ~other:(3000., 200.) with
   | `Blocked (_ : World.t) -> print_s [%sexp "blocked?!"]
   | `Swapped moved ->
     let back = World.receive_swap_blocked moved in
     print_s
       [%message
         ""
           ~reverted:
             (Float.equal back.bird.x init.bird.x
              && Float.equal back.bird.y init.bird.y
              : bool)]);
  [%expect
    {|
    ((moved false) (shield_spent true))
    (reverted true)
    |}]
;;

let%expect_test "using each item empties the slot and reports the right \
                 action"
  =
  let w = racing_world () in
  let show item =
    let w, action = World.use_held_item { w with held_item = Some item } in
    let action =
      match action with
      | `Applied -> "applied locally"
      | `Fire_volley (_, _) -> "fire_volley -> server"
      | `Request_swap -> "swap -> server"
      | `Nothing -> "nothing"
    in
    print_s
      [%message
        (Item.to_string item) action ~slot:(w.held_item : Item.t option)]
  in
  List.iter Item.all ~f:show;
  [%expect
    {|
    (BOOST "applied locally" (slot ()))
    (SHIELD "applied locally" (slot ()))
    (VOLLEY "fire_volley -> server" (slot ()))
    (SWAP "swap -> server" (slot ()))
    |}]
;;
