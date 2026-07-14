open! Core

module Phase = struct
  type t =
    | Countdown of { time_left : float }
    | Racing
    | Dead of
        { time_left : float
        ; died_at : float
        }
    | Finished of { time : float }
  [@@deriving sexp_of, equal]
end

module Bullet = struct
  type t =
    { x : float
    ; y : float
    ; hostile : bool
    ; traveled : float
    }
  [@@deriving sexp_of, equal]
end

type t =
  { bird : Bird.t
  ; phase : Phase.t
  ; invuln_left : float
  ; crashes : int
  ; elapsed : float
  ; seed : int
  ; course : Course.t
  ; held_item : Item.t option
  ; boost_left : float
  ; shielded : bool
  ; stunned : bool
  ; swap_revert : (float * float) option
  ; bullets : Bullet.t list
  ; boxes_taken : Int.Set.t
  }
[@@deriving sexp_of, equal]

let create ~seed =
  { bird = Bird.initial
  ; phase = Countdown { time_left = Config.countdown_duration }
  ; invuln_left = 0.
  ; crashes = 0
  ; elapsed = 0.
  ; seed
  ; course = Course.generate ~seed
  ; held_item = None
  ; boost_left = 0.
  ; shielded = false
  ; stunned = false
  ; swap_revert = None
  ; bullets = []
  ; boxes_taken = Int.Set.empty
  }
;;

(* Advancing the seed (rather than randomizing) keeps every race reproducible
   from the seed in the debug overlay while still giving a fresh course per
   race. In multiplayer the server's seed wins. *)
let new_race t = create ~seed:(t.seed + 1)

let flap t =
  match t.phase with
  | Racing when not t.stunned -> { t with bird = Bird.flap t.bird }
  | Racing | Countdown _ | Dead _ | Finished _ -> t
;;

let mid_height = (Course.ground_top -. Config.bird_size) /. 2.

let hit_ground (bird : Bird.t) =
  Float.( >= ) (bird.y +. Config.bird_size) Course.ground_top
;;

let hit_pipe t (bird : Bird.t) =
  Float.( <= ) t.invuln_left 0.
  && List.exists t.course.rects ~f:(fun rect ->
    Course.Rect.hits_bird rect ~bird_x:bird.x ~bird_y:bird.y)
;;

(* Death: kill any upward velocity so the bird tumbles down from where it was
   hit — the knockdown animation IS the physics (context doc §2). An active
   shield absorbs the hit instead: it breaks, grants brief i-frames to escape
   whatever we're inside of, and bounces the bird up a touch so a ground hit
   doesn't instantly recur. *)
let die t (bird : Bird.t) =
  if t.shielded
  then
    { t with
      bird = { bird with vy = Config.flap_impulse /. 2. }
    ; shielded = false
    ; invuln_left = Config.shield_break_invuln
    }
  else
    { t with
      bird = { bird with vy = Float.max bird.vy 0. }
    ; phase = Dead { time_left = Config.respawn_pause; died_at = bird.x }
    ; crashes = t.crashes + 1
    ; stunned = false
    ; boost_left = 0. (* dying wastes the nitro; it doesn't resume *)
    }
;;

let respawn t ~died_at =
  let bird =
    { Bird.initial with
      x = Course.safe_respawn_x t.course ~died_at
    ; y = mid_height
    }
  in
  { t with
    bird
  ; phase = Racing
  ; invuln_left = Config.invuln_duration
  ; stunned = false
  }
;;

(* The visible tumble: gravity only, x frozen, resting on the ground. *)
let tumble (bird : Bird.t) ~dt : Bird.t =
  let vy =
    Float.min (bird.vy +. (Config.gravity *. dt)) Config.terminal_velocity
  in
  let y =
    Float.min (bird.y +. (vy *. dt)) (Course.ground_top -. Config.bird_size)
  in
  { bird with y; vy }
;;

(* --- Bullets: straight lines, through pipes, only hostile ones can touch my
   bird (client-owns-own-fate: the opponent's own world handles my volley
   hitting them). --- *)

let step_bullets t ~dt =
  let step_one (b : Bullet.t) =
    let dx = Config.bullet_speed *. dt in
    { b with x = b.x +. dx; traveled = b.traveled +. dx }
  in
  let alive (b : Bullet.t) =
    Float.( < ) b.traveled Config.bullet_max_range
  in
  { t with
    bullets =
      List.filter_map t.bullets ~f:(fun b ->
        let b = step_one b in
        Option.some_if (alive b) b)
  }
;;

let bullet_hits_bird (b : Bullet.t) (bird : Bird.t) =
  let r = Config.bullet_radius in
  let s = Config.bird_size in
  Float.( < ) (b.x -. r) (bird.x +. s)
  && Float.( > ) (b.x +. r) bird.x
  && Float.( < ) (b.y -. r) (bird.y +. s)
  && Float.( > ) (b.y +. r) bird.y
;;

(* A hostile bullet connecting: shield absorbs it (bullet gone, shield gone,
   brief i-frames); otherwise the knockdown — flaps cut, vertical velocity
   slammed downward, and the regular ground collision finishes it (context
   doc §2: no special death code for bullets). *)
let resolve_bullet_hits t =
  match t.phase with
  | Countdown _ | Dead _ | Finished _ -> t
  | Racing ->
    let vulnerable = Float.( <= ) t.invuln_left 0. in
    let hit, missed =
      List.partition_tf t.bullets ~f:(fun b ->
        b.hostile && vulnerable && bullet_hits_bird b t.bird)
    in
    (match hit with
     | [] -> t
     | _ :: _ when t.shielded ->
       { t with
         bullets = missed
       ; shielded = false
       ; invuln_left = Config.shield_break_invuln
       }
     | _ :: _ ->
       { t with
         bullets = missed
       ; stunned = true
       ; bird = { t.bird with vy = Config.terminal_velocity }
       })
;;

let step t ~dt ~speed_input =
  match t.phase with
  | Finished _ -> t
  | Countdown { time_left } ->
    let time_left = time_left -. dt in
    if Float.( <= ) time_left 0.
    then { t with phase = Racing }
    else { t with phase = Countdown { time_left } }
  | Dead { time_left; died_at } ->
    let t = step_bullets t ~dt in
    let t = { t with elapsed = t.elapsed +. dt } in
    let time_left = time_left -. dt in
    if Float.( <= ) time_left 0.
    then respawn t ~died_at
    else
      { t with
        bird = tumble t.bird ~dt
      ; phase = Dead { time_left; died_at }
      }
  | Racing ->
    let t =
      { t with
        elapsed = t.elapsed +. dt
      ; invuln_left = Float.max 0. (t.invuln_left -. dt)
      ; boost_left = Float.max 0. (t.boost_left -. dt)
      }
    in
    let boosting = Float.( > ) t.boost_left 0. in
    (* Nitro: the cap rises AND the bird pushes toward it on its own; the
       player keeps brake control during it (context doc §3). *)
    let speed_cap =
      if boosting then Config.boost_speed_cap else Config.speed_cap
    in
    let speed_input : Bird.Speed_input.t =
      match (speed_input : Bird.Speed_input.t) with
      | Brake -> Brake
      | Accelerate -> Accelerate
      | Coast -> if boosting then Accelerate else Coast
    in
    let bird =
      Bird.step
        t.bird
        ~dt
        ~speed_input
        ~scheme:Config.control_scheme
        ~speed_cap
    in
    let t = step_bullets { t with bird } ~dt in
    let t = resolve_bullet_hits t in
    if Float.( >= ) t.bird.x t.course.finish_x
    then { t with phase = Finished { time = t.elapsed } }
    else if hit_ground t.bird || hit_pipe t t.bird
    then die t t.bird
    else t
;;

(* --- Item boxes. --- *)

let touching_unclaimed_box t =
  match t.phase, t.held_item with
  | Racing, None ->
    List.find_map t.course.item_boxes ~f:(fun box ->
      if (not (Set.mem t.boxes_taken box.id))
         && Course.Item_box.touches box ~bird_x:t.bird.x ~bird_y:t.bird.y
      then Some box.id
      else None)
  | (Racing | Countdown _ | Dead _ | Finished _), _ -> None
;;

let receive_pickup t item =
  match t.held_item with
  | Some (_ : Item.t) -> t (* one item max; shouldn't happen *)
  | None -> { t with held_item = Some item }
;;

let box_claimed t ~box_id =
  { t with boxes_taken = Set.add t.boxes_taken box_id }
;;

(* --- Using items. --- *)

let use_held_item t =
  match t.phase, t.held_item with
  | Racing, Some Boost ->
    { t with held_item = None; boost_left = Config.boost_duration }, `Applied
  | Racing, Some Shield ->
    { t with held_item = None; shielded = true }, `Applied
  | Racing, Some Volley ->
    { t with held_item = None }, `Fire_volley (t.bird.x, t.bird.y)
  | Racing, Some Swap -> { t with held_item = None }, `Request_swap
  | (Racing | Countdown _ | Dead _ | Finished _), _ -> t, `Nothing
;;

let receive_volley t ~x ~hostile =
  let bullets =
    List.map Config.volley_heights ~f:(fun y ->
      { Bullet.x = x +. Config.bird_size; y; hostile; traveled = 0. })
  in
  { t with bullets = bullets @ t.bullets }
;;

(* --- Swaps. --- *)

let inside_geometry t ~x ~y =
  List.exists t.course.rects ~f:(fun rect ->
    Course.Rect.hits_bird rect ~bird_x:x ~bird_y:y)
;;

let teleport t ~x ~y =
  (* Someone may materialize inside or adjacent to a pipe: i-frames plus
     snap-to-nearest-safe-x, same logic as respawn (context doc §3). *)
  let x, y =
    if inside_geometry t ~x ~y
    then Course.safe_respawn_x t.course ~died_at:x, mid_height
    else x, y
  in
  { t with
    bird = { t.bird with x; y }
  ; invuln_left = Config.invuln_duration
  }
;;

let receive_swap t ~other:(ox, oy) =
  match t.phase with
  | Countdown _ | Dead _ | Finished _ -> `Swapped t (* nothing to move *)
  | Racing ->
    if t.shielded
    then `Blocked { t with shielded = false }
    else (
      let revert = t.bird.x, t.bird.y in
      `Swapped { (teleport t ~x:ox ~y:oy) with swap_revert = Some revert })
;;

let receive_swap_blocked t =
  match t.swap_revert with
  | None -> t
  | Some (x, y) -> { (teleport t ~x ~y) with swap_revert = None }
;;
