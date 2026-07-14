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

type t =
  { bird : Bird.t
  ; phase : Phase.t
  ; invuln_left : float
  ; crashes : int
  ; elapsed : float
  ; seed : int
  ; course : Course.t
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
  }
;;

(* Advancing the seed (rather than randomizing) keeps every race reproducible
   from the seed in the debug overlay while still giving a fresh course per
   race. Stage 5 replaces this with the server's seed. *)
let new_race t = create ~seed:(t.seed + 1)

let flap t =
  match t.phase with
  | Racing -> { t with bird = Bird.flap t.bird }
  | Countdown _ | Dead _ | Finished _ -> t
;;

let hit_ground (bird : Bird.t) =
  Float.( >= ) (bird.y +. Config.bird_size) Course.ground_top
;;

let hit_pipe t (bird : Bird.t) =
  Float.( <= ) t.invuln_left 0.
  && List.exists t.course.rects ~f:(fun rect ->
    Course.Rect.hits_bird rect ~bird_x:bird.x ~bird_y:bird.y)
;;

(* Death: kill any upward velocity so the bird tumbles down from where it was
   hit — the knockdown animation IS the physics (context doc §2). *)
let die t (bird : Bird.t) =
  { t with
    bird = { bird with vy = Float.max bird.vy 0. }
  ; phase = Dead { time_left = Config.respawn_pause; died_at = bird.x }
  ; crashes = t.crashes + 1
  }
;;

let respawn t ~died_at =
  let bird =
    { Bird.initial with
      x = Course.safe_respawn_x t.course ~died_at
    ; y = (Course.ground_top -. Config.bird_size) /. 2.
    }
  in
  { t with bird; phase = Racing; invuln_left = Config.invuln_duration }
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

let step t ~dt ~speed_input =
  match t.phase with
  | Finished _ -> t
  | Countdown { time_left } ->
    let time_left = time_left -. dt in
    if Float.( <= ) time_left 0.
    then { t with phase = Racing }
    else { t with phase = Countdown { time_left } }
  | Dead { time_left; died_at } ->
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
      }
    in
    let bird =
      Bird.step t.bird ~dt ~speed_input ~scheme:Config.control_scheme
    in
    if Float.( >= ) bird.x t.course.finish_x
    then { t with bird; phase = Finished { time = t.elapsed } }
    else if hit_ground bird || hit_pipe t bird
    then die t bird
    else { t with bird }
;;
