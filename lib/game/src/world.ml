open! Core

module Phase = struct
  type t =
    | Racing
    | Crashed of { time_left : float }
  [@@deriving sexp_of, equal]
end

type t =
  { bird : Bird.t
  ; phase : Phase.t
  ; crashes : int
  }
[@@deriving sexp_of, equal]

let initial = { bird = Bird.initial; phase = Racing; crashes = 0 }

let flap t =
  match t.phase with
  | Racing -> { t with bird = Bird.flap t.bird }
  | Crashed _ -> t
;;

let crashed (bird : Bird.t) =
  Float.( >= ) (bird.y +. Config.bird_size) Course.ground_top
  || List.exists Course.rects ~f:(fun rect ->
    Course.Rect.hits_bird rect ~bird_x:bird.x ~bird_y:bird.y)
;;

let step t ~dt ~speed_input =
  match t.phase with
  | Crashed { time_left } ->
    let time_left = time_left -. dt in
    if Float.( <= ) time_left 0.
    then { t with bird = Bird.initial; phase = Racing }
    else { t with phase = Crashed { time_left } }
  | Racing ->
    let bird = Bird.step t.bird ~dt ~speed_input in
    if crashed bird
    then
      { bird
      ; phase = Crashed { time_left = Config.crash_pause }
      ; crashes = t.crashes + 1
      }
    else { t with bird }
;;
