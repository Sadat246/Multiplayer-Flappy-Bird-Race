open! Core

module Rect = struct
  type t =
    { x : float
    ; y : float
    ; w : float
    ; h : float
    }
  [@@deriving sexp_of, equal]

  let hits_bird t ~bird_x ~bird_y =
    let s = Config.bird_size in
    Float.( < ) bird_x (t.x +. t.w)
    && Float.( > ) (bird_x +. s) t.x
    && Float.( < ) bird_y (t.y +. t.h)
    && Float.( > ) (bird_y +. s) t.y
  ;;
end

let ground_top = Config.canvas_height -. Config.ground_height

(* A pipe pair: solid from the ceiling down to the gap, and from the gap down
   to the ground. [gap_center] is the world y of the gap's middle. *)
let pipe_pair ~x ~gap_center : Rect.t list =
  let half = Config.pipe_gap /. 2. in
  let gap_top = gap_center -. half in
  let gap_bottom = gap_center +. half in
  [ { x; y = 0.; w = Config.pipe_width; h = gap_top }
  ; { x
    ; y = gap_bottom
    ; w = Config.pipe_width
    ; h = ground_top -. gap_bottom
    }
  ]
;;

(* Hardcoded course for the "square avoiding squares" stage. Gap centers
   wander gently so the course is comfortably flyable while feel-tuning;
   Stage 3 derives placements from the seed with a fairness bound instead. *)
let rects =
  List.concat_map
    ~f:(fun (x, gap_center) -> pipe_pair ~x ~gap_center)
    [ 700., 270.
    ; 1250., 210.
    ; 1800., 320.
    ; 2350., 240.
    ; 2900., 300.
    ; 3450., 200.
    ; 4000., 330.
    ; 4550., 250.
    ]
;;
