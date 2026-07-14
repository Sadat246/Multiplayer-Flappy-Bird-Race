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

module Pipe = struct
  type t =
    { x : float
    ; gap_center : float
    }
  [@@deriving sexp_of, equal]
end

type t =
  { pipes : Pipe.t list
  ; rects : Rect.t list
  ; finish_x : float
  }
[@@deriving sexp_of, equal]

let ground_top = Config.canvas_height -. Config.ground_height

(* Gap centers must keep the whole gap on the playfield, with a margin. *)
let gap_center_lo = Config.gap_margin +. (Config.pipe_gap /. 2.)
let gap_center_hi = ground_top -. Config.gap_margin -. (Config.pipe_gap /. 2.)
let uniform state ~lo ~hi = lo +. Random.State.float state (hi -. lo)

(* Baseline spacing is the floor — never tighter. Breathers are the wider,
   easier stretches the design asks for (and where item boxes will prefer to
   spawn in Stage 6). *)
let next_spacing state =
  if Float.( < ) (Random.State.float state 1.) Config.breather_probability
  then
    uniform
      state
      ~lo:Config.spacing_breather_min
      ~hi:Config.spacing_breather_max
  else
    uniform state ~lo:Config.spacing_normal_min ~hi:Config.spacing_normal_max
;;

(* The fairness rule: crossing [spacing] px at full speed takes
   [spacing / speed_cap] seconds, in which a bird can climb (or dive) at
   least [climb_rate] px/s; the margin keeps the bound comfortable. Wider
   spacing honestly buys larger allowed jumps. *)
let max_gap_delta ~spacing =
  spacing /. Config.speed_cap *. Config.climb_rate *. Config.fairness_margin
;;

let pipe_rects { Pipe.x; gap_center } : Rect.t list =
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

let generate ~seed =
  let state = Random.State.make [| seed |] in
  let first : Pipe.t =
    { x = Config.first_pipe_x
    ; gap_center = (gap_center_lo +. gap_center_hi) /. 2.
    }
  in
  let rest =
    List.folding_map
      (List.range 1 Config.course_pipes)
      ~init:first
      ~f:(fun prev (_ : int) ->
        let spacing = next_spacing state in
        let delta = max_gap_delta ~spacing in
        (* Draw uniformly from the intersection of "reachable from the
           previous gap at full speed" and "fully on the playfield".
           (Clamping a wider draw instead piles gap centers onto the band's
           edges — found by the debug-course expect test.) *)
        let gap_center =
          uniform
            state
            ~lo:(Float.max gap_center_lo (prev.gap_center -. delta))
            ~hi:(Float.min gap_center_hi (prev.gap_center +. delta))
        in
        let pipe : Pipe.t = { x = prev.x +. spacing; gap_center } in
        pipe, pipe)
  in
  let pipes = first :: rest in
  let last = List.last_exn pipes in
  { pipes
  ; rects = List.concat_map pipes ~f:pipe_rects
  ; finish_x = last.x +. Config.pipe_width +. Config.finish_after_last_pipe
  }
;;

let safe_respawn_x t ~died_at =
  (* Every candidate is horizontally clear of all pipes by construction: the
     runway start, midpoints of pipe-to-pipe gaps (at least
     (spacing_normal_min - pipe_width) / 2 - bird_size/2 ~ 220px of
     clearance), and just past the last pipe. *)
  let midpoints =
    match t.pipes with
    | [] -> []
    | _ :: tail ->
      List.map2_exn
        (List.drop_last_exn t.pipes)
        tail
        ~f:(fun (a : Pipe.t) (b : Pipe.t) ->
          (a.x +. Config.pipe_width +. b.x) /. 2.)
  in
  let after_last =
    match List.last t.pipes with
    | None -> Config.bird_start_x
    | Some last -> last.x +. Config.pipe_width +. 200.
  in
  let candidates = (Config.bird_start_x :: midpoints) @ [ after_last ] in
  List.min_elt
    candidates
    ~compare:
      (Comparable.lift Float.compare ~f:(fun c -> Float.abs (c -. died_at)))
  |> Option.value ~default:Config.bird_start_x
;;
