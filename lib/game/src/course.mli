(** Seeded course generation: a finite run of pipe pairs ending at a finish
    line, generated deterministically from an integer seed.

    Determinism is the multiplayer foundation (context doc §1): in Stage 4
    the server sends both clients one seed and each generates this identical
    course locally. OCaml's [Random.State] is pure OCaml, so js_of_ocaml
    (client) and native (tests, and both players' machines) produce
    byte-identical courses from the same seed.

    Two placement rules:

    - {b Fairness}: each gap's vertical center differs from the previous one
      by at most what a bird flying at FULL speed can climb in the horizontal
      distance between them ({!Config.climb_rate} scaled by
      {!Config.fairness_margin}). Braking is therefore pure assist — no
      section is impossible at [speed_cap].
    - {b Spacing}: never tighter than {!Config.spacing_normal_min} (the
      baseline difficulty); with {!Config.breather_probability} a spacing is
      drawn from the much wider breather range instead — deliberate recovery
      stretches, and the natural home for Stage 6's item boxes. *)

open! Core

module Rect : sig
  (** Axis-aligned rectangle, top-left + extent, world coordinates. *)
  type t =
    { x : float
    ; y : float
    ; w : float
    ; h : float
    }
  [@@deriving sexp_of, equal]

  (** Does the bird's square (top-left [x, y], side {!Config.bird_size})
      overlap this rectangle? *)
  val hits_bird : t -> bird_x:float -> bird_y:float -> bool
end

module Pipe : sig
  (** One pipe pair: a solid column at [x] with a gap of {!Config.pipe_gap}
      centered on [gap_center]. *)
  type t =
    { x : float
    ; gap_center : float
    }
  [@@deriving sexp_of, equal]
end

type t =
  { pipes : Pipe.t list (** in course order *)
  ; rects : Rect.t list (** the pipes expanded to collision rectangles *)
  ; finish_x : float (** cross it (bird's left edge) to finish *)
  }
[@@deriving sexp_of, equal]

(** World y of the ground's top edge. At or below it = crash. *)
val ground_top : float

val generate : seed:int -> t

(** Where to respawn a bird that died at [died_at]: the nearest x that is
    guaranteed clear of every pipe — the runway before the first pipe, the
    midpoint between two consecutive pipes, or just past the last pipe. Pair
    it with mid-height and the bird can never respawn inside geometry. *)
val safe_respawn_x : t -> died_at:float -> float
