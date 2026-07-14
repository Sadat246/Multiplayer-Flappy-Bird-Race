(** The obstacle course: for this stage, a hardcoded list of static
    rectangles shaped like pipe pairs. Stage 3 replaces the hardcoded list
    with seeded generation behind the same interface — nothing downstream
    should care where the rectangles came from. *)

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

(** World y of the ground's top edge. At or below it = crash. *)
val ground_top : float

(** All pipe rectangles, in course order. *)
val rects : Rect.t list
