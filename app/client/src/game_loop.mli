(** The imperative edge of the client: requestAnimationFrame loop, keyboard
    listeners, and canvas rendering.

    All game *logic* lives in the pure {!Flappy_game.World}; this module only
    feeds it fixed timesteps and key state, and draws the result. It
    deliberately lives outside Bonsai's incremental graph — 60 Hz physics
    through Bonsai state is this stack's anti-goal (see build plan). *)

open! Core

(** DOM id of the canvas {!View} renders and this module draws on. *)
val canvas_id : string

(** Install keyboard handlers and start the frame loop. Safe to call
    immediately after [Bonsai_web.Start.start]: frames before the canvas
    exists in the DOM just skip drawing. Never returns an error and never
    raises; call it exactly once. *)
val start : unit -> unit
