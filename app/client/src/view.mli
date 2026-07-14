(** The Bonsai layer: page shell around the game canvas.

    Deliberately thin — the 60 Hz game world is drawn imperatively on the
    canvas by {!Game_loop}, outside Bonsai's incremental graph (build-plan
    "known risk" section). Bonsai owns everything around it: title, help
    line, and later the lobby, HUD and result screens. *)

open! Core
open! Bonsai_web

(** The whole page. Passed to [Bonsai_web.Start.start] by the entry point.
    Renders the [<canvas>] that {!Game_loop.start} looks up by id. *)
val app : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
