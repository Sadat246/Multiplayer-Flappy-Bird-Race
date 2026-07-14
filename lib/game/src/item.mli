(** The four power-ups (context doc §3, final list). Lives in the game
    library (with [bin_io] so the protocol can carry it): which item a player
    holds is game state; which player got a contested box is the server's
    call. *)

open! Core

type t =
  | Boost (** temporarily raises the speed cap — nitro *)
  | Shield (** blocks exactly one hit of any kind, then breaks *)
  | Volley
  (** 5 bullets forward at fixed heights; pass through pipes; knock any
      player hit to the ground. The chaser's weapon. *)
  | Swap (** instantly trade positions with the opponent — chaos item *)
[@@deriving bin_io, sexp_of, equal, enumerate]

(** One-letter tag for HUD slots ("B", "S", "V", "W"). *)
val tag : t -> string

val to_string : t -> string
