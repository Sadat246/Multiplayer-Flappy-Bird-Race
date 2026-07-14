open! Core

type t =
  | Boost
  | Shield
  | Volley
  | Swap
[@@deriving bin_io, sexp_of, equal, enumerate]

let tag = function
  | Boost -> "B"
  | Shield -> "S"
  | Volley -> "V"
  | Swap -> "W"
;;

let to_string = function
  | Boost -> "BOOST"
  | Shield -> "SHIELD"
  | Volley -> "VOLLEY"
  | Swap -> "SWAP"
;;
