open! Core
open Async_rpc_kernel
module Item = Flappy_game.Item

module Player_id = struct
  type t =
    | P1
    | P2
  [@@deriving bin_io, sexp_of, equal, enumerate]

  let other = function P1 -> P2 | P2 -> P1
  let index = function P1 -> 0 | P2 -> 1
end

module Pos = struct
  type t =
    { x : float
    ; y : float
    }
  [@@deriving bin_io, sexp_of, equal]
end

module Update = struct
  type t =
    { player : Player_id.t
    ; pos : Pos.t
    ; last_seen_event : int
    }
  [@@deriving bin_io, sexp_of]
end

module Event = struct
  type t =
    | Powerup_claimed of
        { box_id : int
        ; by : Player_id.t
        ; item : Item.t
        }
    | Volley_fired of
        { by : Player_id.t
        ; x : float
        ; y : float
        }
    | Swapped of
        { p1 : Pos.t
        ; p2 : Pos.t
        }
    | Swap_blocked
  [@@deriving bin_io, sexp_of, equal]
end

module Stamped_event = struct
  type t =
    { seq : int
    ; race_seed : int
    ; event : Event.t
    }
  [@@deriving bin_io, sexp_of]
end

module Race_state = struct
  type t =
    | Waiting_for_players
    | Ready_to_start
    | Race of { seed : int }
  [@@deriving bin_io, sexp_of, equal]
end

module View = struct
  type t =
    { race : Race_state.t
    ; opponent : Pos.t option
    ; events : Stamped_event.t list
    }
  [@@deriving bin_io, sexp_of]
end

module Use = struct
  type t =
    | Fire_volley of
        { x : float
        ; y : float
        }
    | Swap
    | Swap_blocked
  [@@deriving bin_io, sexp_of]
end

let join_rpc =
  Rpc.Rpc.create
    ~name:"join"
    ~version:1
    ~bin_query:String.bin_t
    ~bin_response:[%bin_type_class: Player_id.t Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

let sync_rpc =
  Rpc.Rpc.create
    ~name:"sync"
    ~version:3
    ~bin_query:Update.bin_t
    ~bin_response:[%bin_type_class: View.t Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

let pickup_request_rpc =
  Rpc.Rpc.create
    ~name:"pickup-request"
    ~version:1
    ~bin_query:[%bin_type_class: Player_id.t * int]
    ~bin_response:[%bin_type_class: Item.t option Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

let use_powerup_rpc =
  Rpc.Rpc.create
    ~name:"use-powerup"
    ~version:1
    ~bin_query:[%bin_type_class: Player_id.t * Use.t]
    ~bin_response:[%bin_type_class: unit Or_error.t]
    ~include_in_error_count:Only_on_exn
;;

let new_race_rpc =
  Rpc.Rpc.create
    ~name:"new-race"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:[%bin_type_class: unit Or_error.t]
    ~include_in_error_count:Only_on_exn
;;
