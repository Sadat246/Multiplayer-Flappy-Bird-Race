(** Native OCaml Graphics client — the X11-forwarding alternative to the
    browser client for playing from two devices without any AWS or
    third-party setup.

    Both players [ssh -X] (or [-Y]) into the EC2 box and run this binary;
    each gets a game window forwarded to their laptop's X server (XQuartz on
    macOS). The window processes and the referee server all live on the same
    machine, talking over localhost — the network only ever carries window
    pixels, through the SSH connection that already exists.

    Same architecture as the browser client: {!Flappy_game.World} is the
    whole simulation, {!Flappy_protocol} RPCs carry positions at
    {!Flappy_game.Config.sync_hz}, the server stays a referee.

    Controls differ from the browser out of necessity: the [Graphics] library
    only reports key PRESSES (characters), never releases, so "hold to
    accelerate" is emulated via X key auto-repeat — a key counts as held
    while its repeats keep arriving. Arrow keys produce no characters at all,
    hence letters:

    - SPACE flap · D (hold) accelerate · A (hold) brake
    - R new race for both players · Q quit *)

open! Core
open! Async
module Protocol = Flappy_protocol
module Config = Flappy_game.Config
module World = Flappy_game.World
module Course = Flappy_game.Course
module Bird = Flappy_game.Bird

(* --- Shared mutable state between the net loop and the frame loop. --- *)

let my_pos = ref { Protocol.Pos.x = 0.; y = 0. }
let opponent : Protocol.Pos.t option ref = ref None
let seed : int option ref = ref None
let net_status = ref "connecting..."
let world : World.t option ref = ref None
let current_seed : int option ref = ref None
let ghost : Protocol.Pos.t option ref = ref None

(* --- Input: Graphics gives key presses only, no releases. A movement key
   counts as "held" while X auto-repeat keeps delivering it (initial repeat
   delay ~0.5s, then ~30 Hz — [held_window] must cover the former). Flaps are
   the opposite: auto-repeat must NOT flap, so a space only flaps if the
   previous one was a beat ago. --- *)

let held_window = Time_ns.Span.of_sec 0.6
let flap_debounce = Time_ns.Span.of_sec 0.1
let last_accel = ref Time_ns.epoch
let last_brake = ref Time_ns.epoch
let last_space = ref Time_ns.epoch

let speed_input () : Bird.Speed_input.t =
  let now = Time_ns.now () in
  let held at = Time_ns.Span.( <= ) (Time_ns.diff now at) held_window in
  match held !last_accel, held !last_brake with
  | true, false -> Accelerate
  | false, true -> Brake
  | _ -> Coast
;;

let request_new_race = ref (fun () -> ())

let drain_keys () =
  while Graphics.key_pressed () do
    let now = Time_ns.now () in
    match Graphics.read_key () with
    | ' ' ->
      let genuine_press =
        Time_ns.Span.( > ) (Time_ns.diff now !last_space) flap_debounce
      in
      last_space := now;
      if genuine_press
      then Option.iter !world ~f:(fun w -> world := Some (World.flap w))
    | 'd' | 'D' -> last_accel := now
    | 'a' | 'A' -> last_brake := now
    | 'r' | 'R' -> !request_new_race ()
    | 'q' | 'Q' -> shutdown 0
    | (_ : char) -> ()
  done
;;

(* --- Rendering. Same programmer art as the browser canvas; Graphics' origin
   is bottom-LEFT, so every y flips through [gy]. --- *)

let gy ~y ~h = Float.to_int (Config.canvas_height -. y -. h)
let px = Float.to_int

let fill_rect ~color ~x ~y ~w ~h =
  Graphics.set_color color;
  Graphics.fill_rect (px x) (gy ~y ~h) (px w) (px h)
;;

let text ~color ~x ~y s =
  Graphics.set_color color;
  Graphics.moveto (px x) (gy ~y ~h:12.);
  Graphics.draw_string s
;;

let bg = Graphics.rgb 13 17 23
let ground_brown = Graphics.rgb 139 90 43
let pipe_green = Graphics.rgb 63 185 80
let bird_yellow = Graphics.rgb 240 198 73
let dead_red = Graphics.rgb 248 81 73
let ghost_pink = Graphics.rgb 200 100 150
let white = Graphics.rgb 230 237 243
let dim = Graphics.rgb 139 148 158
let checker_dark = Graphics.rgb 22 27 34

let draw_finish_post ~sx =
  let square = 12. in
  let rows = Float.to_int (Float.round_up (Course.ground_top /. square)) in
  for row = 0 to rows - 1 do
    for col = 0 to 1 do
      let y = Float.of_int row *. square in
      let color = if (row + col) % 2 = 0 then white else checker_dark in
      fill_rect
        ~color
        ~x:(sx +. (Float.of_int col *. square))
        ~y
        ~w:square
        ~h:(Float.min square (Course.ground_top -. y))
    done
  done
;;

let draw_ghost (w : World.t) ~offset =
  match !ghost with
  | None -> ()
  | Some g ->
    let sx = g.x -. offset in
    if Float.( > ) (sx +. Config.bird_size) 0.
       && Float.( < ) sx Config.canvas_width
    then
      fill_rect
        ~color:ghost_pink
        ~x:sx
        ~y:g.y
        ~w:Config.bird_size
        ~h:Config.bird_size
    else (
      let ahead = Float.( > ) g.x w.bird.x in
      let ex = if ahead then Config.canvas_width -. 26. else 14. in
      let ey =
        Float.clamp_exn g.y ~min:24. ~max:(Course.ground_top -. 12.)
      in
      let dist =
        Float.to_int (Float.round_nearest (Float.abs (g.x -. w.bird.x)))
      in
      fill_rect ~color:ghost_pink ~x:ex ~y:ey ~w:12. ~h:12.;
      text
        ~color:ghost_pink
        ~x:(if ahead then ex -. 60. else ex +. 16.)
        ~y:ey
        [%string "%{dist#Int}px"])
;;

let draw_progress_bar (w : World.t) =
  let track_x = 20. in
  let track_w = Config.canvas_width -. (2. *. track_x) in
  let frac x = Float.clamp_exn (x /. w.course.finish_x) ~min:0. ~max:1. in
  fill_rect ~color:checker_dark ~x:track_x ~y:8. ~w:track_w ~h:6.;
  (match !ghost with
   | None -> ()
   | Some g ->
     fill_rect
       ~color:ghost_pink
       ~x:(track_x +. (frac g.x *. track_w) -. 4.)
       ~y:5.
       ~w:8.
       ~h:12.);
  fill_rect
    ~color:bird_yellow
    ~x:(track_x +. (frac w.bird.x *. track_w) -. 4.)
    ~y:5.
    ~w:8.
    ~h:12.
;;

let seconds s = [%string "%{Float.round_decimal s ~decimal_digits:1#Float}s"]

let draw_race (w : World.t) =
  let offset = w.bird.x -. Config.bird_screen_x in
  fill_rect
    ~color:bg
    ~x:0.
    ~y:0.
    ~w:Config.canvas_width
    ~h:Config.canvas_height;
  fill_rect
    ~color:ground_brown
    ~x:0.
    ~y:Course.ground_top
    ~w:Config.canvas_width
    ~h:Config.ground_height;
  List.iter w.course.rects ~f:(fun { Course.Rect.x; y; w = rw; h } ->
    let sx = x -. offset in
    if Float.( > ) (sx +. rw) 0. && Float.( < ) sx Config.canvas_width
    then fill_rect ~color:pipe_green ~x:sx ~y ~w:rw ~h);
  let finish_sx = w.course.finish_x -. offset in
  if Float.( > ) (finish_sx +. 24.) 0.
     && Float.( < ) finish_sx Config.canvas_width
  then draw_finish_post ~sx:finish_sx;
  draw_ghost w ~offset;
  (* Bird: blink during i-frames, red when dead. *)
  let blink_off =
    Float.( > ) w.invuln_left 0.
    && Float.( < ) (Float.mod_float w.invuln_left 0.15) 0.06
  in
  let color =
    match w.phase with
    | Countdown _ | Racing | Finished _ -> bird_yellow
    | Dead _ -> dead_red
  in
  if not blink_off
  then
    fill_rect
      ~color
      ~x:Config.bird_screen_x
      ~y:w.bird.y
      ~w:Config.bird_size
      ~h:Config.bird_size;
  draw_progress_bar w;
  let cx = Config.canvas_width /. 2. in
  let cy = Config.canvas_height /. 2. in
  (match w.phase with
   | Countdown { time_left } ->
     text
       ~color:bird_yellow
       ~x:(cx -. 10.)
       ~y:(cy -. 20.)
       (Int.to_string (Float.to_int (Float.round_up time_left)));
     text ~color:dim ~x:(cx -. 40.) ~y:cy "get ready..."
   | Racing when Float.( < ) w.elapsed 0.7 ->
     text ~color:pipe_green ~x:(cx -. 15.) ~y:(cy -. 20.) "GO!"
   | Finished { time } ->
     text ~color:pipe_green ~x:(cx -. 40.) ~y:(cy -. 30.) "FINISHED!";
     text
       ~color:white
       ~x:(cx -. 80.)
       ~y:(cy -. 6.)
       [%string "time %{seconds time} - crashes %{w.crashes#Int}"];
     text ~color:dim ~x:(cx -. 90.) ~y:(cy +. 18.) "press R for a new race"
   | Racing | Dead _ -> ());
  text
    ~color:dim
    ~x:8.
    ~y:(Config.canvas_height -. 18.)
    "SPACE flap | hold D faster | hold A brake | R new race | Q quit"
;;

let draw_lobby () =
  fill_rect
    ~color:bg
    ~x:0.
    ~y:0.
    ~w:Config.canvas_width
    ~h:Config.canvas_height;
  fill_rect
    ~color:ground_brown
    ~x:0.
    ~y:Course.ground_top
    ~w:Config.canvas_width
    ~h:Config.ground_height;
  let cx = Config.canvas_width /. 2. in
  let cy = Config.canvas_height /. 2. in
  text ~color:white ~x:(cx -. 100.) ~y:(cy -. 20.) "MULTIPLAYER FLAPPY RACER";
  text ~color:dim ~x:(cx -. 100.) ~y:cy !net_status
;;

let render () =
  (match !world with None -> draw_lobby () | Some w -> draw_race w);
  Graphics.synchronize ()
;;

(* --- Race lifecycle + fixed-timestep stepping, mirroring the browser game
   loop. --- *)

let track_race_state () =
  match !seed with
  | None ->
    world := None;
    current_seed := None;
    ghost := None
  | Some s ->
    if not ([%equal: int option] (Some s) !current_seed)
    then (
      world := Some (World.create ~seed:s);
      current_seed := Some s;
      ghost := None)
;;

let update_ghost ~dt =
  match !opponent with
  | None -> ghost := None
  | Some target ->
    let eased =
      match !ghost with
      | None -> target
      | Some d ->
        let a = 1. -. Float.exp (-10. *. dt) in
        { Protocol.Pos.x = d.x +. ((target.x -. d.x) *. a)
        ; y = d.y +. ((target.y -. d.y) *. a)
        }
    in
    ghost := Some eased
;;

let last_frame = ref None
let accumulator = ref 0.

let frame () =
  drain_keys ();
  track_race_state ();
  let now = Time_ns.now () in
  let dt =
    match !last_frame with
    | None -> 0.
    | Some last ->
      Float.min (Time_ns.Span.to_sec (Time_ns.diff now last)) 0.1
  in
  last_frame := Some now;
  (match !world with
   | None -> accumulator := 0.
   | Some w ->
     accumulator := !accumulator +. dt;
     let w = ref w in
     while Float.( >= ) !accumulator Config.sim_dt do
       w := World.step !w ~dt:Config.sim_dt ~speed_input:(speed_input ());
       accumulator := !accumulator -. Config.sim_dt
     done;
     world := Some !w;
     my_pos := { Protocol.Pos.x = !w.bird.x; y = !w.bird.y });
  update_ghost ~dt;
  render ()
;;

(* --- Networking: same join + 25 Hz sync as the browser's net.ml. --- *)

let apply_view (view : Protocol.View.t) =
  (match view.race with
   | Waiting_for_players ->
     seed := None;
     net_status := "waiting for another player to join..."
   | Race { seed = s } ->
     seed := Some s;
     net_status := "in race");
  opponent := view.opponent
;;

let start_net ~host ~port ~name =
  let uri = Uri.make ~scheme:"ws" ~host ~port ~path:"/" () in
  match%bind Rpc_websocket.Rpc.client uri with
  | Error err ->
    net_status := [%string "connection failed: %{Error.to_string_hum err}"];
    return ()
  | Ok conn ->
    (match%map Rpc.Rpc.dispatch Protocol.join_rpc conn name with
     | Error err | Ok (Error err) ->
       net_status := [%string "join failed: %{Error.to_string_hum err}"]
     | Ok (Ok player) ->
       net_status := "waiting for another player to join...";
       (request_new_race
        := fun () ->
             don't_wait_for
               (Rpc.Rpc.dispatch Protocol.new_race_rpc conn ()
                >>| (ignore : (unit Or_error.t, Error.t) Result.t -> unit)));
       Clock_ns.every'
         (Time_ns.Span.of_sec (1. /. Config.sync_hz))
         (fun () ->
           match%map
             Rpc.Rpc.dispatch
               Protocol.sync_rpc
               conn
               { Protocol.Update.player; pos = !my_pos }
           with
           | Ok (Ok view) -> apply_view view
           | Ok (Error err) ->
             net_status := [%string "desynced: %{Error.to_string_hum err}"]
           | Error (_ : Error.t) -> ()))
;;

let run ~host ~port ~name () =
  (* [" 960x540"] — the Graphics geometry string needs its leading space. *)
  Graphics.open_graph
    [%string
      " %{Float.to_int Config.canvas_width#Int}x%{Float.to_int\n\
      \                                                    \
       Config.canvas_height#Int}"];
  Graphics.set_window_title "Flappy Racer";
  Graphics.auto_synchronize false;
  don't_wait_for (start_net ~host ~port ~name);
  Clock_ns.every
    (Time_ns.Span.of_sec (1. /. 60.))
    (fun () ->
      try frame () with
      | Graphics.Graphic_failure (_ : string) ->
        (* Window closed. *)
        shutdown 0);
  Deferred.never ()
;;

let command =
  Command.async
    ~summary:
      "Native game window (OCaml Graphics over X11). Run on the server box \
       via ssh -X/-Y from a laptop with an X server (XQuartz)."
    (let%map_open.Command host =
       flag
         "-host"
         (optional_with_default "localhost" string)
         ~doc:"HOST game server host (default localhost)"
     and port =
       flag
         "-port"
         (optional_with_default 8080 int)
         ~doc:"PORT game server port (default 8080)"
     and name =
       flag
         "-name"
         (optional_with_default "player" string)
         ~doc:"NAME display name sent to the server"
     in
     fun () -> run ~host ~port ~name ())
    ~behave_nicely_in_pipeline:false
;;

let () = Command_unix.run command
