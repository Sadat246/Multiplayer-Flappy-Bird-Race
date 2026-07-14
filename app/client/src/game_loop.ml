open! Core
open Js_of_ocaml
open Flappy_game

let canvas_id = "game-canvas"

(* --- Input: which keys are held right now, tracked by [code]. --- *)

let held : String.Hash_set.t = String.Hash_set.create ()

let speed_input () : Bird.Speed_input.t =
  match Hash_set.mem held "ArrowRight", Hash_set.mem held "ArrowLeft" with
  | true, false -> Accelerate
  | false, true -> Brake
  | _ -> Coast
;;

(* --- Mutable client state: the world, and overlay visibility. --- *)

let world = ref (World.create ~seed:Config.debug_seed)
let show_overlay = ref true

let on_keydown code =
  (* [held] makes flap edge-triggered: OS key-repeat fires keydown again, but
     we only flap on the first press. *)
  if not (Hash_set.mem held code)
  then (
    Hash_set.add held code;
    match code with
    | "Space" -> world := World.flap !world
    | "KeyR" -> world := World.restart !world
    | "Backquote" -> show_overlay := not !show_overlay
    | _ -> ())
;;

let install_input_handlers () =
  Dom_html.document##.onkeydown
  := Dom_html.handler (fun ev ->
    let code =
      Js.to_string (Js.Optdef.get ev##.code (fun () -> Js.string ""))
    in
    on_keydown code;
    (* Swallow keys the game uses so Space/arrows never scroll the page. *)
    match code with
    | "Space" | "ArrowLeft" | "ArrowRight" | "Backquote" | "KeyR" ->
      Js._false
    | _ -> Js._true);
  Dom_html.document##.onkeyup
  := Dom_html.handler (fun ev ->
    let code =
      Js.to_string (Js.Optdef.get ev##.code (fun () -> Js.string ""))
    in
    Hash_set.remove held code;
    Js._true)
;;

(* --- Rendering. Programmer art only (build-plan rule 3). --- *)

(* Canvas methods take wrapped JS numbers in recent js_of_ocaml. *)
let n = Js.number_of_float

let fill_rect ctx ~color ~x ~y ~w ~h =
  ctx##.fillStyle := Js.string color;
  ctx##fillRect (n x) (n y) (n w) (n h)
;;

let fill_text ctx ~color ~font ~x ~y text =
  ctx##.fillStyle := Js.string color;
  ctx##.font := Js.string font;
  ctx##fillText (Js.string text) (n x) (n y)
;;

let seconds s = [%string "%{Float.round_decimal s ~decimal_digits:1#Float}s"]

let draw_overlay ctx (w : World.t) =
  let bird = w.bird in
  let state =
    match w.phase with
    | Racing when Float.( > ) w.invuln_left 0. ->
      [%string "invulnerable (%{seconds w.invuln_left})"]
    | Racing -> "alive"
    | Dead { time_left; _ } -> [%string "dead (%{seconds time_left})"]
    | Finished { time } -> [%string "finished (%{seconds time})"]
  in
  let lines =
    [ [%string
        "x %{Float.round_nearest bird.x#Float} / %{Float.round_nearest \
         w.course.finish_x#Float}"]
    ; [%string "y %{Float.round_nearest bird.y#Float}"]
    ; [%string "vy %{Float.round_nearest bird.vy#Float}"]
    ; [%string "speed %{Float.round_nearest bird.speed#Float}"]
    ; [%string "state %{state}"]
    ; [%string "crashes %{w.crashes#Int}"]
    ; [%string "time %{seconds w.elapsed}"]
    ; [%string
        "seed %{Config.debug_seed#Int} · scheme %{Sexp.to_string [%sexp \
         (Config.control_scheme : Config.Control_scheme.t)]}"]
    ]
  in
  ctx##.font := Js.string "12px monospace";
  ctx##.fillStyle := Js.string "#e6edf3";
  List.iteri lines ~f:(fun i line ->
    ctx##fillText
      (Js.string line)
      (n 8.)
      (n (16. +. (Float.of_int i *. 14.))))
;;

let draw_win_screen ctx ~time ~crashes =
  fill_rect
    ctx
    ~color:"rgba(0, 0, 0, 0.65)"
    ~x:0.
    ~y:0.
    ~w:Config.canvas_width
    ~h:Config.canvas_height;
  let center_x = Config.canvas_width /. 2. in
  fill_text
    ctx
    ~color:"#3fb950"
    ~font:"bold 32px monospace"
    ~x:(center_x -. 130.)
    ~y:((Config.canvas_height /. 2.) -. 20.)
    "FINISHED!";
  fill_text
    ctx
    ~color:"#e6edf3"
    ~font:"16px monospace"
    ~x:(center_x -. 150.)
    ~y:((Config.canvas_height /. 2.) +. 16.)
    [%string "time %{seconds time} · crashes %{crashes#Int}"];
  fill_text
    ctx
    ~color:"#8b949e"
    ~font:"14px monospace"
    ~x:(center_x -. 110.)
    ~y:((Config.canvas_height /. 2.) +. 48.)
    "press R to race again"
;;

(* Flash the bird during i-frames: a ~7 Hz blink driven by the remaining
   invulnerability time (deterministic, no wall clock). *)
let invuln_blink_off ~invuln_left =
  Float.( > ) invuln_left 0.
  && Float.( < ) (Float.mod_float invuln_left 0.15) 0.06
;;

let render () =
  match
    Dom_html.getElementById_coerce canvas_id Dom_html.CoerceTo.canvas
  with
  | None -> () (* Bonsai hasn't mounted the canvas yet; skip this frame. *)
  | Some canvas ->
    let ctx = canvas##getContext Dom_html._2d_ in
    let w = !world in
    let bird = w.bird in
    (* Camera: bird fixed at [bird_screen_x]; the world slides past. *)
    let offset = bird.x -. Config.bird_screen_x in
    fill_rect
      ctx
      ~color:"#0d1117"
      ~x:0.
      ~y:0.
      ~w:Config.canvas_width
      ~h:Config.canvas_height;
    (* Ground. *)
    fill_rect
      ctx
      ~color:"#8b5a2b"
      ~x:0.
      ~y:Course.ground_top
      ~w:Config.canvas_width
      ~h:Config.ground_height;
    (* Pipes: only those overlapping the camera window. *)
    List.iter w.course.rects ~f:(fun { Course.Rect.x; y; w = rw; h } ->
      let sx = x -. offset in
      if Float.( > ) (sx +. rw) 0. && Float.( < ) sx Config.canvas_width
      then fill_rect ctx ~color:"#3fb950" ~x:sx ~y ~w:rw ~h);
    (* Finish line: a white post from ceiling to ground. *)
    let finish_sx = w.course.finish_x -. offset in
    if Float.( > ) finish_sx 0. && Float.( < ) finish_sx Config.canvas_width
    then
      fill_rect
        ctx
        ~color:"#e6edf3"
        ~x:finish_sx
        ~y:0.
        ~w:10.
        ~h:Course.ground_top;
    (* The bird: yellow racing, red dead, blinking during i-frames. *)
    let color =
      match w.phase with
      | Racing | Finished _ -> "#f0c649"
      | Dead _ -> "#f85149"
    in
    if not (invuln_blink_off ~invuln_left:w.invuln_left)
    then
      fill_rect
        ctx
        ~color
        ~x:Config.bird_screen_x
        ~y:bird.y
        ~w:Config.bird_size
        ~h:Config.bird_size;
    (match w.phase with
     | Finished { time } -> draw_win_screen ctx ~time ~crashes:w.crashes
     | Racing | Dead _ -> ());
    if !show_overlay then draw_overlay ctx w
;;

(* --- The loop: fixed-timestep simulation, render once per frame. --- *)

let start () =
  install_input_handlers ();
  let last_ms = ref None in
  let accumulator = ref 0. in
  let rec frame now_ms =
    (match !last_ms with
     | None -> ()
     | Some last ->
       (* Clamp so a backgrounded tab doesn't fast-forward on return. *)
       let elapsed = Float.min ((now_ms -. last) /. 1000.) 0.1 in
       accumulator := !accumulator +. elapsed;
       while Float.( >= ) !accumulator Config.sim_dt do
         world
         := World.step !world ~dt:Config.sim_dt ~speed_input:(speed_input ());
         accumulator := !accumulator -. Config.sim_dt
       done);
    last_ms := Some now_ms;
    render ();
    request_frame ()
  and request_frame () =
    ignore
      (Dom_html.window##requestAnimationFrame
         (Js.wrap_callback (fun ms -> frame (Js.float_of_number ms)))
       : Dom_html.animation_frame_request_id)
  in
  request_frame ()
;;
