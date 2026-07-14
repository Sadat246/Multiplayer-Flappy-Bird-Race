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

let world = ref World.initial
let show_overlay = ref true

let on_keydown code =
  (* [held] makes flap edge-triggered: OS key-repeat fires keydown again, but
     we only flap on the first press. *)
  if not (Hash_set.mem held code)
  then (
    Hash_set.add held code;
    match code with
    | "Space" -> world := World.flap !world
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
    | "Space" | "ArrowLeft" | "ArrowRight" | "Backquote" -> Js._false
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

let draw_overlay ctx (w : World.t) =
  let bird = w.bird in
  let state =
    match w.phase with
    | Racing -> "alive"
    | Crashed { time_left } -> [%string "crashed (%{time_left#Float}s)"]
  in
  let lines =
    [ [%string "x %{Float.round_nearest bird.x#Float}"]
    ; [%string "y %{Float.round_nearest bird.y#Float}"]
    ; [%string "vy %{Float.round_nearest bird.vy#Float}"]
    ; [%string "speed %{Float.round_nearest bird.speed#Float}"]
    ; [%string "state %{state}"]
    ; [%string "crashes %{w.crashes#Int}"]
    ]
  in
  ctx##.fillStyle := Js.string "#e6edf3";
  ctx##.font := Js.string "12px monospace";
  List.iteri lines ~f:(fun i line ->
    ctx##fillText
      (Js.string line)
      (n 8.)
      (n (16. +. (Float.of_int i *. 14.))))
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
    List.iter Course.rects ~f:(fun { Course.Rect.x; y; w = rw; h } ->
      let sx = x -. offset in
      if Float.( > ) (sx +. rw) 0. && Float.( < ) sx Config.canvas_width
      then fill_rect ctx ~color:"#3fb950" ~x:sx ~y ~w:rw ~h);
    (* The bird: yellow while racing, red while crashed. *)
    let color =
      match w.phase with Racing -> "#f0c649" | Crashed _ -> "#f85149"
    in
    fill_rect
      ctx
      ~color
      ~x:Config.bird_screen_x
      ~y:bird.y
      ~w:Config.bird_size
      ~h:Config.bird_size;
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
