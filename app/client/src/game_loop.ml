open! Core
open Js_of_ocaml
open Flappy_game
module Protocol = Flappy_protocol

let canvas_id = "game-canvas"

(* --- Input: which keys are held right now, tracked by [code]. --- *)

let held : String.Hash_set.t = String.Hash_set.create ()

let speed_input () : Bird.Speed_input.t =
  match Hash_set.mem held "ArrowRight", Hash_set.mem held "ArrowLeft" with
  | true, false -> Accelerate
  | false, true -> Brake
  | _ -> Coast
;;

(* --- Mutable client state. ---

   The race lifecycle is server-driven (Stage 4): we build a [World] only
   when the server hands us a seed, and rebuild whenever the seed changes
   (that's how "new race" arrives). [world = None] = lobby. *)

let world : World.t option ref = ref None
let current_seed : int option ref = ref None
let show_overlay = ref true

(* The opponent's ghost as DRAWN: eased toward the last received network
   position every frame, never snapped (context doc §4). *)
let ghost : Protocol.Pos.t option ref = ref None

let use_held_item () =
  Option.iter !world ~f:(fun w ->
    let w, action = World.use_held_item w in
    world := Some w;
    match action with
    | `Applied | `Nothing -> () (* boost/shield are purely local *)
    | `Fire_volley (x, y) -> Net.send_use (Fire_volley { x; y })
    | `Request_swap -> Net.send_use Swap)
;;

let on_keydown code =
  (* [held] makes flap edge-triggered: OS key-repeat fires keydown again, but
     we only flap on the first press. *)
  if not (Hash_set.mem held code)
  then (
    Hash_set.add held code;
    match code with
    | "Space" ->
      Option.iter !world ~f:(fun w -> world := Some (World.flap w))
    | "KeyE" -> use_held_item ()
    | "KeyR" ->
      (* Server-arbitrated: a new seed comes back through sync and both
         clients rebuild. Ignored (server-side) unless 2 players. *)
      Net.request_new_race ()
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
    | "Space" | "ArrowLeft" | "ArrowRight" | "Backquote" | "KeyR" | "KeyE" ->
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

(* --- Simulation bookkeeping driven from the frame loop. --- *)

(* Rebuild the world when the server's seed appears or changes. *)
let track_race_state () =
  match Net.race_seed () with
  | None ->
    world := None;
    current_seed := None;
    ghost := None
  | Some seed ->
    if not ([%equal: int option] (Some seed) !current_seed)
    then (
      world := Some (World.create ~seed);
      current_seed := Some seed;
      ghost := None)
;;

(* Ease the drawn ghost toward the last received position: exponential
   smoothing, frame-rate independent, never snapping (except the very first
   sighting). Variable opponent speed comes for free. *)
let update_ghost ~dt =
  match Net.opponent () with
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

let publish_my_pos (w : World.t) =
  Net.my_pos := { Protocol.Pos.x = w.bird.x; y = w.bird.y }
;;

(* --- Network events -> world: the client glue World's mli describes. Only
   translation lives here; all rules live in the pure World. --- *)

let apply_event w ~(me : Protocol.Player_id.t) (event : Protocol.Event.t) =
  match event with
  | Powerup_claimed { box_id; by = _; item = _ } ->
    (* Despawns the box for both players; if I won it, my held item arrived
       separately via the pickup response. *)
    World.box_claimed w ~box_id
  | Volley_fired { by; x; y = _ } ->
    World.receive_volley w ~x ~hostile:(not (Protocol.Player_id.equal by me))
  | Swapped { p1; p2 } ->
    (* Both clients teleport their own bird to the OTHER's position. *)
    let other : Protocol.Pos.t = match me with P1 -> p2 | P2 -> p1 in
    (match World.receive_swap w ~other:(other.x, other.y) with
     | `Swapped w -> w
     | `Blocked w ->
       (* My shield ate it: tell the world so the initiator reverts. *)
       Net.send_use Swap_blocked;
       w)
  | Swap_blocked -> World.receive_swap_blocked w
;;

let process_network (w : World.t) =
  let w =
    List.fold (Net.drain_pickup_results ()) ~init:w ~f:(fun w result ->
      match result with
      | Some item -> World.receive_pickup w item
      | None -> w (* opponent won the box; the claim event despawns it *))
  in
  let w =
    match Net.me () with
    | None -> w
    | Some me ->
      List.fold
        (Net.drain_events ~current_seed:w.seed)
        ~init:w
        ~f:(fun w event -> apply_event w ~me event)
  in
  (* Touching an unclaimed box with free hands: ask the server. The box only
     despawns when the claim event comes back — first request wins. *)
  (match World.touching_unclaimed_box w with
   | Some box_id -> Net.request_pickup ~box_id
   | None -> ());
  w
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

let fill_circle ctx ~color ~x ~y ~r =
  ctx##.fillStyle := Js.string color;
  ctx##beginPath;
  ctx##arc (n x) (n y) (n r) (n 0.) (n (2. *. Float.pi)) Js._false;
  ctx##fill
;;

let stroke_rect ctx ~color ~line_width ~x ~y ~w ~h =
  ctx##.strokeStyle := Js.string color;
  ctx##.lineWidth := n line_width;
  ctx##strokeRect (n x) (n y) (n w) (n h)
;;

let with_alpha ctx alpha ~f =
  ctx##.globalAlpha := n alpha;
  f ();
  ctx##.globalAlpha := n 1.0
;;

let seconds s = [%string "%{Float.round_decimal s ~decimal_digits:1#Float}s"]

let draw_overlay ctx (w : World.t option) =
  let world_lines =
    match w with
    | None -> [ "state lobby" ]
    | Some w ->
      let bird = w.bird in
      let state =
        match w.phase with
        | Countdown { time_left } ->
          [%string "countdown (%{seconds time_left})"]
        | Racing when Float.( > ) w.invuln_left 0. ->
          [%string "invulnerable (%{seconds w.invuln_left})"]
        | Racing -> "alive"
        | Dead { time_left; _ } -> [%string "dead (%{seconds time_left})"]
        | Finished { time } -> [%string "finished (%{seconds time})"]
      in
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
          "item %{Option.value_map w.held_item ~default:\"-\" \
           ~f:Item.to_string} · boost %{seconds w.boost_left} · shield \
           %{w.shielded#Bool}"]
      ; [%string
          "seed %{w.seed#Int} · scheme %{Sexp.to_string [%sexp \
           (Config.control_scheme : Config.Control_scheme.t)]}"]
      ]
  in
  let net_lines =
    [ [%string "net %{Net.status_line ()}"]
    ; (match Net.ms_since_opponent_update () with
       | None -> "opp no update yet"
       | Some ms -> [%string "opp update %{ms#Int}ms ago"])
    ]
  in
  ctx##.font := Js.string "12px monospace";
  ctx##.fillStyle := Js.string "#e6edf3";
  List.iteri (world_lines @ net_lines) ~f:(fun i line ->
    ctx##fillText
      (Js.string line)
      (n 8.)
      (n (26. +. (Float.of_int i *. 14.))))
;;

(* The pre-race countdown: big centered digits, plus GO! at the start. *)
let draw_countdown ctx ~time_left =
  fill_text
    ctx
    ~color:"#f0c649"
    ~font:"bold 96px monospace"
    ~x:((Config.canvas_width /. 2.) -. 30.)
    ~y:(Config.canvas_height /. 2.)
    (Int.to_string (Float.to_int (Float.round_up time_left)));
  fill_text
    ctx
    ~color:"#8b949e"
    ~font:"16px monospace"
    ~x:((Config.canvas_width /. 2.) -. 60.)
    ~y:((Config.canvas_height /. 2.) +. 40.)
    "get ready..."
;;

let draw_go ctx =
  fill_text
    ctx
    ~color:"#3fb950"
    ~font:"bold 72px monospace"
    ~x:((Config.canvas_width /. 2.) -. 65.)
    ~y:(Config.canvas_height /. 2.)
    "GO!"
;;

(* The finish line: an unmissable two-column checkered post from ceiling to
   ground (still rectangles — build-plan rule 3). *)
let draw_finish_post ctx ~sx =
  let square = 12. in
  let rows = Float.to_int (Float.round_up (Course.ground_top /. square)) in
  for row = 0 to rows - 1 do
    for col = 0 to 1 do
      let y = Float.of_int row *. square in
      let color = if (row + col) % 2 = 0 then "#e6edf3" else "#161b22" in
      fill_rect
        ctx
        ~color
        ~x:(sx +. (Float.of_int col *. square))
        ~y
        ~w:square
        ~h:(Float.min square (Course.ground_top -. y))
    done
  done
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
    ~x:(center_x -. 155.)
    ~y:((Config.canvas_height /. 2.) +. 48.)
    "press R to start a new race (both players)"
;;

let draw_lobby ctx =
  fill_rect
    ctx
    ~color:"#0d1117"
    ~x:0.
    ~y:0.
    ~w:Config.canvas_width
    ~h:Config.canvas_height;
  fill_rect
    ctx
    ~color:"#8b5a2b"
    ~x:0.
    ~y:Course.ground_top
    ~w:Config.canvas_width
    ~h:Config.ground_height;
  fill_text
    ctx
    ~color:"#e6edf3"
    ~font:"bold 24px monospace"
    ~x:((Config.canvas_width /. 2.) -. 220.)
    ~y:((Config.canvas_height /. 2.) -. 10.)
    "MULTIPLAYER FLAPPY RACER";
  fill_text
    ctx
    ~color:"#8b949e"
    ~font:"16px monospace"
    ~x:((Config.canvas_width /. 2.) -. 220.)
    ~y:((Config.canvas_height /. 2.) +. 24.)
    (Net.status_line ())
;;

(* Flash the bird during i-frames: a ~7 Hz blink driven by the remaining
   invulnerability time (deterministic, no wall clock). *)
let invuln_blink_off ~invuln_left =
  Float.( > ) invuln_left 0.
  && Float.( < ) (Float.mod_float invuln_left 0.15) 0.06
;;

(* Held-item slot top-right, plus active-effect readouts. *)
let draw_hud ctx (w : World.t) =
  let x = Config.canvas_width -. 64. in
  fill_rect ctx ~color:"#161b22" ~x ~y:24. ~w:44. ~h:44.;
  stroke_rect ctx ~color:"#30363d" ~line_width:2. ~x ~y:24. ~w:44. ~h:44.;
  (match w.held_item with
   | Some item ->
     fill_text
       ctx
       ~color:"#f0c649"
       ~font:"bold 26px monospace"
       ~x:(x +. 14.)
       ~y:55.
       (Item.tag item);
     fill_text
       ctx
       ~color:"#8b949e"
       ~font:"11px monospace"
       ~x:(x -. 12.)
       ~y:82.
       [%string "%{Item.to_string item} [E]"]
   | None ->
     fill_text
       ctx
       ~color:"#30363d"
       ~font:"bold 26px monospace"
       ~x:(x +. 16.)
       ~y:55.
       "-");
  if Float.( > ) w.boost_left 0.
  then
    fill_text
      ctx
      ~color:"#f0c649"
      ~font:"bold 14px monospace"
      ~x:(x -. 30.)
      ~y:104.
      [%string "BOOST %{seconds w.boost_left}"];
  if w.shielded
  then
    fill_text
      ctx
      ~color:"#58a6ff"
      ~font:"bold 14px monospace"
      ~x:(x -. 30.)
      ~y:122.
      "SHIELD UP"
;;

(* Incoming-volley warning: a flashing red frame while hostile bullets are in
   flight (context doc §3: the victim gets a visual warning). *)
let draw_volley_warning ctx (w : World.t) =
  let incoming = List.exists w.bullets ~f:(fun b -> b.hostile) in
  if incoming && Float.( < ) (Float.mod_float w.elapsed 0.3) 0.18
  then
    stroke_rect
      ctx
      ~color:"#f85149"
      ~line_width:6.
      ~x:3.
      ~y:3.
      ~w:(Config.canvas_width -. 6.)
      ~h:(Config.canvas_height -. 6.)
;;

(* Progress bar across the top: both birds' positions along the full course —
   essential, not polish (context doc §1). *)
let draw_progress_bar ctx (w : World.t) =
  let track_x = 20. in
  let track_w = Config.canvas_width -. (2. *. track_x) in
  let frac x = Float.clamp_exn (x /. w.course.finish_x) ~min:0. ~max:1. in
  fill_rect ctx ~color:"#30363d" ~x:track_x ~y:8. ~w:track_w ~h:6.;
  (match !ghost with
   | None -> ()
   | Some g ->
     with_alpha ctx 0.7 ~f:(fun () ->
       fill_rect
         ctx
         ~color:"#f778ba"
         ~x:(track_x +. (frac g.x *. track_w) -. 4.)
         ~y:5.
         ~w:8.
         ~h:12.));
  fill_rect
    ctx
    ~color:"#f0c649"
    ~x:(track_x +. (frac w.bird.x *. track_w) -. 4.)
    ~y:5.
    ~w:8.
    ~h:12.
;;

(* The opponent: a 55%-opacity square at its eased position when on screen;
   otherwise a small edge marker with the distance, so the opponent is always
   perceivable (context doc §1). *)
let draw_ghost ctx (w : World.t) ~offset =
  match !ghost with
  | None -> ()
  | Some g ->
    let sx = g.x -. offset in
    if Float.( > ) (sx +. Config.bird_size) 0.
       && Float.( < ) sx Config.canvas_width
    then
      with_alpha ctx 0.55 ~f:(fun () ->
        fill_rect
          ctx
          ~color:"#f778ba"
          ~x:sx
          ~y:g.y
          ~w:Config.bird_size
          ~h:Config.bird_size)
    else (
      let ahead = Float.( > ) g.x w.bird.x in
      let ex = if ahead then Config.canvas_width -. 26. else 14. in
      let ey =
        Float.clamp_exn g.y ~min:24. ~max:(Course.ground_top -. 12.)
      in
      let dist =
        Float.to_int (Float.round_nearest (Float.abs (g.x -. w.bird.x)))
      in
      with_alpha ctx 0.8 ~f:(fun () ->
        fill_rect ctx ~color:"#f778ba" ~x:ex ~y:ey ~w:12. ~h:12.);
      fill_text
        ctx
        ~color:"#f778ba"
        ~font:"12px monospace"
        ~x:(if ahead then ex -. 60. else ex +. 16.)
        ~y:(ey +. 10.)
        [%string "%{dist#Int}px"])
;;

let draw_race ctx (w : World.t) =
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
  (* Item boxes: yellow "?" squares, gone once claimed. *)
  List.iter w.course.item_boxes ~f:(fun box ->
    if not (Set.mem w.boxes_taken box.id)
    then (
      let sx = box.x -. offset in
      if Float.( > ) (sx +. Config.item_box_size) 0.
         && Float.( < ) sx Config.canvas_width
      then (
        fill_rect
          ctx
          ~color:"#f0c649"
          ~x:sx
          ~y:box.y
          ~w:Config.item_box_size
          ~h:Config.item_box_size;
        fill_text
          ctx
          ~color:"#0d1117"
          ~font:"bold 18px monospace"
          ~x:(sx +. 8.)
          ~y:(box.y +. 20.)
          "?")));
  (* Volley bullets: red = the opponent's (can hit me), white = mine (display
     only; their client decides their fate). *)
  List.iter w.bullets ~f:(fun b ->
    let sx = b.x -. offset in
    if Float.( > ) sx 0. && Float.( < ) sx Config.canvas_width
    then
      fill_circle
        ctx
        ~color:(if b.hostile then "#f85149" else "#e6edf3")
        ~x:sx
        ~y:b.y
        ~r:Config.bullet_radius);
  (* Finish line: checkered post from ceiling to ground. *)
  let finish_sx = w.course.finish_x -. offset in
  if Float.( > ) (finish_sx +. 24.) 0.
     && Float.( < ) finish_sx Config.canvas_width
  then draw_finish_post ctx ~sx:finish_sx;
  (* The opponent first, so our own bird draws on top when overlapping. *)
  draw_ghost ctx w ~offset;
  (* The bird: yellow racing, red dead, blinking during i-frames. *)
  let color =
    match w.phase with
    | Countdown _ | Racing | Finished _ -> "#f0c649"
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
  (* Shield: a ring around the bird while it's up. *)
  if w.shielded
  then
    stroke_rect
      ctx
      ~color:"#58a6ff"
      ~line_width:3.
      ~x:(Config.bird_screen_x -. 5.)
      ~y:(bird.y -. 5.)
      ~w:(Config.bird_size +. 10.)
      ~h:(Config.bird_size +. 10.);
  draw_progress_bar ctx w;
  draw_hud ctx w;
  draw_volley_warning ctx w;
  match w.phase with
  | Countdown { time_left } -> draw_countdown ctx ~time_left
  | Racing when Float.( < ) w.elapsed 0.7 -> draw_go ctx
  | Finished { time } -> draw_win_screen ctx ~time ~crashes:w.crashes
  | Racing | Dead _ -> ()
;;

let render () =
  match
    Dom_html.getElementById_coerce canvas_id Dom_html.CoerceTo.canvas
  with
  | None -> () (* Bonsai hasn't mounted the canvas yet; skip this frame. *)
  | Some canvas ->
    let ctx = canvas##getContext Dom_html._2d_ in
    (match !world with None -> draw_lobby ctx | Some w -> draw_race ctx w);
    if !show_overlay then draw_overlay ctx !world
;;

(* --- The loop: fixed-timestep simulation, render once per frame. --- *)

let start () =
  install_input_handlers ();
  let last_ms = ref None in
  let accumulator = ref 0. in
  let rec frame now_ms =
    track_race_state ();
    (match !last_ms with
     | None -> ()
     | Some last ->
       (* Clamp so a backgrounded tab doesn't fast-forward on return. *)
       let elapsed = Float.min ((now_ms -. last) /. 1000.) 0.1 in
       (match !world with
        | None -> accumulator := 0.
        | Some w ->
          accumulator := !accumulator +. elapsed;
          let w = ref w in
          while Float.( >= ) !accumulator Config.sim_dt do
            w
            := World.step !w ~dt:Config.sim_dt ~speed_input:(speed_input ());
            accumulator := !accumulator -. Config.sim_dt
          done;
          let w = process_network !w in
          world := Some w;
          publish_my_pos w);
       update_ghost ~dt:elapsed);
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
