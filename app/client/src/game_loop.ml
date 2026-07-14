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
    | "Enter" -> if Net.ready_to_start () then Net.request_new_race ()
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
    Js._true);
  (* The lobby's start button is the whole canvas: click to launch. *)
  Dom_html.document##.onclick
  := Dom_html.handler (fun (_ : Dom_html.mouseEvent Js.t) ->
    if Net.ready_to_start () then Net.request_new_race ();
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

let seconds s = [%string "%{Float.round_decimal s ~decimal_digits:1#Float}s"]

let fill_path ctx ~color points =
  ctx##.fillStyle := Js.string color;
  ctx##beginPath;
  (match points with
   | [] -> ()
   | (x0, y0) :: rest ->
     ctx##moveTo (n x0) (n y0);
     List.iter rest ~f:(fun (x, y) -> ctx##lineTo (n x) (n y)));
  ctx##closePath;
  ctx##fill
;;

(* --- The night-sky scene (Stage 8: parallax background). Everything is
   deterministic from the camera offset — no wall clock, no state. --- *)

let sky_bands =
  (* Vertical gradient, dark zenith to a lighter horizon. *)
  [ "#0a0e1a"; "#0d1322"; "#111a2d"; "#161f38"; "#1a2643"; "#1f2c4e" ]
;;

let wrap_x x = Float.mod_float (Float.mod_float x 1920. +. 1920.) 1920.

let draw_background ctx ~offset =
  let band_h =
    Config.canvas_height /. Float.of_int (List.length sky_bands)
  in
  List.iteri sky_bands ~f:(fun i color ->
    fill_rect
      ctx
      ~color
      ~x:0.
      ~y:(Float.of_int i *. band_h)
      ~w:Config.canvas_width
      ~h:(band_h +. 1.));
  (* Moon: pinned to the sky (infinite distance), with a bite taken out of it
     for a crescent. *)
  fill_circle ctx ~color:"#d6d9e0" ~x:830. ~y:80. ~r:26.;
  fill_circle ctx ~color:"#0d1322" ~x:842. ~y:72. ~r:22.;
  (* Stars: slow parallax (0.12x), fixed pseudo-random constellation. *)
  for i = 0 to 69 do
    (* 31-bit-safe hash: js_of_ocaml ints are 32-bit. *)
    let h = i * 73856093 land 0xFFFFFF in
    let x0 = Float.of_int (h % 1920) in
    let y = Float.of_int (8 + (h / 1920 % 300)) in
    let sx = wrap_x (x0 -. (offset *. 0.12)) in
    if Float.( < ) sx Config.canvas_width
    then (
      let bright = i % 5 = 0 in
      fill_rect
        ctx
        ~color:(if bright then "#e6edf3" else "#6e7681")
        ~x:sx
        ~y
        ~w:(if bright then 2.5 else 1.5)
        ~h:(if bright then 2.5 else 1.5))
  done;
  (* Rolling hill silhouettes: mid parallax (0.35x). *)
  let spacing = 330. in
  let par = offset *. 0.35 in
  let first = Float.round_down (par /. spacing) in
  for j = -1 to 4 do
    let k = first +. Float.of_int j in
    let cx = (k *. spacing) -. par in
    let bump = Float.of_int (55 + (Float.to_int k * 7919 % 65)) in
    fill_circle
      ctx
      ~color:"#152030"
      ~x:(cx +. (spacing /. 2.))
      ~y:(Course.ground_top +. 150. -. bump)
      ~r:150.
  done
;;

let draw_ground ctx ~offset =
  fill_rect
    ctx
    ~color:"#5c4023"
    ~x:0.
    ~y:Course.ground_top
    ~w:Config.canvas_width
    ~h:Config.ground_height;
  (* Grass lip. *)
  fill_rect
    ctx
    ~color:"#2ea043"
    ~x:0.
    ~y:Course.ground_top
    ~w:Config.canvas_width
    ~h:8.;
  (* Scrolling dirt flecks so ground speed is readable. *)
  let par = Float.mod_float (Float.mod_float offset 60. +. 60.) 60. in
  let x = ref (-.par) in
  while Float.( < ) !x Config.canvas_width do
    fill_rect
      ctx
      ~color:"#4a3118"
      ~x:!x
      ~y:(Course.ground_top +. 22.)
      ~w:22.
      ~h:5.;
    fill_rect
      ctx
      ~color:"#6d4d2a"
      ~x:(!x +. 31.)
      ~y:(Course.ground_top +. 40.)
      ~w:14.
      ~h:4.;
    x := !x +. 60.
  done
;;

(* A pipe segment with body shading and a rimmed cap at the gap-facing end (y
   = 0 means it hangs from the ceiling, so its cap is at the bottom;
   otherwise the cap is on top). *)
let draw_pipe ctx ~sx ~y ~w ~h =
  let cap_h = Float.min 20. h in
  let is_top = Float.( <= ) y 0.5 in
  fill_rect ctx ~color:"#2ea043" ~x:sx ~y ~w ~h;
  fill_rect ctx ~color:"#56d364" ~x:(sx +. 6.) ~y ~w:8. ~h;
  fill_rect ctx ~color:"#1f7a33" ~x:(sx +. w -. 14.) ~y ~w:14. ~h;
  let cap_y = if is_top then y +. h -. cap_h else y in
  fill_rect
    ctx
    ~color:"#38b249"
    ~x:(sx -. 5.)
    ~y:cap_y
    ~w:(w +. 10.)
    ~h:cap_h;
  fill_rect ctx ~color:"#56d364" ~x:(sx -. 5.) ~y:cap_y ~w:6. ~h:cap_h;
  stroke_rect
    ctx
    ~color:"#0f3d1a"
    ~line_width:2.
    ~x:(sx -. 5.)
    ~y:cap_y
    ~w:(w +. 10.)
    ~h:cap_h
;;

(* An actual bird (Stage 8): round body, belly, flapping wing, beak and eye —
   built from primitives on the same 30px collision square (the hitbox never
   changed, only the feathers). *)
let draw_bird ctx ~x ~y ~vy ~body ~wing ~belly ~ghost =
  let cx = x +. (Config.bird_size /. 2.) in
  let cy = y +. (Config.bird_size /. 2.) in
  let r = Config.bird_size /. 2. in
  fill_circle ctx ~color:body ~x:cx ~y:cy ~r;
  if not ghost
  then fill_circle ctx ~color:belly ~x:(cx -. 3.) ~y:(cy +. 6.) ~r:(r *. 0.5);
  (* Wing flaps with vertical motion: raised while rising. *)
  let wing_tip_y = if Float.( < ) vy (-50.) then cy -. 14. else cy +. 9. in
  fill_path
    ctx
    ~color:wing
    [ cx -. 14., cy; cx +. 1., wing_tip_y; cx +. 5., cy +. 3. ];
  (* Beak. *)
  fill_path
    ctx
    ~color:"#f0883e"
    [ cx +. 11., cy -. 5.; cx +. 24., cy; cx +. 11., cy +. 5. ];
  (* Eye. *)
  if not ghost
  then (
    fill_circle ctx ~color:"#ffffff" ~x:(cx +. 6.) ~y:(cy -. 6.) ~r:5.;
    fill_circle ctx ~color:"#0d1117" ~x:(cx +. 8.) ~y:(cy -. 6.) ~r:2.3)
;;

(* Always-on race stats (the debug overlay stays behind backtick). *)
let draw_stats ctx (w : World.t) ~ghost_x =
  fill_rect ctx ~color:"rgba(13, 17, 23, 0.75)" ~x:8. ~y:26. ~w:190. ~h:92.;
  let place =
    match ghost_x with
    | Some gx when Float.( > ) gx w.bird.x -> "2nd", "#f778ba"
    | Some (_ : float) -> "1st", "#3fb950"
    | None -> "solo", "#8b949e"
  in
  let place_text, place_color = place in
  fill_text
    ctx
    ~color:place_color
    ~font:"bold 22px monospace"
    ~x:16.
    ~y:52.
    place_text;
  let progress =
    Float.round_nearest (100. *. w.bird.x /. w.course.finish_x)
  in
  let lines =
    [ [%string "speed %{Float.round_nearest w.bird.speed#Float} px/s"]
    ; [%string
        "pos %{Float.round_nearest w.bird.x#Float}m · %{progress#Float}%"]
    ; [%string "time %{seconds w.elapsed} · crashes %{w.crashes#Int}"]
    ]
  in
  List.iteri lines ~f:(fun i line ->
    fill_text
      ctx
      ~color:"#e6edf3"
      ~font:"12px monospace"
      ~x:16.
      ~y:(70. +. (Float.of_int i *. 16.))
      line)
;;

let with_alpha ctx alpha ~f =
  ctx##.globalAlpha := n alpha;
  f ();
  ctx##.globalAlpha := n 1.0
;;

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
  (* Below the always-on stats panel. *)
  List.iteri (world_lines @ net_lines) ~f:(fun i line ->
    ctx##fillText
      (Js.string line)
      (n 8.)
      (n (136. +. (Float.of_int i *. 14.))))
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
  draw_background ctx ~offset:0.;
  draw_ground ctx ~offset:0.;
  (* A decorative bird bobbing on the title screen. *)
  draw_bird
    ctx
    ~x:(Config.bird_screen_x -. 60.)
    ~y:170.
    ~vy:(-100.)
    ~body:"#f0c649"
    ~wing:"#d9a62e"
    ~belly:"#f7e3a1"
    ~ghost:false;
  fill_text
    ctx
    ~color:"#e6edf3"
    ~font:"bold 28px monospace"
    ~x:((Config.canvas_width /. 2.) -. 210.)
    ~y:230.
    "MULTIPLAYER FLAPPY RACER";
  if Net.ready_to_start ()
  then (
    (* The start button (the whole canvas is clickable; ENTER works too). *)
    let bx = (Config.canvas_width /. 2.) -. 110. in
    fill_rect ctx ~color:"#238636" ~x:bx ~y:280. ~w:220. ~h:56.;
    stroke_rect
      ctx
      ~color:"#3fb950"
      ~line_width:2.
      ~x:bx
      ~y:280.
      ~w:220.
      ~h:56.;
    fill_text
      ctx
      ~color:"#ffffff"
      ~font:"bold 24px monospace"
      ~x:(bx +. 38.)
      ~y:316.
      "START RACE";
    fill_text
      ctx
      ~color:"#8b949e"
      ~font:"14px monospace"
      ~x:(bx -. 20.)
      ~y:360.
      "opponent found - click or press ENTER")
  else
    fill_text
      ctx
      ~color:"#8b949e"
      ~font:"16px monospace"
      ~x:((Config.canvas_width /. 2.) -. 220.)
      ~y:300.
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
        draw_bird
          ctx
          ~x:sx
          ~y:g.y
          ~vy:100.
          ~body:"#f778ba"
          ~wing:"#c9509a"
          ~belly:"#f7a3d0"
          ~ghost:true)
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
  draw_background ctx ~offset;
  draw_ground ctx ~offset;
  (* Pipes: only those overlapping the camera window. *)
  List.iter w.course.rects ~f:(fun { Course.Rect.x; y; w = rw; h } ->
    let sx = x -. offset in
    if Float.( > ) (sx +. rw +. 6.) 0.
       && Float.( < ) (sx -. 6.) Config.canvas_width
    then draw_pipe ctx ~sx ~y ~w:rw ~h);
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
  let body, wing =
    match w.phase with
    | Countdown _ | Racing | Finished _ -> "#f0c649", "#d9a62e"
    | Dead _ -> "#f85149", "#b62324"
  in
  if not (invuln_blink_off ~invuln_left:w.invuln_left)
  then
    draw_bird
      ctx
      ~x:Config.bird_screen_x
      ~y:bird.y
      ~vy:bird.vy
      ~body
      ~wing
      ~belly:"#f7e3a1"
      ~ghost:false;
  (* Shield: a bubble around the bird while it's up. *)
  if w.shielded
  then (
    ctx##.strokeStyle := Js.string "#58a6ff";
    ctx##.lineWidth := n 3.;
    ctx##beginPath;
    ctx##arc
      (n (Config.bird_screen_x +. (Config.bird_size /. 2.)))
      (n (bird.y +. (Config.bird_size /. 2.)))
      (n ((Config.bird_size /. 2.) +. 8.))
      (n 0.)
      (n (2. *. Float.pi))
      Js._false;
    ctx##stroke);
  draw_progress_bar ctx w;
  draw_stats
    ctx
    w
    ~ghost_x:(Option.map !ghost ~f:(fun (g : Protocol.Pos.t) -> g.x));
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
