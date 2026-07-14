open! Core
open! Bonsai_web
module Config = Flappy_game.Config

(* Same inline-style discipline as the jsip-exchange dashboard: no ppx_css,
   named tokens, one [style] helper. *)
let bg = "#0d1117"
let text = "#e6edf3"
let text_dim = "#8b949e"
let style s = Vdom.Attr.create "style" s

let page =
  style
    [%string
      "background:%{bg}; color:%{text}; min-height:100vh; margin:0; \
       display:flex; flex-direction:column; align-items:center; gap:12px; \
       padding:24px; font-family:'SF Mono',ui-monospace,Menlo,monospace; \
       box-sizing:border-box"]
;;

let canvas =
  (* Fixed integer attribute size = the coordinate system Game_loop draws in;
     CSS never scales it, so one canvas px = one Config px. *)
  Vdom.Node.create
    "canvas"
    ~attrs:
      [ Vdom.Attr.id Game_loop.canvas_id
      ; Vdom.Attr.create
          "width"
          (Int.to_string (Float.to_int Config.canvas_width))
      ; Vdom.Attr.create
          "height"
          (Int.to_string (Float.to_int Config.canvas_height))
      ; style "border:1px solid #30363d; border-radius:6px"
      ]
    []
;;

let app (local_ _graph) : Vdom.Node.t Bonsai.t =
  Bonsai.return
    {%html|
      <div %{page}>
        <h1 style="font-size:18px; margin:0">Flappy Racer — square avoiding squares</h1>
        %{canvas}
        <div %{style ("font-size:13px; color:" ^ text_dim)}>
          space = flap · hold → to speed up · hold ← to brake · R = new race for both players · ` = debug overlay
        </div>
      </div>
    |}
;;
