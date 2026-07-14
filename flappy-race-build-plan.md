# BUILD PLAN: Multiplayer Flappy Racer — Staged Implementation Instructions (OCaml + Bonsai)

Use this alongside the context doc (`flappy-race-context-prompt.md`). The context doc says WHAT we're building; this doc says HOW and IN WHAT ORDER. Paste both into the build session.

> **STACK CHANGE — supersedes context doc §4's "browser JS clients, Node.js + `ws`, JSON".**
> The game is written in **OCaml end to end**: a **Bonsai web client** (compiled to JS via `js_of_ocaml`) and a **native OCaml Async server**, talking **Async-RPC over WebSocket** (`async_rpc_websocket`). Everything else in the context doc — fat client / thin referee server, seed determinism, client-owns-own-fate, the power-up designs, the anti-goals — still stands unchanged. The §6 note ("steal the protocol discipline from jsip-exchange, not the server-side simulation") upgrades to: **steal the actual machinery too.**

---

## Stack reference: code to pull from `/home/ubuntu/jsip-exchange`

All patterns below already build and run in this environment (see `jsip-exchange.opam`: `bonsai_web`, `async_rpc_websocket`, `cohttp-async`, `ppx_html`, `js_of_ocaml-ppx` are installed).

| What we need | Steal from | Pattern |
|---|---|---|
| Server: one port serving `index.html` + `main.bc.js` + RPC-over-WebSocket | `app/dashboard/server/main.ml` | `Rpc_websocket.Rpc.serve ~implementations ~http_handler` with a whitelisted static handler |
| Client entry point (jsoo executable) | `app/dashboard/client/bin/{main.ml,dune}` | `Bonsai_web.Start.start`, `(modes js)`, `--effects=cps` |
| Client→server RPC from Bonsai | `app/dashboard/client/src/view.ml:235-280` | `Rpc_effect.Where_to_connect.self` (connects back to the serving origin — client needs **no host config, ever**) + `Rpc_effect.Rpc.poll` |
| Protocol library shape | `lib/dashboard_protocol/` | Wire types + `Rpc.Rpc.create` defs in a lib depending only on `core` + `async_rpc_kernel` (usable from both jsoo client and native server) |
| Request/response arbitration RPC | `lib/gateway/src/rpc_protocol.ml` (`submit_order_rpc`) | Our `pickup_request` is the same shape |
| Pure, testable state module | `app/dashboard/client/controller/controller.ml` | Pure fold (`empty` / `apply` / `display`), `[@@deriving sexp_of, equal]`, expect-tested without a browser |
| Static page | `app/dashboard/client/static/index.html` | 12 lines: `<div id="app">` + `<script defer src="main.bc.js">` |
| Latest-slot backpressure | `app/dashboard/server/main.ml` (`latest` ref) | Server stores each player's newest position in a single slot, never a queue |
| Code conventions | `/home/ubuntu/jsip-exchange/CLAUDE.md` | See rule 9 below |

**Key architectural consequence (from the dashboard server's doc comment): Bonsai has no `Rpc_effect.Pipe_rpc` — browsers can't subscribe to server-push pipes.** So the context doc's "server relays `pos` messages" becomes a **polling state-exchange**: each client calls one `sync_rpc` at tick rate (25 Hz), sending its own position and receiving the opponent's last-known position + race state + any events it hasn't seen yet (client sends a `last_seen_event` sequence number; server responds with newer events). Same information flow, RPC-shaped. The server remains a referee holding "latest" slots — no queues, no simulation.

---

## Rules for you (the implementing session) — read first

1. **Build ONE stage at a time. After each stage: stop, tell me how to run it, and wait for my confirmation that the checkpoint passes before writing any code for the next stage.** Do not implement ahead, do not "also add" features from later stages, even small ones. If I say "continue," move to the next stage.
2. **Every stage must end in something I can run and feel in under 2 minutes.** If a stage's output can't be demonstrated by running it, the stage is wrong. **Commit at every passed checkpoint** — the fallback ladder (see bottom) only works if each rung is a commit.
3. **Programmer art only until Stage 8.** Bird = colored square (~30×30). Pipes = green rectangles. Bullets = small circles. Item boxes = yellow squares with "?". Ground = brown rectangle. The collision box IS the drawn shape — no separate visual/collision bounds until final skinning. Do not add sprites, gradients, particles, or animations before Stage 8, even if asked-adjacent.
4. **All tunable numbers live in ONE `Config` module** (`lib/game/src/config.ml`, plain named constants) from the first line of Stage 0: gravity, flap impulse, terminal velocity, speed floor/cap/cruise, accel/brake ramp times, pipe gap size, gap max vertical delta, pipe spacing, course length, respawn time, i-frame duration, bullet speed and heights, effect durations, tick rate. Never bury a magic number in logic. When I ask for a feel change, change constants first, logic only if constants can't do it.
5. **Fixed project structure (jsip-exchange dune conventions) — do not add libs or dirs without asking:**
   - `lib/game/src/` — **pure** game logic, `core` only, no Async, no jsoo: `config.ml`, physics step, seeded course generation, respawn safe-snap, bullet trajectories. Deterministic → runs identically under jsoo (client) and native (tests).
   - `lib/game/test/` — expect tests for the pure logic (course fairness bound, completability-at-max-speed, safe-snap never inside a pipe, bullet-gap property).
   - `lib/protocol/src/` — wire types + RPC definitions (`core` + `async_rpc_kernel` only, like `lib/dashboard_protocol`).
   - `app/server/bin/` — native Async binary: `Rpc_websocket.Rpc.serve`, referee state, static file handler.
   - `app/client/bin/` — jsoo executable (`main.ml` = one line of `Bonsai_web.Start.start`).
   - `app/client/src/` — Bonsai app: view/HUD/screens in Bonsai + `ppx_html`; the game world on a canvas element driven by a `requestAnimationFrame` fixed-timestep loop (game state in a ref outside the Bonsai graph — Bonsai owns the shell, not the 60 Hz physics).
   - `app/client/static/index.html`
   No other frameworks, no npm, no TypeScript, no bundlers. `dune` is the whole build.
6. **Debug overlay from Stage 0, never removed:** x, y, vertical velocity, horizontal speed, state (alive/dead/invuln), held item, last RPC response received, and ms since last successful sync. Drawn on the canvas (not Bonsai — it updates at 60 Hz). Toggle with backtick.
7. **Hardcoded seed (`Config.debug_seed = 42`) until Stage 7.** Same course every run so tuning changes are comparable and generation bugs aren't masked by randomness. Seeding: `Random.State.make [| seed |]` — OCaml's PRNG is pure OCaml, so jsoo and native produce identical courses.
8. If you hit an ambiguity the context doc doesn't settle, ask me — do not invent a mechanic.
9. **Write Jane Street-standard OCaml.** Follow `/home/ubuntu/jsip-exchange/CLAUDE.md` conventions throughout: `open! Core` everywhere (plus `open! Async` server-side); every module gets an `.mli` with `(** doc *)` comments; `[%string]` not `sprintf`; derive `sexp_of`; `Or_error.t` at RPC boundaries, `_exn` suffix for raising functions; no `helpers.ml`; no `| _ ->` on variants; expect tests in `lib/<x>/test/` named `test_<module>.ml`; mirror jsip-exchange's dune stanzas (`ppx_jane`, `(inline_tests)` in test libs). Format with `dune fmt --auto-promote` before every checkpoint.

---

## Stage 0 — Skeleton: a square that flaps
**Build:** dune project skeleton (all dirs from rule 5, protocol lib empty for now). `lib/game`: `Config` + physics step (gravity, flap impulse, terminal velocity) as pure functions. `app/client`: Bonsai shell mounting a canvas; rAF fixed-timestep loop stepping the pure physics; spacebar flaps; debug overlay. `app/server`: the `Rpc_websocket.Rpc.serve` skeleton from the dashboard server — **zero RPCs yet**, it exists only to serve `index.html` + `main.bc.js` (this front-loads the serving story so "open the game" is one command from day one). No pipes, no scrolling, square can fall off screen harmlessly.
**Run:** `dune build` then `dune exec app/server/bin/main.exe -- -port 8080` → open `http://localhost:8080`.
**Checkpoint:** I open the page, press space, the square hops. Debug overlay shows live values. `dune runtest` passes (a first trivial expect test on the physics step, proving the test harness wiring works).

## Stage 1 — Feel tuning pass
**Build:** nothing new. Iterate on `Config` gravity / flap impulse / terminal velocity with me. Add a simple ground rectangle (landing on it just stops the square for now — death comes later).
**Checkpoint:** I say "the flap feels right." We do not proceed on floaty or twitchy physics — every later feature inherits this.

## Stage 2 — Horizontal speed control
**Build:** world scrolling (square stays ~1/3 from left, world moves). Right arrow accelerates toward `speed_cap`, left brakes toward `speed_floor` (~45% of cap), with `accel_ramp_ms` easing — no instant snaps. Implement BOTH control schemes behind `Config.control_scheme : Control_scheme.t` (variant: `Hold | Set`):
- `Hold`: speed decays toward `cruise_speed` when no key held
- `Set`: arrows adjust a persistent speed setting
Distance counter on the debug overlay so speed is perceivable.
**Checkpoint:** I try both schemes and pick one. Record the choice in `Config`.

## Stage 3 — Seeded course, death, respawn, finish line
**Build:**
- Course generation from seed in `lib/game` (pure): N pipe pairs; each gap's vertical center within `gap_max_delta` of the previous, where `gap_max_delta` is derived from max achievable height change between pipes AT `speed_cap` (course must be completable at full speed).
- **Variable pipe spacing (spec addition):** spacing between pipes NEVER drops below the baseline minimum (never harder), but ~30% of gaps draw from a much wider "breather" range — deliberate easy stretches. Stage 6 places item boxes preferentially in breathers, so grabbing a power-up leaves room to recover.
- Pipe collision → death: square tumbles (kill upward velocity, fall), 2s dead, respawn at nearest safe x (nearest gap-center with no pipe overlap — course is known, compute it), mid-height, 1.5s invulnerability (blink the square).
- Ground collision = death too.
- Finish line at course end; crossing it shows a win screen (Bonsai) + restart key.
- Expect tests: fairness bound holds for many seeds; safe-snap never lands inside geometry.
**Checkpoint:** I can play a full single-player race on seed 42, die, respawn safely, finish. `dune runtest` green. **This commit is the fallback demo.**

## Stage 4 — Networking on localhost
**Build:** `lib/protocol` for real, then wire both sides. **Finalize the RPC signatures with me BEFORE implementing — this is the "define the protocol before the team splits" moment.** Draft to start from (shapes mirror `rpc_protocol.ml`):

| RPC | Type | Purpose |
|---|---|---|
| `join_rpc : string -> Player_id.t Or_error.t` | `Rpc.Rpc` | enter lobby (≈ `login_rpc`) |
| `sync_rpc : Client_update.t -> Server_view.t` | `Rpc.Rpc`, polled at `Config.tick_rate_hz` (25) | client sends `{ pos; last_seen_event }`; response carries opponent's last-known pos, race state (lobby/countdown/racing/finished), and events newer than `last_seen_event` |

Server referee state: two latest-pos slots, a monotonically numbered event log, race state machine. When both players have joined → race state becomes `Countdown { seed; start_at }` in the next sync responses → both clients generate the identical course. A third `join` gets `Or_error.error_s` ("race in progress") — a public server WILL see a third visitor.
Client: opponent rendered as a 55%-opacity square, **interpolated** toward its last received position (never snapped). Off-screen opponent: edge arrow + distance. Progress bar (Bonsai HUD) across the top with both markers.
**Run instructions required:** one server command, two browser tabs at `http://localhost:8080`.
**Checkpoint:** two tabs on my laptop race the same course; each tab sees the other's ghost moving smoothly; debug overlay shows sync round-trips at ~25 Hz. Do NOT write any EC2/deployment material yet.

## Stage 5 — Race flow + EC2 deploy
**Build:** synchronized countdown (3-2-1 driven by race state in sync responses), `finish` (event from client) → server declares first finisher → `Winner` race state → result screen → restart resets both clients to a fresh race (same hardcoded seed for now). Handle disconnect mid-race (WebSocket close / sync timeouts): other player gets "opponent left — you win by default", server resets to lobby.
**Then deploy:** `dune build --profile release`, run `app/server/bin/main.exe` on the EC2 box (this dev machine IS an AWS Ubuntu box — likely zero copying needed), open port 8080 in the security group (context doc §5). **No client change at all:** `Rpc_effect.Where_to_connect.self` connects back to whatever origin served the page, and same-origin http+ws means the mixed-content trap can't happen. Players browse to `http://<public-ip>:8080`.
**Checkpoint:** two machines race over the internet end to end, including a full restart cycle.

## Stage 6 — Item boxes + arbitration (items do nothing yet)
**Build:** item-box positions derived from seed (pure, `lib/game`). Touch box → `pickup_request_rpc : Box_id.t -> Item.t option Or_error.t` (`Rpc.Rpc`, ≈ `submit_order_rpc`) → server first-come-first-served on claimed IDs → response `Some item` to the winner, `None` to a loser; a `Powerup_claimed { box; player; item }` event enters the log so both clients despawn the box and the winner's HUD slot (Bonsai) shows the item (server picks uniformly random from the four). One held item max; further boxes ignored while holding. Press use-key → `use_powerup_rpc` → server appends the effect event → for now both clients just log/flash it. No effects yet.
**Checkpoint:** both players grab boxes over the network, simultaneous-grab gives it to exactly one player (the arbitration RPC guarantees it), HUD shows held item, use-key consumes it visibly.

## Stage 7 — Power-ups, ONE AT A TIME (four sub-stages, checkpoint after each)
Implement in this exact order — it is dependency-sorted. Playtest between each; do not batch. All effect propagation rides the existing event log; no new RPC shapes.

- **7a. Speed boost:** timed effect that raises `speed_cap` for `boost_ms`. This builds the generic timed-effect system (apply/expire/HUD indicator) everything else reuses.
- **7b. Shield:** held state that intercepts exactly one lethal/negative event (pipe hit, ground hit from knockdown, bullet, incoming swap), consumes itself, brief flash. Builds the hit-interception hook.
- **7c. Bullet volley:** `Fire_volley { x; y; t }` event → BOTH clients simulate 5 deterministic straight-line bullets locally in `lib/game` (speed `bullet_speed` ≈ 1.8× `speed_cap`) at 5 fixed heights from `Config` leaving 2–3 bird-sized gaps (expect-test the gap property); bullets pass through pipes; each client checks hits ONLY against its own bird; a hit applies hard downward impulse (existing ground-death handles the rest) unless shielded/invulnerable. Visual warning flash for the target when fired. No per-frame bullet network sync — ever.
- **7d. Swap places:** `use_powerup_rpc (Swap)` → server emits `Swap { pos_a; pos_b }` event using last-known positions → both clients teleport their own bird to the other's position on receipt, both get 1.5s i-frames, and if a bird materializes overlapping a pipe, snap to nearest safe x (reuse respawn logic). Shield blocks it: the shielded client, on seeing the `Swap` event, doesn't teleport and reports a `Swap_blocked` event — the other client reverts on receipt (accepted flicker; simplest acceptable handling — revisit at this checkpoint only if it feels bad).
- **Also in this stage:** switch `Config.debug_seed` to server-generated random-per-race (still shown in the debug overlay for reproducing bugs).
**Checkpoint (each sub-stage):** we play a race using that item and I confirm it feels fair before you start the next one.

## Stage 8 — Juice (only if time remains)
Strictly after everything above works: death screen-shake, shield-break flash, boost speed-lines/camera stretch, bird tilt with vertical velocity, parallax background, then (last) sprite skins over the same collision rects. Rubber-banded item odds (weighted random favoring the trailing player, server-side, one function) also lives here.
**Checkpoint:** none — this stage is interruptible at any point and the game must remain shippable between every individual change.

---

## Definition of done for the day
Stage 5 complete = shippable. Stage 6–7 = the real game. Stage 8 = gravy. If we are behind schedule at any point, the fallback ladder is: cut 8, then 7d, then 7c, then ship whatever stage last passed its checkpoint (every checkpoint is a commit — rule 2).

## Known risk added by the OCaml/Bonsai stack (accepted)
- Build/iteration loop is `dune build` + browser refresh instead of just refresh — slower feel-tuning cycles at Stages 1–2.
- 25 Hz polling `sync_rpc` replaces server-push; opponent staleness becomes ~(poll interval + RTT)/2 — still display-only, still fine, but if the ghost feels choppy the knob is `Config.tick_rate_hz`, not architecture.
- rAF canvas loop lives outside Bonsai's incremental graph by design; only HUD/screens are Bonsai. Do not try to run 60 Hz physics through Bonsai state — that's this stack's version of the context doc's networking anti-goal.
- If jsoo/Bonsai integration stalls badly at Stage 0–1, escalate immediately — the fallback is NOT to silently switch stacks.
