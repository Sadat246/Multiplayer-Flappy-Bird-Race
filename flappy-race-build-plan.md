# BUILD PLAN: Multiplayer Flappy Racer — Staged Implementation Instructions

Use this alongside the context doc (`flappy-race-context-prompt.md`). The context doc says WHAT we're building; this doc says HOW and IN WHAT ORDER. Paste both into the build session.

---

## Rules for you (the implementing session) — read first

1. **Build ONE stage at a time. After each stage: stop, tell me how to run it, and wait for my confirmation that the checkpoint passes before writing any code for the next stage.** Do not implement ahead, do not "also add" features from later stages, even small ones. If I say "continue," move to the next stage.
2. **Every stage must end in something I can run and feel in under 2 minutes.** If a stage's output can't be demonstrated by running it, the stage is wrong.
3. **Programmer art only until Stage 8.** Bird = colored square (~30×30). Pipes = green rectangles. Bullets = small circles. Item boxes = yellow squares with "?". Ground = brown rectangle. The collision box IS the drawn shape — no separate visual/collision bounds until final skinning. Do not add sprites, gradients, particles, or animations before Stage 8, even if asked-adjacent.
4. **All tunable numbers live in ONE `CONFIG` object** (shared `config.js`) from the first line of Stage 0: gravity, flap impulse, terminal velocity, speed floor/cap/cruise, accel/brake ramp times, pipe gap size, gap max vertical delta, pipe spacing, course length, respawn time, i-frame duration, bullet speed and heights, effect durations, tick rate. Never bury a magic number in logic. When I ask for a feel change, change constants first, logic only if constants can't do it.
5. **Fixed file structure — do not add files or folders without asking:**
   - `client/index.html` (canvas + script tags, minimal)
   - `client/game.js` (game loop, physics, rendering, input)
   - `client/net.js` (WebSocket connection + message handlers, no game logic)
   - `server/server.js` (Node + `ws`)
   - `shared/config.js` (the CONFIG object, used by both sides)
   No classes-and-folders architecture, no bundlers, no frameworks, no TypeScript. Plain JS, plain canvas.
6. **Debug overlay from Stage 0, never removed:** top-left text showing x, y, vertical velocity, horizontal speed, state (alive/dead/invuln), held item, last network message type received, and ms since last opponent update. Toggle with backtick.
7. **Hardcoded seed (`CONFIG.debugSeed = 42`) until Stage 7.** Same course every run so tuning changes are comparable and generation bugs aren't masked by randomness.
8. If you hit an ambiguity the context doc doesn't settle, ask me — do not invent a mechanic.

---

## Stage 0 — Skeleton: a square that flaps
**Build:** `index.html` + `game.js` + `config.js`. Canvas, fixed-timestep game loop, one square with gravity, spacebar applies flap impulse, terminal velocity cap, debug overlay. No pipes, no scrolling, square can fall off screen harmlessly.
**Checkpoint:** I open index.html, press space, the square hops. Debug overlay shows live values.

## Stage 1 — Feel tuning pass
**Build:** nothing new. Expose CONFIG so I can iterate on gravity / flap impulse / terminal velocity with you. Add a simple ground rectangle (landing on it just stops the square for now — death comes later).
**Checkpoint:** I say "the flap feels right." We do not proceed on floaty or twitchy physics — every later feature inherits this.

## Stage 2 — Horizontal speed control
**Build:** world scrolling (square stays ~1/3 from left, world moves). Right arrow accelerates toward `speedCap`, left brakes toward `speedFloor` (~45% of cap), with `accelRampMs` easing — no instant snaps. Implement BOTH control schemes behind `CONFIG.controlScheme: "hold" | "set"`:
- `hold`: speed decays toward `cruiseSpeed` when no key held
- `set`: arrows adjust a persistent speed setting
Distance counter on the debug overlay so speed is perceivable.
**Checkpoint:** I try both schemes and pick one. Record the choice in CONFIG.

## Stage 3 — Seeded course, death, respawn, finish line
**Build:**
- Seeded PRNG (tiny mulberry32-style, in config.js or game.js — no library).
- Course generation from seed: N pipe pairs; each gap's vertical center within `gapMaxDelta` of the previous, where `gapMaxDelta` is derived from max achievable height change between pipes AT `speedCap` (course must be completable at full speed).
- Pipe collision → death: square tumbles (kill upward velocity, fall), 2s dead, respawn at nearest safe x (nearest gap-center with no pipe overlap — course is known, compute it), mid-height, 1.5s invulnerability (blink the square).
- Ground collision = death too.
- Finish line at course end; crossing it shows a win screen + restart key.
**Checkpoint:** I can play a full single-player race on seed 42, die, respawn safely, finish. **This build gets saved/committed as the fallback demo.**

## Stage 4 — Networking on localhost
**Build:** `server/server.js` (Node + `ws`, JSON messages) and `client/net.js`. Flow: two clients connect → both send `join` → server sends `start(seed, countdownMs)` → both generate identical course → clients send `pos(x,y)` at `CONFIG.tickRate` (25 Hz) → server relays to the other client → opponent rendered as a 55%-opacity square, **interpolated** toward last received position (never snapped). Off-screen opponent: edge arrow + distance. Progress bar across the top with both markers.
**Run instructions required:** how to start the server locally and open two tabs at `ws://localhost:8080`.
**Checkpoint:** two tabs on my laptop race the same course; each tab sees the other's ghost moving smoothly. Do NOT write any EC2/deployment material yet.

## Stage 5 — Race flow + EC2 deploy
**Build:** synchronized countdown (3-2-1 from server `start`), `finish` → server declares first finisher → `winner` broadcast → result screen → `restart` resets both clients to a fresh race (same hardcoded seed for now). Handle disconnect mid-race: other player gets a "opponent left — you win by default" screen, server resets to lobby.
**Then deploy:** exact steps to run the same server.js on our EC2 Ubuntu box (node install if needed, run command, security-group port note from context doc §5), and the one-line client change (`ws://<public-ip>:8080`). Nothing else changes.
**Checkpoint:** two machines race over EC2 end to end, including a full restart cycle.

## Stage 6 — Item boxes + arbitration (items do nothing yet)
**Build:** item-box positions derived from seed. Touch box → `pickup_request(boxId)` → server first-come-first-served on claimed IDs → `powerup_claimed(boxId, player, item)` → box despawns on both clients, winner's HUD slot shows the item (server picks uniformly random from the four). One held item max; further boxes ignored while holding. Press use-key → `use_powerup` → server broadcasts → for now both clients just log/flash it. No effects yet.
**Checkpoint:** both players grab boxes over the network, simultaneous-grab gives it to exactly one player, HUD shows held item, use-key consumes it visibly.

## Stage 7 — Power-ups, ONE AT A TIME (four sub-stages, checkpoint after each)
Implement in this exact order — it is dependency-sorted. Playtest between each; do not batch.

- **7a. Speed boost:** timed effect that raises `speedCap` for `boostMs`. This builds the generic timed-effect system (apply/expire/HUD indicator) everything else reuses.
- **7b. Shield:** held state that intercepts exactly one lethal/negative event (pipe hit, ground hit from knockdown, bullet, incoming swap), consumes itself, brief flash. Builds the hit-interception hook.
- **7c. Bullet volley:** `fire_volley(x, y, t)` → server broadcasts → BOTH clients simulate 5 deterministic straight-line bullets locally (speed `bulletSpeed` ≈ 1.8× speedCap) at 5 fixed heights from CONFIG leaving 2–3 bird-sized gaps; bullets pass through pipes; each client checks hits ONLY against its own bird; a hit applies hard downward impulse (existing ground-death handles the rest) unless shielded/invulnerable. Audio-free warning flash for the target when fired. No per-frame bullet network sync — ever.
- **7d. Swap places:** `use_powerup(swap)` → server responds with `swap(posA, posB)` using last-known positions → both clients teleport their own bird to the other's position simultaneously, both get 1.5s i-frames, and if a bird materializes overlapping a pipe, snap to nearest safe x (reuse respawn logic). Shield on either player blocks it (server checks nothing; the shielded client sends a `swap_blocked` broadcast — simplest acceptable handling).
- **Also in this stage:** switch `CONFIG.debugSeed` to random-per-race (server generates, still logged in the debug overlay for reproducing bugs).
**Checkpoint (each sub-stage):** we play a race using that item and I confirm it feels fair before you start the next one.

## Stage 8 — Juice (only if time remains)
Strictly after everything above works: death screen-shake, shield-break flash, boost speed-lines/camera stretch, bird tilt with vertical velocity, parallax background, then (last) sprite skins over the same collision rects. Rubber-banded item odds (weighted random favoring the trailing player, server-side, one function) also lives here.
**Checkpoint:** none — this stage is interruptible at any point and the game must remain shippable between every individual change.

---

## Definition of done for the day
Stage 5 complete = shippable. Stage 6–7 = the real game. Stage 8 = gravy. If we are behind schedule at any point, the fallback ladder is: cut 8, then 7d, then 7c, then ship whatever stage last passed its checkpoint.
