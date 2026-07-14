# CONTEXT PROMPT: Multiplayer Flappy Racer — Planning Session

Copy everything below into a new session.

---

You are helping me **plan** (not build yet) a one-day project: a 2-player, Flappy Bird-inspired **racing game**. All major design decisions below have already been made in a previous session — treat them as settled unless I explicitly reopen one. Your job in this session is to help me turn this into a concrete plan: task breakdown, file/module structure, message schemas, team work-split, and a build order. **Do not one-shot the whole implementation.** We will plan first, then implement piece by piece.

## 1. Game concept

Two players on separate devices race through the **same randomly generated, finite obstacle course** to a finish line. Flappy Bird movement (constant forward motion, gravity, tap/space to flap) plus a twist: **players also control horizontal speed**. Each player views the world through their own camera. First to the finish line wins.

Key mechanics, all settled:

- **Vertical control:** standard flappy — gravity pulls down, spacebar flap gives an upward impulse. Hitting a pipe or the ground = death (see respawn rules).
- **Horizontal speed control:** right arrow accelerates toward a **max speed cap**, left arrow brakes toward a **minimum speed floor (~40–50% of max, never zero, never reverse)**. Speed changes use short acceleration ramps (~0.3–0.5s), not instant snaps — anticipation is the skill. Use case: brake to thread a tight pipe gap, floor it on open stretches. Control scheme (hold-to-boost with decay toward a cruise speed vs. persistent set-speed) is undecided — build it behind a variable, playtest both.
- **Players pass through each other.** No bird-vs-bird collision, ever. Opponent position is display-only and never gameplay-affecting (this is a deliberate networking decision — see §4). Render the opponent at ~50–60% opacity so overlapping birds are distinguishable.
- **Course generation:** deterministic from a **seed** the server sends both clients at race start. Both clients generate the identical course locally from the seed. Fairness rule: constrain each pipe gap's vertical center to within a max delta of the previous gap's center, where the delta is computed so the course is **completable at max horizontal speed** (then braking is pure assist and no section is ever impossible). Power-up spawn locations are also derived from the seed.
- **Race feel requirements:** opponent always perceivable — live interpolated ghost when on-screen, arrow + distance indicator when off-screen, and a **progress bar** across the top showing both birds' positions along the full course. These are essential, not polish: variable speed means players will frequently be far apart.
- **Race flow:** lobby/join → server sends seed + synchronized countdown → race → first `finish` message wins → result screen → restart option.

## 2. Death & respawn (all settled)

- Hitting a pipe, the ground, or being killed by a power-up = death.
- **Respawn after ~2 seconds** (tunable — start at 2s, not 5s; at a 60–90s race length, long respawns make single mistakes race-ending and make offensive power-ups snowball). Respawn guarantees the race always finishes.
- **Respawn position:** where the player died, snapped to a guaranteed-safe spot — mid-height at the nearest x with no pipe overlap ("nearest gap center"). Both clients know the full course from the seed, so this is computable locally. Never respawn inside geometry.
- **~1.5s of invulnerability on respawn** (flashing sprite). Dead and invulnerable players cannot be hit by anything.
- **Death is client-announced:** each client detects its *own* death (it simulates its own physics and bullet hits), sends `died(x, cause)` to the server, server broadcasts so the opponent sees the tumble. The server never verifies deaths — trust model is fine for a 2-player class project.
- Bullet hits don't need special death code: the bullet applies a hard downward impulse / kills vertical velocity, and the existing ground-collision death handles the rest. The knockdown animation IS the physics.

## 3. Power-ups (final list, settled)

**Acquisition:** shared item-box spawn points on the course (positions from the seed). Player holds **one item max**, shown large on the HUD, one dedicated key to use. Server picks the random item on pickup (enables later rubber-banding by weighting odds for the trailing player — stretch goal, one weighted-random function). Pickups are **server-arbitrated** so both players can't grab the same box: client touches box → `pickup_request(id)` → server checks unclaimed → broadcasts `powerup_claimed(id, player, item)` → both clients despawn the box. First request wins.

1. **Speed boost** — temporarily *raises the player's max speed cap* (nitro). Player keeps brake control during it. Purely local effect.
2. **Shield** — blocks **one hit of any kind** (pipe collision, bullet, and it also blocks being swapped), then breaks. One-hit-then-break, not timed. Doubles as an aggression tool: shield up = you can floor it through a section you'd normally brake for.
3. **Bullet volley (comeback item)** — fires **5 bullets forward at different fixed heights** that **pass through pipes** but knock any player they hit to the ground (downward impulse → existing death handling). Critical design constraint: the 5 heights must **leave 2–3 bird-sized gaps** in the spread — dodging a volley is a skill-shot/thread-the-gap interaction, not a guaranteed blue-shell hit. Bullet speed ~1.5–2x max player speed (dodge window of roughly 1–2s from typical trailing distance). Fired forward, so it's naturally the chaser's weapon. Victim gets audio/visual warning when fired.
4. **Swap places** — instantly swaps the two players' positions. Known-swingy by design (it's our chaos/comeback item). Implementation requirements: server broadcasts `swap` with both players' last-known positions; **both clients teleport simultaneously** to each other's positions; **both get ~1.5s post-swap invulnerability** (someone may materialize inside or adjacent to a pipe — i-frames plus, if needed, snap-to-nearest-safe-x using the same "nearest gap center" logic as respawn). Shield blocks a swap targeting you.

## 4. Networking architecture (settled — this is the important part)

**Model: fat clients, thin server. The server is a referee for shared events, never the physics simulation.**

- **Client owns:** its own bird's physics entirely (gravity, flapping, speed, pipe collisions, bullet hits on itself, its own death). Sends position updates at **20–30 Hz**, fire-and-forget, no acks.
- **Server owns (only):** the seed, power-up claim arbitration, relaying/broadcasting events between clients (positions, item use, deaths, swap), race state (countdown start, first-finish wins, restart).
- **Bullets are deterministic and never synced per-frame:** shooter sends `fire_volley(x, y, t)`, server broadcasts, both clients simulate the straight-line bullets locally. The victim's own client detects the hit on itself and self-reports. Same client-owns-own-fate pattern as deaths.
- **Opponent rendering:** interpolate the ghost smoothly toward its last received position — never snap/teleport it. Variable speed comes for free through interpolation.
- **Stack:** browser JS clients, **Node.js + `ws` (WebSocket) server, JSON messages**. No binary protocols, no RPC frameworks, no rooms/matchmaking libraries. The whole server is realistically 60–150 lines.
- **Anti-goal, do not drift toward this:** putting physics, collision, or "verification" on the server. That path leads to tick sync and lag compensation — the genuinely hard networking — and is unnecessary for a trusted 2-player game.

**Message protocol (draft — help me finalize exact JSON schemas early, before the team splits up; mismatched message shapes between teammates is the classic hackathon time-sink):**

| Message | Direction | Purpose |
|---|---|---|
| `join(name)` | client → server | enter lobby |
| `start(seed, countdown_ms)` | server → both | begin race, both generate course |
| `pos(x, y)` | client → server → other client | 20–30 Hz position, fire-and-forget |
| `pickup_request(box_id)` | client → server | claim an item box |
| `powerup_claimed(box_id, player, item)` | server → both | arbitration result, despawn box |
| `use_powerup(item, payload)` | client → server | activate held item |
| `fire_volley(x, y, t)` / `effect_applied(...)` / `swap(posA, posB)` | server → both | broadcast item effects |
| `died(x, cause)` | client → server → other | self-reported death |
| `finish(t)` | client → server | crossed the line; server declares winner |
| `winner(player)`, `restart` | server → both | race end / reset |

## 5. Deployment (EC2)

We have an EC2 instance (Ubuntu) and will connect clients directly to its public IP. Practical notes already learned:

- **Open the WebSocket port (e.g. 8080) in the EC2 security group** for inbound traffic — the #1 "why won't it connect" time-sink.
- Use plain **`ws://` + public IP**. No TLS, no domain — class project. Gotcha: pages served over `https` cannot open `ws://` connections (mixed content); serve the client page over plain http or open it from `file://`.
- The server also generates and distributes the seed at race start. Lazy option we're taking: clients derive item-box *positions* from the seed; the server only tracks claimed box IDs and never needs to know where anything is.

## 6. Reference: the JSIP exchange

If useful for architectural grounding, look at https://github.com/jane-street-immersion-program/jsip-exchange (a Jane Street teaching project: OCaml client/server stock exchange). We reviewed it and our game uses **the same skeleton at ~10% of the complexity**: a central server on a TCP port, clients connect, request/response calls for arbitration (their `submit_order_rpc` ≈ our `pickup_request`) and server-push event streams (their `market_data_rpc` pipe ≈ our position/effect broadcasts). Key differences to keep in mind: the exchange's server IS the simulation (matching engine, order book) with real correctness/ordering requirements; ours deliberately inverts that (simulation on clients, server referees shared bits only). Steal from it: the discipline of a small, explicitly documented message protocol defined before implementation. Do not steal from it: server-side simulation, queues/backpressure, binary serialization.

## 7. Build order (agreed, with fallbacks)

1. Single-player flappy: physics, speed control, seeded course gen, finish line. **(This is the fallback demo if networking dies.)**
2. Networking core: Node/ws server on EC2, two clients join, seed exchange, position broadcast, interpolated ghost, progress bar + off-screen indicator.
3. Race flow: countdown, finish, winner, restart.
4. Death/respawn system (safe-position snap, i-frames).
5. Item boxes + server-arbitrated pickup + HUD slot.
6. Power-ups in order of complexity: boost → shield → volley → swap.
7. Stretch: rubber-band item odds, moving obstacles, speed-readability juice (camera zoom, parallax, motion lines, bird tilt).

## 8. What I want from you in this session

- Ask clarifying questions where the design above is genuinely ambiguous, then help me produce: a module/file structure for client and server, finalized JSON message schemas, a physics constants sheet (gravity, flap impulse, speed floor/cap/accel, bullet speed, gap sizes) as a single tunable config, and a work-split for a small team against the build order above.
- Flag risks and edge cases (disconnect mid-race, both finish same tick, pickup race conditions, swap-into-pipe) and propose the simplest acceptable handling for each.
- Keep everything scoped to one day. When in doubt, propose the dumber option.
- Do **not** generate the full codebase unprompted. We'll implement stage by stage.
