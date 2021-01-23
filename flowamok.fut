import "lib/github.com/diku-dk/lys/lys"
import "model"
module scenario = import "scenarios"

let scenario_init [h] [w] (sid: i64) (grid: *[h][w]cell): *[h][w]cell =
  match sid
  case 0 -> scenario.single_fork.init grid
  case 1 -> scenario.tight_queues.init grid
  case 2 -> scenario.basic_cycle.init grid
  case 3 -> scenario.crossing.init grid
  case 4 -> scenario.close_crossing.init grid
  case 5 -> scenario.crossroads.init grid
  case 6 -> scenario.small_tight_cycle.init grid
  case 7 -> scenario.independent_tight_cycles.init grid
  case 8 -> scenario.overlapping_tight_cycles.init grid
  case 9 -> scenario.hits_an_edge.init grid
  case _ -> grid

let scenario_step [h] [w] (sid: i64) (grid: *[h][w]cell) (steps: i64) (rng: rng): (rng, *[h][w]cell) =
  match sid
  case 0 -> scenario.single_fork.step grid steps rng
  case 1 -> scenario.tight_queues.step grid steps rng
  case 2 -> scenario.basic_cycle.step grid steps rng
  case 3 -> scenario.crossing.step grid steps rng
  case 4 -> scenario.close_crossing.step grid steps rng
  case 5 -> scenario.crossroads.step grid steps rng
  case 6 -> scenario.small_tight_cycle.step grid steps rng
  case 7 -> scenario.independent_tight_cycles.step grid steps rng
  case 8 -> scenario.overlapping_tight_cycles.step grid steps rng
  case 9 -> scenario.hits_an_edge.step grid steps rng
  case _ -> (rng, grid)

let s1 +++ s2 = s1 ++ "|" ++ s2
let scenario_names =
  "single fork"
  +++ "tight queues"
  +++ "basic cycle"
  +++ "crossing"
  +++ "close crossing"
  +++ "crossroads"
  +++ "small tight cycle"
  +++ "independent tight cycles"
  +++ "overlapping tight cycles"
  +++ "hits an edge"

-- FIXME: Maybe this should be adjustable in some fashion.
let scale = 10i64
let gh = 108i64
let gw = 192i64

type text_content = (i32, i32, i32, i32, i64)
module lys: lys with text_content = text_content = {
  type~ state =
    {h: i64, w: i64,
     auto: bool,
     rng: rng,
     time_unused: f32,
     steps_auto: i64,
     steps_auto_per_second: i32,
     steps: i64,
     grids: [][][]cell,
     cycle_checks: [][][](direction flow),
     scenario_id: i64}

  type text_content = text_content

  let text_format () = "FPS: %d\nScenario: %[" ++ scenario_names ++ "]\nAuto: %[no|yes]\nSteps per second: %d\nCycles detected: %ld"

  let text_content (fps: f32) (s: state): text_content =
    (t32 fps, i32.i64 s.scenario_id, i32.bool s.auto, s.steps_auto_per_second, length s.cycle_checks)

  let text_colour = const argb.white

  let init_grid (h: i64) (w: i64) (rng: rng) (scenario_id: i64): (rng, [h][w]cell) =
    let (rng, grid) = create_grid h w rng
    let grid = scenario_init scenario_id grid
    in (rng, grid)

  let init (seed: u32) (h: i64) (w: i64): state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let rngs = rnge.split_rng scenario.n_scenarios rng
    let (rngs, grids) = unzip (map2 (init_grid gh gw) rngs (0..<scenario.n_scenarios))
    let rng = rnge.join_rng rngs
    let scenario_id = 0
    let cycle_checks = find_cycles grids[scenario_id]
    in {h, w, auto=false, rng, time_unused=0,
        steps_auto=0, steps_auto_per_second=30, steps=0,
        grids, cycle_checks, scenario_id}

  let grab_mouse = false

  let resize (h: i64) (w: i64) (s: state): state =
    s with h = h
      with w = w

  let step (s: state) (n_steps: i64): state =
    let grids = copy s.grids :> *[scenario.n_scenarios][gh][gw]cell
    let grid = copy grids[s.scenario_id]
    let cycle_checks = s.cycle_checks :> [][gh][gw](direction flow)
    let grid = loop grid for _i < n_steps do step gh gw cycle_checks grid
    let (rng, grid) = scenario_step s.scenario_id grid s.steps s.rng
    let grids[s.scenario_id] = grid
    in s with rng = rng
         with steps = s.steps + n_steps
         with grids = grids

let event (e: event) (s: state): state =
    match e
    case #step td ->
      if s.auto
      then let time_total = td + s.time_unused
           let steps_new = time_total * f32.i32 s.steps_auto_per_second
           let steps_new' = i64.f32 steps_new
           let time_unused = time_total - f32.i64 steps_new' / f32.i32 s.steps_auto_per_second
           let s = step s steps_new'
           in s with time_unused = time_unused
                with steps_auto = s.steps_auto + steps_new'
      else s
    case #keydown {key} ->
      if key == SDLK_SPACE
      then step s 1 with auto = false
      else if key == SDLK_LEFT
      then let scenario_id = (s.scenario_id - 1) % scenario.n_scenarios
           -- FIXME: This is recalculated on every move to a different scenario,
           -- which is potentially slow.  But it's hard to really fix this in a
           -- satisfying way, since different grids can have a different number
           -- of cycles, and since we may need to redo cycle detection after
           -- stepping, which could also change the number of cycles.
           let cycle_checks = find_cycles s.grids[scenario_id]
           in s with scenario_id = scenario_id
                with cycle_checks = cycle_checks
      else if key == SDLK_RIGHT
      then let scenario_id = (s.scenario_id + 1) % scenario.n_scenarios
           let cycle_checks = find_cycles s.grids[scenario_id]
           in s with scenario_id = scenario_id
                with cycle_checks = cycle_checks
      else if key == SDLK_a
      then s with auto = !s.auto
      else if key == SDLK_UP
      then s with steps_auto_per_second = s.steps_auto_per_second + 1
      else if key == SDLK_DOWN
      then s with steps_auto_per_second = i32.max 1 (s.steps_auto_per_second - 1)
      else if key == SDLK_r
      then let (rng, grid) = init_grid gh gw s.rng s.scenario_id
           let grids = copy s.grids :> *[scenario.n_scenarios][gh][gw]cell
           let grids[s.scenario_id] = grid
           let cycle_checks = find_cycles grid
           in s with rng = rng
                with grids = grids
                with cycle_checks = cycle_checks
                with auto = false
      else s
    case _ ->
      s

  let render (s: state): [][]argb.colour =
    render s.h s.w gh gw scale (s.grids[s.scenario_id] :> [gh][gw]cell)
}
