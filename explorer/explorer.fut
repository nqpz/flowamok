import "../lib/github.com/diku-dk/lys/lys"
import "../flowamok"

module explorer_scenario_helper = import "explorer_scenario_helper"
module scenario = explorer_scenario_helper.mk_scenario_helper_exposed {
  open explorer_scenario_helper
  open import "scenarios"

  module S00 = mk_scenario_helper single_fork
  module S01 = mk_scenario_helper tight_queues
  module S02 = mk_scenario_helper basic_cycle
  module S03 = mk_scenario_helper crossing
  module S04 = mk_scenario_helper close_crossing
  module S05 = mk_scenario_helper crossroads
  module S06 = mk_scenario_helper small_tight_cycle
  module S07 = mk_scenario_helper independent_tight_cycles
  module S08 = mk_scenario_helper overlapping_tight_cycles
  module S09 = mk_scenario_helper hits_an_edge
  module S10 = mk_scenario_helper adding_lines
  module S11 = mk_scenario_helper multi_spill
  module S12 = mk_scenario_helper (mk_many_cycles { def n = 2i64 })
  module S13 = mk_scenario_helper t_cross

  def scenario_helper [gh][gw] (sid: i64)
                      (a: *[gh][gw]cell) (b: *[gh][gw]cell) (c: i64) (d: rng):
                      helper_out [gh] [gw] [] =
    match sid
    case 00 -> S00.helper a b c d
    case 01 -> S01.helper a b c d
    case 02 -> S02.helper a b c d
    case 03 -> S03.helper a b c d
    case 04 -> S04.helper a b c d
    case 05 -> S05.helper a b c d
    case 06 -> S06.helper a b c d
    case 07 -> S07.helper a b c d
    case 08 -> S08.helper a b c d
    case 09 -> S09.helper a b c d
    case 10 -> S10.helper a b c d
    case 11 -> S11.helper a b c d
    case 12 -> S12.helper a b c d
    case 13 -> S13.helper a b c d
    case _ -> (a, (d, false, b), "")
}

type cell = cell ()

-- FIXME: Maybe this should be adjustable in some fashion.
def gh = 108i64
def gw = 192i64

type text_content = (i32, i32, i32, i32, i64, i32, i64, i64)
module lys: lys with text_content = text_content = {
  type~ state =
    {h: i64, w: i64,
     scale: i64,
     auto: bool,
     rng: rng,
     time_unused: f32,
     steps_auto_per_second: i32,
     steps: []i64,
     grids: [][][]cell,
     perfect_simulation: bool,
     cycle_checks: [][][](direction flow),
     n_cell_leaks: i64,
     scenario_id: i64}

  type text_content = text_content

  def text_format () =
    let scenario_names =
      loop s = scenario.name 0
      for i < scenario.n_scenarios - 1
      do s ++ "|" ++ scenario.name (i + 1)
    in "FPS: %d\nScenario: %[" ++ scenario_names ++ "]\nAuto: %[no|yes]\nSteps per second: %d\nSteps: %ld\nPerfect: %[false|true]\nCycles detected: %ld\nCell leaks this step: %ld"

  def text_content (fps: f32) (s: state): text_content =
    (t32 fps, i32.i64 s.scenario_id, i32.bool s.auto,
     s.steps_auto_per_second, s.steps[s.scenario_id],
     i32.bool s.perfect_simulation,
     length s.cycle_checks, s.n_cell_leaks)

  def text_colour = const argb.white

  def init_grid (gh: i64) (gw: i64) (rng: rng) (scenario_id: i64): (rng, [gh][gw]cell) =
    let (rng, grid) = create_grid gh gw () rng
    let grid = scenario.init scenario_id grid
    in (rng, grid)

  def init (seed: u32) (h: i64) (w: i64): state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let rngs = rnge.split_rng scenario.n_scenarios rng
    let (rngs, grids) = unzip (map2 (init_grid gh gw) rngs (0..<scenario.n_scenarios))
    let perfect_simulation = true
    let rng = rnge.join_rng rngs
    let scenario_id = 0
    let cycle_checks = if perfect_simulation
                       then stepper_perfect.find_cycles grids[scenario_id]
                       else []
    in {h, w, scale=10, auto=false, rng, time_unused=0,
        steps_auto_per_second=30, steps=replicate scenario.n_scenarios 0,
        grids, perfect_simulation, cycle_checks, n_cell_leaks=0, scenario_id}

  def grab_mouse = false

  def resize (h: i64) (w: i64) (s: state): state =
    s with h = h
      with w = w

  def step (s: state) (n_steps: i64): state =
    let grids = copy s.grids :> *[scenario.n_scenarios][gh][gw]cell
    let cycle_checks = s.cycle_checks :> [][gh][gw](direction flow)
    let steps = copy s.steps
    let (rng, grid, cycle_checks, step_cur, n_cell_leaks) =
      loop (rng, grid, cycle_checks, step_cur, n_cell_leaks) =
        (s.rng, copy grids[s.scenario_id], copy cycle_checks, copy steps[s.scenario_id], 0)
      for _i < n_steps
      do let (grid, cell_leaks) =
           if s.perfect_simulation
           then stepper_perfect.step gh gw choose_direction_random cycle_checks () grid
           else stepper_quick.step gh gw choose_direction_random () grid
         let (rng, recompute_cycles, grid) = scenario.step s.scenario_id grid step_cur rng
         let cycle_checks = if recompute_cycles && s.perfect_simulation
                            then copy (stepper_perfect.find_cycles grid)
                            else cycle_checks
         in (rng, grid, cycle_checks, step_cur + 1, n_cell_leaks + length cell_leaks)
    in s with rng = rng
         with steps = (steps with [s.scenario_id] = step_cur)
         with grids = (grids with [s.scenario_id] = grid)
         with cycle_checks = cycle_checks
         with n_cell_leaks = n_cell_leaks

def event (e: event) (s: state): state =
    match e
    case #step td ->
      if s.auto
      then let time_total = td + s.time_unused
           let steps_new = time_total * f32.i32 s.steps_auto_per_second
           let steps_new' = i64.f32 steps_new
           let time_unused = time_total - f32.i64 steps_new' / f32.i32 s.steps_auto_per_second
           let s = if steps_new' > 0
                   then step s steps_new'
                   else s
           in s with time_unused = time_unused
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
           let cycle_checks = if s.perfect_simulation
                              then stepper_perfect.find_cycles s.grids[scenario_id]
                              else s.cycle_checks
           in s with scenario_id = scenario_id
                with cycle_checks = cycle_checks
                with n_cell_leaks = 0
      else if key == SDLK_RIGHT
      then let scenario_id = (s.scenario_id + 1) % scenario.n_scenarios
           let cycle_checks = if s.perfect_simulation
                              then stepper_perfect.find_cycles s.grids[scenario_id]
                              else s.cycle_checks
           in s with scenario_id = scenario_id
                with cycle_checks = cycle_checks
      else if key == SDLK_a
      then s with auto = !s.auto
      else if key == SDLK_p
      then let s = s with perfect_simulation = !s.perfect_simulation
           in if s.perfect_simulation
              then s with cycle_checks = stepper_perfect.find_cycles s.grids[s.scenario_id]
              else s
      else if key == SDLK_UP
      then s with steps_auto_per_second = s.steps_auto_per_second + 1
      else if key == SDLK_DOWN
      then s with steps_auto_per_second = i32.max 1 (s.steps_auto_per_second - 1)
      else if key == SDLK_r
      then let (rng, grid) = init_grid gh gw s.rng s.scenario_id
           let grids = copy s.grids :> *[scenario.n_scenarios][gh][gw]cell
           let grids[s.scenario_id] = grid
           let cycle_checks = if s.perfect_simulation
                              then stepper_perfect.find_cycles grid
                              else s.cycle_checks
           in s with rng = rng
                with grids = grids
                with cycle_checks = cycle_checks
                with steps = (copy s.steps with [s.scenario_id] = 0)
                with auto = false
      else if key == SDLK_1
      then s with scale = s.scale + 1
      else if key == SDLK_2
      then s with scale = i64.max 1 (s.scale - 1)
      else s
    case _ ->
      s

  def render (s: state): [][]argb.colour =
    render s.h s.w gh gw s.scale (s.grids[s.scenario_id] :> [gh][gw]cell)
}
