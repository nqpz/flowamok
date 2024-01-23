import "../lib/github.com/diku-dk/lys/lys"
import "../flowamok"

type cell = cell ()

module type scenario_helper_exposed = {
  val name: i64 -> string []

  val init [gh][gw]: i64 -> *[gh][gw]cell -> *[gh][gw]cell

  val step [gh][gw]: i64 -> *[gh][gw]cell -> i64 -> rng -> (rng, bool, *[gh][gw]cell)

  val n_scenarios: i64
}

type helper_out [gh][gw] [n] = (*[gh][gw]cell,
                                 (rng, bool, *[gh][gw]cell),
                                 string [n])

module type scenario_helper = {
  val helper [gh][gw]: *[gh][gw]cell -> *[gh][gw]cell -> i64 -> rng ->
             helper_out [gh][gw] []
}

module mk_scenario_helper (scenario: scenario): scenario_helper = {
  let helper [gh][gw]
             (init_grid: *[gh][gw]cell)
             (step_grid: *[gh][gw]cell) (step_steps: i64) (step_rng: rng):
             helper_out [gh][gw] [] =
    (scenario.init init_grid,
     scenario.step step_grid step_steps step_rng,
     scenario.name ())
}

module type scenario_helper_helper = {
  val scenario_helper [gh][gw]: i64 -> *[gh][gw]cell -> *[gh][gw]cell -> i64 -> rng ->
                      helper_out [gh][gw] []
}

module mk_scenario_helper_exposed (scenario_helper_helper: scenario_helper_helper): scenario_helper_exposed = {
  let dummy_rng () = rnge.rng_from_seed [0]

  let dummy_grid gh gw = replicate gh (replicate gw (create_cell () (dummy_rng ())))

  let name (sid: i64): string [] =
    (scenario_helper_helper.scenario_helper sid (dummy_grid 1 1) (dummy_grid 1 1) 0 (dummy_rng ())).2

  let init [gh][gw] (sid: i64) (grid: *[gh][gw]cell): *[gh][gw]cell =
    (scenario_helper_helper.scenario_helper sid grid (dummy_grid gh gw) 0 (dummy_rng ())).0

  let step [gh][gw] (sid: i64) (grid: *[gh][gw]cell) (steps: i64) (rng: rng): (rng, bool, *[gh][gw]cell) =
    copy (scenario_helper_helper.scenario_helper sid (dummy_grid gh gw) grid steps rng).1

  let n_scenarios: i64 =
    loop i = 0
    while length (name i) != 0
    do i + 1
}
