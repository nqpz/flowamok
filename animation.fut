-- | Basic animation support.  For a more full-featured interactive use, see
-- `explorer/explorer.fut`.

import "flowamok"

type state [n_cycles] [gh] [gw] =
  {grid: [gh][gw](cell ()),
   cycles: [n_cycles][gh][gw](direction flow),
   rng: rng,
   scale: i64,
   steps: i64}

module mk_anim (scenario: scenario) = {
  def init (gh: i64) (gw: i64) (scale: i64) (seed: i32): state [] [gh] [gw] =
    let (rng, grid) = create_grid gh gw () (rnge.rng_from_seed [seed])
    let grid = scenario.init grid
    let cycles = stepper_perfect.find_cycles grid
    in {grid, cycles, rng, scale, steps=0}

  def step [gh][gw] (s: state [] [gh] [gw]): ([][]u32, state [] [gh] [gw]) =
    let (h, w) = (gh * s.scale, gw * s.scale)
    let pixels = render h w gh gw s.scale s.grid
    let grid = copy s.grid
    let (grid, _) = stepper_perfect.step gh gw choose_direction_random s.cycles () grid
    let (rng, recompute_cycles, grid) = scenario.step grid s.steps s.rng
    let cycles = if recompute_cycles
                 then stepper_perfect.find_cycles grid
                 else s.cycles
    let s = s with grid = grid
              with cycles = cycles
              with rng = rng
    in (pixels, s)
}
