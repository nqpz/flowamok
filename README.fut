-- # FLOWAMOK
--
-- A traffic simulation for the ages.  Maybe a game at some point.
--
-- It's a stencil and then some.  Work in progress!
--
-- Requires [Futhark](http://futhark-lang.org) and SDL2 and SDL2-ttf
-- libraries with associated header files.
--
--
-- ## Building and running
--
-- First run `futhark pkg sync` once.
--
-- Then run `make run` to build and run in a window.
--
--
-- ## Controls
--
-- - Space: Step through the simulation manually.
-- - Left arrow key: Move left in the list of scenarios.
-- - Right arrow key: Move left in the list of scenarios.
-- - `a`: Toggle auto-stepping.
-- - Up arrow key: Increment the steps per second in the auto-stepping mode.
-- - Down arrow key: Decrement the steps per second in the auto-stepping mode.
-- - `r`: Reset the current grid and stop auto-stepping.
-- - F1: Toggle text in upper-left corner.

-- ## What it looks like

import "model"
module scenarios = import "scenarios"

type state [n_cycles] [gh] [gw] =
  {grid: [gh][gw]cell,
   cycles: [n_cycles][gh][gw](direction flow),
   rng: rng,
   scale: i64,
   steps: i64}

module mk_anim (scenario: scenarios.scenario) = {
  let init (gh: i64) (gw: i64) (scale: i64) (seed: i32): state [] [gh] [gw] =
    let (rng, grid) = create_grid gh gw (rnge.rng_from_seed [seed])
    let grid = scenario.init grid
    let cycles = find_cycles grid
    in {grid, cycles, rng, scale, steps=0}

  let step [gh] [gw] (s: state [] [gh] [gw]): ([][]u32, state [] [gh] [gw]) =
    let (h, w) = (gh * s.scale, gw * s.scale)
    let pixels = render h w gh gw s.scale s.grid
    let grid = copy s.grid
    let grid = step gh gw s.cycles grid
    let (rng, recompute_cycles, grid) = scenario.step grid s.steps s.rng
    let cycles = if recompute_cycles
                 then find_cycles grid
                 else s.cycles
    let s = s with grid = grid
              with cycles = cycles
              with rng = rng
    in (pixels, s)
}

module anim = mk_anim scenarios.overlapping_tight_cycles
entry init: i64 -> i64 -> i64 -> i32 -> state [] [] [] = anim.init
entry step: state [] [] [] -> ([][]u32, state [] [] []) = anim.step

-- > :brief :anim (step, init 30i64 30i64 10i64 123i32, 400i64);
-- format: gif
