import "../lib/github.com/athas/matte/colour"
import "../flowamok"
import "explorer_scenario_helper"

module single_fork: scenario = {
  def name () = "single fork"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 10 10 20 1 grid
    let grid = add_line_vertical 10 40 20 1 grid
    let grid = add_line_horizontal 30 5 25 (-1) grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    if steps % 5 == 0
    then let (rng, color) = random_color rng
         in (rng, false, add_cell 10 10 color {y=0, x=1} grid)
    else (rng, false, grid)
}

module tight_queues: scenario = {
  def name () = "tight queues"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 40 10 20 1 grid
    let grid = add_line_vertical 10 40 20 (-1) grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    let (rng, color) = random_color rng
    in (rng, false, add_cell 40 10 color {y=0, x=1} grid)
}

module basic_cycle: scenario = {
  def name () = "basic cycle"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 10 10 30 1 grid
    let grid = add_line_vertical 10 30 30 1 grid
    let grid = add_line_horizontal 30 10 30 (-1) grid
    let grid = add_line_vertical 10 30 10 (-1) grid
    let grid = add_cell 10 20 argb.red {y=0, x=1} grid
    let grid = add_cell 20 30 argb.blue {y=1, x=0} grid
    let grid = add_cell 30 20 argb.brown {y=0, x= -1} grid
    let grid = add_cell 20 10 argb.magenta {y= -1, x=0} grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    (rng, false, grid)
}

module crossing: scenario = {
  def name () = "crossing"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 20 10 30 1 grid
    let grid = add_line_vertical 10 30 20 1 grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    if steps % 5 == 0
    then let grid = add_cell 20 10 argb.red {y=0, x=1} grid
         let grid = add_cell 10 20 argb.blue {y=1, x=0} grid
         in (rng, false, grid)
    else (rng, false, grid)
}

module close_crossing: scenario = {
  def name () = "close crossing"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 20 10 30 1 grid
    let grid = add_line_vertical 10 30 20 1 grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    let (rng, color) = random_color rng
    let grid = add_cell 20 10 color {y=0, x=1} grid
    let (rng, color) = random_color rng
    let grid = add_cell 10 20 color {y=1, x=0} grid
    in (rng, false, grid)
}

module crossroads: scenario = {
  def name () = "crossroads"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 29 1 58 (-1) grid
    let grid = add_line_horizontal 30 1 58 1 grid
    let grid = add_line_vertical 1 58 29 1 grid
    let grid = add_line_vertical 1 58 30 (-1) grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    let (rng, choice_east) = dist_i8.rand (0, 1) rng -- most busy
    let (rng, choice_south) = dist_i8.rand (0, 4) rng
    let (rng, choice_north) = dist_i8.rand (0, 9) rng
    let (rng, choice_west) = dist_i8.rand (0, 19) rng -- least busy
    let grid = if choice_north == 0
               then add_cell 1 29 argb.brown {y=1, x=0} grid
               else grid
    let grid = if choice_east == 0
               then add_cell 29 58 argb.green {y=0, x= -1} grid
               else grid
    let grid = if choice_south == 0
               then add_cell 58 30 argb.violet {y= -1, x=0} grid
               else grid
    let grid = if choice_west == 0
               then add_cell 30 1 argb.yellow {y=0, x=1} grid
               else grid
    in (rng, false, grid)
}

module small_tight_cycle: scenario = {
  def name () = "small tight cycle"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 11 11 13 1 grid
    let grid = add_line_vertical 11 13 13 1 grid
    let grid = add_line_horizontal 13 11 13 (-1) grid
    let grid = add_line_vertical 11 13 11 (-1) grid
    let grid = loop grid for i < 2 do add_cell 11 (11 + i) argb.red {y=0, x=1} grid
    let grid = loop grid for i < 2 do add_cell (11 + i) 13 argb.blue {y=1, x=0} grid
    let grid = loop grid for i < 2 do add_cell 13 (12 + i) argb.orange {y=0, x= -1} grid
    let grid = loop grid for i < 2 do add_cell (12 + i) 11 argb.magenta {y= -1, x=0} grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    (rng, false, grid)
}

module independent_tight_cycles: scenario = {
  def name () = "independent tight cycles"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = small_tight_cycle.init grid
    let grid = add_line_horizontal 10 10 14 (-1) grid
    let grid = add_line_vertical 10 14 14 (-1) grid
    let grid = add_line_horizontal 14 10 14 1 grid
    let grid = add_line_vertical 10 14 10 1 grid
    let grid = loop grid for i < 4 do add_cell 10 (11 + i) argb.red {y=0, x= -1} grid
    let grid = loop grid for i < 4 do add_cell (11 + i) 14 argb.blue {y= -1, x=0} grid
    let grid = loop grid for i < 4 do add_cell 14 (10 + i) argb.orange {y=0, x=1} grid
    let grid = loop grid for i < 4 do add_cell (10 + i) 10 argb.magenta {y=1, x=0} grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    (rng, false, grid)
}

module overlapping_tight_cycles: scenario = {
  def name () = "overlapping tight cycles"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 10 10 14 (-1) grid
    let grid = add_line_vertical 10 24 14 (-1) grid
    let grid = add_line_horizontal 24 10 14 1 grid
    let grid = add_line_vertical 10 24 10 1 grid

    let grid = add_line_horizontal 14 14 24 1 grid
    let grid = add_line_vertical 14 20 24 1 grid
    let grid = add_line_horizontal 20 14 24 (-1) grid

    let grid = loop grid for i < 4 do add_cell 10 (11 + i) argb.red {y=0, x= -1} grid
    let grid = loop grid for i < 14 do add_cell (11 + i) 14 argb.blue {y= -1, x=0} grid
    let grid = loop grid for i < 4 do add_cell 24 (10 + i) argb.orange {y=0, x=1} grid
    let grid = loop grid for i < 14 do add_cell (10 + i) 10 argb.magenta {y=1, x=0} grid

    let grid = loop grid for i < 9 do add_cell 14 (15 + i) argb.brown {y=0, x=1} grid
    let grid = loop grid for i < 6 do add_cell (14 + i) 24 argb.violet {y=1, x=0} grid
    let grid = loop grid for i < 10 do add_cell 20 (15 + i) argb.brown {y=0, x= -1} grid

    in grid

  def step [h][w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    (rng, false, grid)
}

module hits_an_edge: scenario = {
  def name () = "hits an edge"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 10 5 15 1 grid
    let grid = add_line_vertical 5 15 16 (-1) grid
    let grid = add_cell 10 10 argb.brown {y=0, x=1} grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    (rng, false, grid)
}

module adding_lines: scenario = {
  def name () = "adding lines"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 1 1 6 1 grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    let steps = steps + 10 in
    let (grid, recompute_cycles) =
      if steps % 5 == 0 && steps / 5 < 20
      then let k = (steps / 10) * 5 + 1
           in if (steps / 5) % 2 == 1
              then (add_line_horizontal k k (k + 5) 1 grid, true)
              else (add_line_vertical (k - 5) k k 1 grid, true)
      else (grid, false)
    let (rng, new_cell) = dist_i8.rand (0, 3) rng
    in if new_cell == 0
       then let (rng, color) = random_color rng
            in (rng, recompute_cycles, add_cell 1 1 color {y=0, x=1} grid)
       else (rng, recompute_cycles, grid)
}

module multi_spill: scenario = {
  def name () = "multi spill"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_vertical 1 9 10 1 grid
    let grid = add_line_horizontal 10 1 9 1 grid
    let grid = add_line_vertical 11 19 10 (-1) grid
    let grid = add_line_horizontal 10 11 19 (-1) grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    if steps % 5 == 0
    then let grid = add_cell 1 10 argb.red {y=1, x=0} grid
         let grid = add_cell 10 1 argb.blue {y=0, x=1} grid
         let grid = add_cell 19 10 argb.green {y= -1, x=0} grid
         let grid = add_cell 10 19 argb.yellow {y=0, x= -1} grid
         in (rng, false, grid)
    else (rng, false, grid)
}

-- Examine how the cycle detection algorithm fares with a lot of cycles.
module mk_many_cycles (input: { val n: i64 }): scenario = {
  def n = input.n

  def name () = "many cycles (n = " ++ ['0' + u8.i64 n] ++ ")"

  def init [h][w] (grid: *[h][w]cell): *[h][w]cell =
    let (yo, xo) = (19, 10)
    let grid =
      loop grid for y < n
      do loop grid for x < n
         do let (y', x') = (y + 1, x + 1)
            let grid = add_line_horizontal (yo + 10 * y) (xo + 10 * x) (xo + 10 * x' - i64.bool (x == n - 1)) 1 grid
            let grid = add_line_vertical (yo + 10 * y) (yo + 10 * y' - i64.bool (y == n -1)) (xo + 10 * x' - 1) 1 grid
            let grid = add_line_horizontal (yo + 10 * y' - 1) (xo + 10 * x - 1 + i64.bool (x == 0)) (xo + 10 * x' - 1) (-1) grid
            let grid = add_line_vertical (yo + 10 * y - 1 + i64.bool (y == 0)) (yo + 10 * y' - 1) (xo + 10 * x) (-1) grid
            in grid
    let grid = add_cell yo xo argb.green {y=0, x=1} grid
    in grid

  def step [h][w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, bool, *[h][w]cell) =
    (rng, false, grid)
}
