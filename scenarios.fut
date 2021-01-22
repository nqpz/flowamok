import "model"

module type scenario = {
  val init [h] [w]: *[h][w]cell -> *[h][w]cell

  val step [h] [w]: *[h][w]cell -> i64 -> rng -> (rng, *[h][w]cell)
}

let random_color (rng: rng): (rng, argb.colour) =
  let (rng, color_r) = dist_f32.rand (0.2, 0.8) rng
  let (rng, color_g) = dist_f32.rand (0.2, 0.8) rng
  let (rng, color_b) = dist_f32.rand (0.2, 0.8) rng
  in (rng, argb.from_rgba color_r color_g color_b 1.0)

module single_fork: scenario = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 10 10 20 1 grid
    let grid = add_line_vertical 10 40 20 1 grid
    let grid = add_line_horizontal 30 5 25 (-1) grid
    in grid

  let step [h] [w] (grid: *[h][w]cell) (steps: i64) (rng: rng): (rng, *[h][w]cell) =
    if steps % 5 == 0
    then let (rng, color) = random_color rng
         in (rng, add_cell 10 10 color {y=0, x=1} grid)
    else (rng, grid)
}

module tight_queues: scenario = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 40 10 20 1 grid
    let grid = add_line_vertical 10 40 20 (-1) grid
    in grid

  let step [h] [w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, *[h][w]cell) =
    let (rng, color) = random_color rng
    in (rng, add_cell 40 10 color {y=0, x=1} grid)
}

module basic_cycle: scenario = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 10 10 30 1 grid
    let grid = add_line_vertical 10 30 30 1 grid
    let grid = add_line_horizontal 30 10 30 (-1) grid
    let grid = add_line_vertical 10 30 10 (-1) grid
    let grid = add_cell 10 20 argb.red {y=0, x=1} grid
    let grid = add_cell 20 30 argb.blue {y=1, x=0} grid
    let grid = add_cell 30 20 argb.brown {y=0, x= -1} grid
    let grid = add_cell 20 10 argb.magenta {y= -1, x=0} grid
    in grid

  let step [h] [w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, *[h][w]cell) =
    (rng, grid)
}

module crossing: scenario = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 20 10 30 1 grid
    let grid = add_line_vertical 10 30 20 1 grid
    in grid

  let step [h] [w] (grid: *[h][w]cell) (steps: i64) (rng: rng): (rng, *[h][w]cell) =
    if steps % 5 == 0
    then let grid = add_cell 20 10 argb.red {y=0, x=1} grid
         let grid = add_cell 10 20 argb.blue {y=1, x=0} grid
         in (rng, grid)
    else (rng, grid)
}

module close_crossing: scenario = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 20 10 30 1 grid
    let grid = add_line_vertical 10 30 20 1 grid
    in grid

  let step [h] [w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, *[h][w]cell) =
    let (rng, color) = random_color rng
    let grid = add_cell 20 10 color {y=0, x=1} grid
    let (rng, color) = random_color rng
    let grid = add_cell 10 20 color {y=1, x=0} grid
    in (rng, grid)
}

module crossroads: scenario = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 29 1 58 (-1) grid
    let grid = add_line_horizontal 30 1 58 1 grid
    let grid = add_line_vertical 1 58 29 1 grid
    let grid = add_line_vertical 1 58 30 (-1) grid
    in grid

  let step [h] [w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, *[h][w]cell) =
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
    in (rng, grid)
}

module small_tight_cycle: scenario = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 11 11 13 1 grid
    let grid = add_line_vertical 11 13 13 1 grid
    let grid = add_line_horizontal 13 11 13 (-1) grid
    let grid = add_line_vertical 11 13 11 (-1) grid
    let grid = loop grid for i < 2 do add_cell 11 (11 + i) argb.red {y=0, x=1} grid
    let grid = loop grid for i < 2 do add_cell (11 + i) 13 argb.blue {y=1, x=0} grid
    let grid = loop grid for i < 2 do add_cell 13 (12 + i) argb.orange {y=0, x= -1} grid
    let grid = loop grid for i < 2 do add_cell (12 + i) 11 argb.magenta {y= -1, x=0} grid
    in grid

  let step [h] [w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, *[h][w]cell) =
    (rng, grid)
}

module independent_tight_cycles: scenario = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
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

  let step [h] [w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, *[h][w]cell) =
    (rng, grid)
}

module overlapping_tight_cycles: scenario = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
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

  let step [h] [w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, *[h][w]cell) =
    (rng, grid)
}

module hits_an_edge: scenario = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 10 5 15 1 grid
    let grid = add_line_vertical 5 15 16 (-1) grid
    let grid = add_cell 10 10 argb.brown {y=0, x=1} grid
    in grid

  let step [h] [w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, *[h][w]cell) =
    (rng, grid)
}

let n_scenarios = 10i64
