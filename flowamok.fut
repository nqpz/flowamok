import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/diku-dk/cpprandom/random"

module rnge = xorshift128plus
module dist_i8 = uniform_int_distribution i8 rnge
module dist_f32 = uniform_real_distribution f32 rnge
type rng = rnge.rng

type flow = i8 -- -1 or +1

type direction 'base =
  {y: base, x: base}

type underlying =
  {rng: rng,
   direction: direction flow}

type can_be_moved_to_from =
  {calculated: bool,
   direction: direction bool,
   next_preference_flow_x: bool}

type cell =
  {underlying: underlying,
   direction: direction flow,
   color: argb.colour,
   can_be_moved_to_from: can_be_moved_to_from}

let empty_direction 'base (empty: base): direction base =
  {y=empty, x=empty}

let create_underlying (rng: rng): underlying =
  {rng=rng,
   direction=empty_direction 0}

let empty_can_be_moved_to_from: can_be_moved_to_from =
  {calculated=false,
   direction=empty_direction false,
   next_preference_flow_x=false}

let create_cell (rng: rng): cell =
  {underlying=create_underlying rng,
   direction=empty_direction 0,
   color=argb.white,
   can_be_moved_to_from=empty_can_be_moved_to_from}

let has_underlying (cell: cell): bool =
  cell.underlying.direction.y != 0 || cell.underlying.direction.x != 0

let is_occupied (cell: cell): bool =
  cell.direction.y != 0 || cell.direction.x != 0

let flatten_coordinate (w: i64) (y: i64) (x: i64): i64 =
  y * w + x

let in_bounds (h: i64) (w: i64) (y: i64) (x: i64): bool =
  y >= 0 && y < h && x >= 0 && x < w

let create_grid (h: i64) (w: i64) (rng: rng): (rng, *[h][w]cell) =
  let n = h * w
  let rngs = rnge.split_rng n rng
  let cells = map create_cell rngs
  in (rnge.join_rng rngs, unflatten h w cells)

-- FIXME: Also save coordinates separately in a sparse way backwards for later
-- quick iterating for close queues.
let add_line_vertical [h] [w] (y_start: i64) (y_end: i64) (x: i64) (flow: flow) (cells: *[h][w]cell):
                      *[h][w]cell =
  let n = y_end - y_start + 1
  let cells' = flatten cells
  let indexes = map (\t -> flatten_coordinate w (y_start + t) x) (0..<n)
  let values = map (\i -> cells'[i] with underlying.direction.y = flow) indexes
  in unflatten h w (scatter cells' indexes values)

let add_line_horizontal [h] [w] (y: i64) (x_start: i64) (x_end: i64) (flow: flow) (cells: *[h][w]cell):
                        *[h][w]cell =
  let n = x_end - x_start + 1
  let cells' = flatten cells
  let indexes = map (\t -> flatten_coordinate w y (x_start + t)) (0..<n)
  let values = map (\i -> cells'[i] with underlying.direction.x = flow) indexes
  in unflatten h w (scatter cells' indexes values)

let add_cell [h] [w] (y: i64) (x: i64) (color: argb.colour) (direction: direction flow) (cells: *[h][w]cell): *[h][w]cell =
  let cell = cells[y, x] with color = color
                         with direction = direction
  in cells with [y, x] = copy cell

let find_cycles [h] [w] (cells: [h][w]cell): [][h][w](direction flow) =
  let is_corner (cell: cell): bool =
    cell.underlying.direction.y != 0 && cell.underlying.direction.x != 0
  let corners = filter (\(y, x) -> is_corner cells[y, x]) (flatten (tabulate_2d h w (\y x -> (y, x))))
  let empties () = replicate h (replicate w (empty_direction 0))
  let starts = flatten (map (\(y, x) ->
                               let cell_dir = cells[y, x].underlying.direction
                               let start1 = (empties () with [y, x] = {y=cell_dir.y, x=0},
                                             (y + i64.i8 cell_dir.y, x))
                               let start2 = (empties () with [y, x] = {y=0, x=cell_dir.x},
                                             (y, x + i64.i8 cell_dir.x))
                               in [start1, start2]
                            ) corners)
  let n_starts = length starts
  let (_, n_grids, grids) =
    loop (cur_grid, n_grids, grids) = (0, n_starts, starts)
    while cur_grid < n_grids
    do let (grid, (y, x)) = grids[cur_grid]
       let grid_cell = grid[y, x]
       in if grid_cell.y != 0 || grid_cell.x != 0 -- We have a cycle
          then (cur_grid + 1, n_grids, grids)
          else let cell = cells[y, x]
               let cell_dirs = cell.underlying.direction
               in if cell_dirs.y != 0 && cell_dirs.x != 0
                  then let grid_a = copy grid -- FIXME: Unnecesary copy
                       let grid_b = copy grid
                       let grids[cur_grid] = (grid_a with [y, x] = {y=cell_dirs.y, x=0},
                                              (y + i64.i8 cell_dirs.y, x))
                       let grids = grids ++ [(grid_b with [y, x] = {y=0, x=cell_dirs.x},
                                              (y, x + i64.i8 cell_dirs.x))]
                       in (cur_grid, n_grids + 1, grids)
                  else if cell_dirs.y != 0 || cell_dirs.x != 0
                  then let grids[cur_grid] = (copy grid with [y, x] = cell_dirs,
                                              (y + i64.i8 cell_dirs.y, x + i64.i8 cell_dirs.x))
                       in (cur_grid, n_grids, grids)
                  else let grids[cur_grid] = (copy grid, (-1, -1))
                       in (cur_grid + 1, n_grids, grids)
  let grids = grids[0:n_grids]
  let grids = map (.0) (filter (\(_, (y, _)) -> y != -1) grids)
  -- FIXME: Remove duplicates.
  in grids

let step [n_cycle_checks] (h: i64) (w: i64)
                          (cycle_checks: [n_cycle_checks][h][w](direction flow))
                          (cells: *[h][w]cell): *[h][w]cell =
  let update_can_be_moved_to_from (cells: *[h][w]cell): *[h][w]cell =
    let update_can_be_moved_to_from_cell (y: i64) (x: i64): cell =
      let cell = cells[y, x]
      in if cell.can_be_moved_to_from.calculated
         then cell
         else let (can_be_moved_to_from_calculated, can_be_moved_to_from_base) =
                if is_occupied cell
                then let (y_next, x_next) = (y + i64.i8 cell.direction.y, x + i64.i8 cell.direction.x)
                     in if in_bounds h w y_next x_next
                        then let cell_next = cells[y_next, x_next]
                             in if has_underlying cell_next
                                then (cell_next.can_be_moved_to_from.calculated,
                                      ((cell.direction.y != 0 && cell_next.can_be_moved_to_from.direction.y) ||
                                       (cell.direction.x != 0 && cell_next.can_be_moved_to_from.direction.x)))
                                else (true, true)
                        else (true, true)
                else (true, true)
              in if can_be_moved_to_from_calculated
                 then let cell = cell with can_be_moved_to_from.calculated = true
                      in if can_be_moved_to_from_base
                         then if has_underlying cell
                              then let y_prev = y - i64.i8 cell.underlying.direction.y
                                   let x_prev = x - i64.i8 cell.underlying.direction.x
                                   let cell_prev_y_occupied = cell.underlying.direction.y != 0 &&
                                                              in_bounds h w y_prev x &&
                                                              cells[y_prev, x].direction.y != 0
                                   let cell_prev_x_occupied = cell.underlying.direction.x != 0 &&
                                                              in_bounds h w y x_prev &&
                                                              cells[y, x_prev].direction.x != 0
                                   let (flow_y, flow_x, next_preference_flow_x) =
                                     match (cell_prev_y_occupied, cell_prev_x_occupied, cell.can_be_moved_to_from.next_preference_flow_x)
                                     case (_, true, true) -> (false, true, false)
                                     case (true, _, false) -> (true, false, true)
                                     case (true, false, true) -> (true, false, true)
                                     case (false, true, false) -> (false, true, false)
                                     case (false, false, b) -> (false, false, b)
                                   in cell with can_be_moved_to_from.direction.y = flow_y
                                           with can_be_moved_to_from.direction.x = flow_x
                                           with can_be_moved_to_from.next_preference_flow_x = next_preference_flow_x
                              else -- Accept "leaks" of cells into nothingness.
                                cell with can_be_moved_to_from.direction.y = true
                                     with can_be_moved_to_from.direction.x = true
                         else cell
                 else cell

    -- FIXME: This is better done sequentially backwards to reduce the times we
    -- need to iterate in case of close queues.
    in tabulate_2d h w update_can_be_moved_to_from_cell

  let (cells, _n_calculated, _) =
    loop (cells, n_calculated_prev, running) = (cells, 0, true) while running
    do let cells' = update_can_be_moved_to_from cells
       let n_calculated = i64.sum (map (\cell -> i64.bool (cell.can_be_moved_to_from.calculated)) (flatten cells'))
       in (cells', n_calculated, n_calculated != n_calculated_prev)

  -- Cycle detection and resolution.
  -- FIXME: This can be made to run faster.
  let n_cells = h * w
  let cells =
    loop cells for i < n_cycle_checks
    do let cycle_check_grid = cycle_checks[i]
       let cells_flat = flatten_to n_cells cells
       let checks_flat = flatten_to n_cells cycle_check_grid
       in if all (\(cell: cell, check: direction flow) ->
                    (check.y == 0 && check.x == 0) || (!cell.can_be_moved_to_from.calculated &&
                                                       cell.direction == check))
                 (zip cells_flat checks_flat)
          then let cells_flat =
                 map3 (\(cell: cell) (check: direction flow) ((y, x): (i64, i64)) ->
                         if check.y != 0 || check.x != 0
                         then let u_dir = cell.underlying.direction
                              let cell = cell with can_be_moved_to_from.calculated = true
                              in if u_dir.y != 0 && u_dir.x != 0
                                 then if cycle_check_grid[y - i64.i8 u_dir.y, x] == {y=u_dir.y, x=0}
                                      then cell with can_be_moved_to_from.direction.y = true
                                      else cell with can_be_moved_to_from.direction.x = true
                                 else cell with can_be_moved_to_from.direction.y = cell.underlying.direction.y != 0
                                           with can_be_moved_to_from.direction.x = cell.underlying.direction.x != 0
                         else cell)
                      cells_flat checks_flat (flatten_to n_cells (tabulate_2d h w (\y x -> (y, x))))
               in unflatten h w cells_flat
          else cells

  let move_cell (y: i64) (x: i64): cell =
    let cell = cells[y, x]
    in if cell.can_be_moved_to_from.direction.y || cell.can_be_moved_to_from.direction.x
       then let (y_prev, x_prev) = if cell.can_be_moved_to_from.direction.y
                                   then (y - i64.i8 cell.underlying.direction.y, x)
                                   else (y, x - i64.i8 cell.underlying.direction.x)
            in if in_bounds h w y_prev x_prev
               then let cell_prev = cells[y_prev, x_prev]
                    let (flow_y_ok, flow_y_ok_isolated) =
                      if cell.underlying.direction.y != 0 &&
                         in_bounds h w (y + i64.i8 cell.underlying.direction.y) x
                      then let cell_next = cells[y + i64.i8 cell.underlying.direction.y, x]
                           let ok_base = cell_next.underlying.direction.y == cell.underlying.direction.y
                           in (ok_base, ok_base ||
                                        (cell_next.underlying.direction.y == 0 && cell_next.underlying.direction.x == 0))
                      else (false, false)
                    let (flow_x_ok, flow_x_ok_isolated) =
                      if cell.underlying.direction.x != 0 &&
                         in_bounds h w y (x + i64.i8 cell.underlying.direction.x)
                      then let cell_next = cells[y, x + i64.i8 cell.underlying.direction.x]
                           let ok_base = cell_next.underlying.direction.x == cell.underlying.direction.x
                           in (ok_base, ok_base ||
                                        (cell_next.underlying.direction.y == 0 && cell_next.underlying.direction.x == 0))
                      else (false, false)
                    let (rng, direction) =
                      if flow_y_ok && flow_x_ok
                      then let (rng, choice) = dist_i8.rand (0, 1) cell.underlying.rng
                           -- FIXME: Better pathfinder than randomness
                           in (rng, if choice == 0
                                    then {y=cell.underlying.direction.y, x=0}
                                    else {y=0, x=cell.underlying.direction.x})
                      else if !flow_x_ok && flow_y_ok_isolated
                      then (cell.underlying.rng, {y=cell.underlying.direction.y, x=0})
                      else if !flow_y_ok && flow_x_ok_isolated
                      then (cell.underlying.rng, {y=0, x=cell.underlying.direction.x})
                      else (cell.underlying.rng, empty_direction 0) -- is this possible?
                    in cell with color = cell_prev.color
                            with direction = direction
                            with underlying.rng = rng
               else cell
       else let (y_next, x_next) = (y + i64.i8 cell.direction.y, x + i64.i8 cell.direction.x)
            in if in_bounds h w y_next x_next
               then let cell_next = cells[y_next, x_next]
                    in if (cell_next.can_be_moved_to_from.direction.y && cell.direction.y != 0) ||
                          (cell_next.can_be_moved_to_from.direction.x && cell.direction.x != 0)
                       then cell with direction = empty_direction 0
                       else cell
               else cell with direction = empty_direction 0

  -- Actually move the movable cells.
  let cells = tabulate_2d h w move_cell

  -- Reset can_be_moved_to_from fields.
  let cells = map (map (\(cell: cell) -> cell with can_be_moved_to_from.calculated = false
                                              with can_be_moved_to_from.direction.x = false
                                              with can_be_moved_to_from.direction.y = false)) cells
  in cells

module type scenario = {
  val init [h] [w]: *[h][w]cell -> *[h][w]cell

  val step [h] [w]: *[h][w]cell -> i64 -> rng -> (rng, *[h][w]cell)
}

type text_content = i32
module mk_lys (scenario: scenario): lys with text_content = text_content = {
  type~ state =
    {h: i64, w: i64,
     rng: rng,
     steps: i64,
     grid: [][]cell,
     cycle_checks: [][][](direction flow)}

  type text_content = text_content

  let text_format () = "FPS: %d\n"

  let text_content (fps: f32) (_s: state): text_content =
    t32 fps

  let text_colour = const argb.white

  let scale = 10i64

  let init (seed: u32) (h: i64) (w: i64): state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let (rng, grid) = create_grid (h / scale) (w / scale) rng
    let grid = scenario.init grid
    let cycle_checks = find_cycles grid
    in {h, w, rng, steps=0, grid, cycle_checks}

  let grab_mouse = false

  -- FIXME
  let resize (_h: i64) (_w: i64) (s: state): state =
    s
    -- s with h = h
    --   with w = w
    --   with grid = resize_grid s.grid h w s.rng

  let event (e: event) (s: state): state =
    match e
    case #step _td ->
      s
    case #keydown {key} ->
      if key == SDLK_SPACE
      then let (h, w) = (s.h / scale, s.w / scale)
           let grid = copy (s.grid :> [h][w]cell) -- FIXME: ugly
           let cycle_checks = s.cycle_checks :> [][h][w](direction flow)
           let grid = step h w cycle_checks grid
           let (rng, grid) = scenario.step grid s.steps s.rng
           in s with rng = rng
                with steps = s.steps + 1
                with grid = grid
      else s
    case _ ->
      s

  let render (s: state): [][]argb.colour =
    let render_cell (cell: cell): (argb.colour, bool, i8, i8) =
      let (color, occupied) =
        if is_occupied cell
        then (cell.color, true)
        else let (has_flow_x, has_flow_y) = (cell.underlying.direction.x != 0,
                                             cell.underlying.direction.y != 0)
             in if has_flow_x && has_flow_y
                then (argb.white, false)
                else if has_flow_x
                then (argb.white, false)
                else if has_flow_y
                then (argb.white, false)
                else (argb.black, false)
      in (color, occupied, cell.underlying.direction.x, cell.underlying.direction.y)

    let rendered_cells = map (map render_cell) s.grid

    let flow_draw_at (flow_x: i8) (flow_y: i8) (y: i64) (x: i64): bool =
      let (d2, m2) = (scale / 2, scale % 2)
      let (y, x) = (y - d2,
                    x - d2)
      let (y, x) = if m2 == 0
                   then (y + i64.bool (y >= 0),
                         x + i64.bool (x >= 0))
                   else (y, x)
      let d = d2 - m2
      let d' = d - 1 - m2
      let (y', x') = (i64.i8 flow_y * y, i64.i8 flow_x * x)
      let (y_abs, x_abs) = (i64.abs y, i64.abs x)
      in if flow_y == 0
         then x' == d - y_abs && y_abs <= d
         else if flow_x == 0
         then y' == d - x_abs && x_abs <= d
         else (x' == d' && y_abs <= d') || (y' == d' && x_abs <= d')

    let render_pixel (y: i64) (x: i64): argb.colour =
      let (y_grid, x_grid) = (y / scale, x / scale)
      in if in_bounds (s.h / scale) (s.w / scale) y_grid x_grid
         then let (pixel_base, occupied, flow_x, flow_y) = rendered_cells[y_grid, x_grid]
              in if flow_x != 0 || flow_y != 0
                 then let (y_inside_block, x_inside_block) = (y % scale, x % scale)
                      in if flow_draw_at flow_x flow_y y_inside_block x_inside_block
                         then let grey = argb.gray 0.3
                              in if occupied
                                 then argb.mix 0.5 grey 0.5 pixel_base
                                 else grey
                         else pixel_base
                 else argb.black
         else argb.black

    in tabulate_2d s.h s.w render_pixel
}

let random_color (rng: rng): (rng, argb.colour) =
  let (rng, color_r) = dist_f32.rand (0.2, 0.8) rng
  let (rng, color_g) = dist_f32.rand (0.2, 0.8) rng
  let (rng, color_b) = dist_f32.rand (0.2, 0.8) rng
  in (rng, argb.from_rgba color_r color_g color_b 1.0)

module scenario_one_choice = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 10 10 20 1 grid
    let grid = add_line_vertical 10 40 20 1 grid
    let grid = add_line_horizontal 30 5 25 (-1) grid
    in grid

  let step [h] [w] (grid: *[h][w]cell) (steps: i64) (rng: rng): (rng, *[h][w]cell) =
    if steps % 5 == 0
    then let (rng, color_r) = dist_f32.rand (0.2, 0.8) rng
         let (rng, color_g) = dist_f32.rand (0.2, 0.8) rng
         let (rng, color_b) = dist_f32.rand (0.2, 0.8) rng
         let color = argb.from_rgba color_r color_g color_b 1.0
         in (rng, add_cell 10 10 color {y=0, x=1} grid)
    else (rng, grid)
}

module scenario_close_queues = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = add_line_horizontal 40 10 20 1 grid
    let grid = add_line_vertical 10 40 20 (-1) grid
    in grid

  let step [h] [w] (grid: *[h][w]cell) (_steps: i64) (rng: rng): (rng, *[h][w]cell) =
    let (rng, color) = random_color rng
    in (rng, add_cell 40 10 color {y=0, x=1} grid)
}

module scenario_cycle = {
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

module scenario_crossing = {
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

module scenario_crossing_close = {
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

module scenario_crossroads = {
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

module scenario_cycle_small = {
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

module scenario_cycles_independent = {
  let init [h] [w] (grid: *[h][w]cell): *[h][w]cell =
    let grid = scenario_cycle_small.init grid
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

module scenario_cycles_overlapping = {
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

-- For now, uncomment what scenario you want to see play out:

-- FIXME: Make this more user friendly.

-- module lys = mk_lys scenario_one_choice
-- module lys = mk_lys scenario_close_queues
-- module lys = mk_lys scenario_cycle
-- module lys = mk_lys scenario_crossing
-- module lys = mk_lys scenario_crossing_close
-- module lys = mk_lys scenario_crossroads
-- module lys = mk_lys scenario_cycle_small
-- module lys = mk_lys scenario_cycles_independent
module lys = mk_lys scenario_cycles_overlapping
