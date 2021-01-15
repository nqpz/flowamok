import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/diku-dk/cpprandom/random"

module rnge = xorshift128plus
module dist_i8 = uniform_int_distribution i8 rnge
module dist_f32 = uniform_real_distribution f32 rnge
type rng = rnge.rng

-- FIXME: i64 is larger than it needs to for many of the cases below.
type flow = i64 -- -1 or +1

type direction =
  {y: flow, x: flow}

type underlying =
  {rng: rng,
   flow_y: flow,
   flow_x: flow}

type can_be_moved_to_from =
  {calculated: bool,
   flow_y: bool,
   flow_x: bool,
   next_preference_flow_x: bool}

type cell =
  {underlying: underlying,
   color: argb.colour,
   along_axis: direction,
   can_be_moved_to_from: can_be_moved_to_from}

let empty_direction: direction =
  {y=0, x=0}

let create_underlying (rng: rng): underlying =
  {rng=rng,
   flow_y=0,
   flow_x=0}

let empty_can_be_moved_to_from: can_be_moved_to_from =
  {calculated=false,
   flow_y=false,
   flow_x=false,
   next_preference_flow_x=false}

let create_cell (rng: rng): cell =
  {underlying=create_underlying rng,
   color=argb.white,
   along_axis=empty_direction,
   can_be_moved_to_from=empty_can_be_moved_to_from}

let is_occupied (cell: cell): bool =
  cell.along_axis.y != 0 || cell.along_axis.x != 0

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
  let values = map (\i -> cells'[i] with underlying.flow_y = flow) indexes
  in unflatten h w (scatter cells' indexes values)

let add_line_horizontal [h] [w] (y: i64) (x_start: i64) (x_end: i64) (flow: flow) (cells: *[h][w]cell):
                        *[h][w]cell =
  let n = x_end - x_start + 1
  let cells' = flatten cells
  let indexes = map (\t -> flatten_coordinate w y (x_start + t)) (0..<n)
  let values = map (\i -> cells'[i] with underlying.flow_x = flow) indexes
  in unflatten h w (scatter cells' indexes values)

let add_cell [h] [w] (y: i64) (x: i64) (color: argb.colour) (along_axis: direction) (cells: *[h][w]cell): *[h][w]cell =
  let cell = cells[y, x] with color = color
                         with along_axis = along_axis
  in cells with [y, x] = copy cell

let step (h: i64) (w: i64) (cells: *[h][w]cell): *[h][w]cell =
  let update_can_be_moved_to_from (cells: *[h][w]cell): *[h][w]cell =
    let update_can_be_moved_to_from_cell (y: i64) (x: i64): cell =
      let cell = cells[y, x]
      in if cell.can_be_moved_to_from.calculated
         then cell
         else let (can_be_moved_to_from_calculated, can_be_moved_to_from_base) =
                if is_occupied cell
                then let (y_next, x_next) = (y + cell.along_axis.y, x + cell.along_axis.x)
                     in if in_bounds h w y_next x_next
                        then let cell_next = cells[y_next, x_next]
                             in if cell_next.underlying.flow_y != 0 || cell_next.underlying.flow_x != 0
                                then (cell_next.can_be_moved_to_from.calculated,
                                      ((cell.along_axis.y != 0 && cell_next.can_be_moved_to_from.flow_y) ||
                                       (cell.along_axis.x != 0 && cell_next.can_be_moved_to_from.flow_x)))
                                else (true, true)
                        else (true, true)
                else (true, true)
              in if can_be_moved_to_from_calculated
                 then let cell = cell with can_be_moved_to_from.calculated = true
                      in if can_be_moved_to_from_base
                         then if cell.underlying.flow_y != 0 || cell.underlying.flow_x != 0
                              then let y_prev = y - cell.underlying.flow_y
                                   let x_prev = x - cell.underlying.flow_x
                                   let cell_prev_y_occupied = cell.underlying.flow_y != 0 &&
                                                              in_bounds h w y_prev x &&
                                                              cells[y_prev, x].along_axis.y != 0
                                   let cell_prev_x_occupied = cell.underlying.flow_x != 0 &&
                                                              in_bounds h w y x_prev &&
                                                              cells[y, x_prev].along_axis.x != 0
                                   let (flow_y, flow_x, next_preference_flow_x) =
                                     match (cell_prev_y_occupied, cell_prev_x_occupied, cell.can_be_moved_to_from.next_preference_flow_x)
                                     case (_, true, true) -> (false, true, false)
                                     case (true, _, false) -> (true, false, true)
                                     case (true, false, true) -> (true, false, true)
                                     case (false, true, false) -> (false, true, false)
                                     case (false, false, b) -> (false, false, b)
                                   in cell with can_be_moved_to_from.flow_y = flow_y
                                           with can_be_moved_to_from.flow_x = flow_x
                                           with can_be_moved_to_from.next_preference_flow_x = next_preference_flow_x
                              else -- Accept "leaks" of cells into nothingness.
                                cell with can_be_moved_to_from.flow_y = true
                                     with can_be_moved_to_from.flow_x = true
                         else cell
                 else cell

    -- FIXME: This is better done sequentially backwards to reduce the times we
    -- need to iterate in case of close queues.
    in tabulate_2d h w update_can_be_moved_to_from_cell

  let (cells, _, _) =
    -- FIXME: Handle deadlocks somehow.
    loop (cells, n_steps, running) = (cells, 0, true) while running
    do let cells' = update_can_be_moved_to_from cells
       let n_calculated = i64.sum (map (\cell -> i64.bool (cell.can_be_moved_to_from.calculated)) (flatten cells'))
       in if n_calculated == h * w || n_steps > h * w -- FIXME (temporary security measure)
          then (cells', n_steps, false)
          else (cells', n_steps + 1, true)

  let move_cell (y: i64) (x: i64): cell =
    let cell = cells[y, x]
    in if cell.can_be_moved_to_from.flow_y || cell.can_be_moved_to_from.flow_x
       then let (y_prev, x_prev) = if cell.can_be_moved_to_from.flow_y
                                   then (y - cell.underlying.flow_y, x)
                                   else (y, x - cell.underlying.flow_x)
            in if in_bounds h w y_prev x_prev
               then let cell_prev = cells[y_prev, x_prev]
                    let (flow_y_ok, flow_y_ok_isolated) =
                      if cell.underlying.flow_y != 0 &&
                         in_bounds h w (y + cell.underlying.flow_y) x
                      then let cell_next = cells[y + cell.underlying.flow_y, x]
                           let ok_base = cell_next.underlying.flow_y == cell.underlying.flow_y
                           in (ok_base, ok_base ||
                                        (cell_next.underlying.flow_y == 0 && cell_next.underlying.flow_x == 0))
                      else (false, false)
                    let (flow_x_ok, flow_x_ok_isolated) =
                      if cell.underlying.flow_x != 0 &&
                         in_bounds h w y (x + cell.underlying.flow_x)
                      then let cell_next = cells[y, x + cell.underlying.flow_x]
                           let ok_base = cell_next.underlying.flow_x == cell.underlying.flow_x
                           in (ok_base, ok_base ||
                                        (cell_next.underlying.flow_y == 0 && cell_next.underlying.flow_x == 0))
                      else (false, false)
                    let (rng, along_axis) =
                      if flow_y_ok && flow_x_ok
                      then let (rng, choice) = dist_i8.rand (0, 1) cell.underlying.rng
                           -- FIXME: Better pathfinder than randomness
                           in (rng, if choice == 0
                                    then {y=cell.underlying.flow_y, x=0}
                                    else {y=0, x=cell.underlying.flow_x})
                      else if !flow_x_ok && flow_y_ok_isolated
                      then (cell.underlying.rng, {y=cell.underlying.flow_y, x=0})
                      else if !flow_y_ok && flow_x_ok_isolated
                      then (cell.underlying.rng, {y=0, x=cell.underlying.flow_x})
                      else (cell.underlying.rng, empty_direction) -- is this possible?
                    in cell with color = cell_prev.color
                            with along_axis = along_axis
                            with underlying.rng = rng
               else cell
       else let (y_next, x_next) = (y + cell.along_axis.y, x + cell.along_axis.x)
            in if in_bounds h w y_next x_next
               then let cell_next = cells[y_next, x_next]
                    in if (cell_next.can_be_moved_to_from.flow_y && cell.along_axis.y != 0) ||
                          (cell_next.can_be_moved_to_from.flow_x && cell.along_axis.x != 0)
                       then cell with along_axis = empty_direction
                       else cell
               else cell with along_axis = empty_direction

  -- Actually move the movable cells.
  let cells = tabulate_2d h w move_cell

  -- Reset can_be_moved_to_from fields.
  let cells = map (map (\(cell: cell) -> cell with can_be_moved_to_from.calculated = false
                                              with can_be_moved_to_from.flow_x = false
                                              with can_be_moved_to_from.flow_y = false)) cells
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
     grid: [][]cell}

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
    in {h, w, rng, steps=0, grid}

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
           let grid = step h w grid
           let (rng, grid) = scenario.step grid s.steps s.rng
           in s with rng = rng
                with steps = s.steps + 1
                with grid = grid
      else s
    case _ ->
      s

  let render (s: state): [][]argb.colour =
    let render_cell (cell: cell): (argb.colour, bool, i64, i64) =
      let (color, occupied) =
        if is_occupied cell
        then (cell.color, true)
        else let (has_flow_x, has_flow_y) = (cell.underlying.flow_x != 0,
                                             cell.underlying.flow_y != 0)
             in if has_flow_x && has_flow_y
                then (argb.white, false)
                else if has_flow_x
                then (argb.white, false)
                else if has_flow_y
                then (argb.white, false)
                else (argb.black, false)
      in (color, occupied, cell.underlying.flow_x, cell.underlying.flow_y)

    let rendered_cells = map (map render_cell) s.grid

    let flow_draw_at (flow_x: i64) (flow_y: i64) (y: i64) (x: i64): bool =
      let (d2, m2) = (scale / 2, scale % 2)
      let (y, x) = (y - d2,
                    x - d2)
      let (y, x) = if m2 == 0
                   then (y + i64.bool (y >= 0),
                         x + i64.bool (x >= 0))
                   else (y, x)
      let d = d2 - m2
      let d' = d - 1 - m2
      let (y', x') = (flow_y * y, flow_x * x)
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

-- For now, uncomment what scenario you want to see play out:

-- FIXME: Make this more user friendly.

-- module lys = mk_lys scenario_one_choice
-- module lys = mk_lys scenario_close_queues
-- module lys = mk_lys scenario_cycle
-- module lys = mk_lys scenario_crossing
-- module lys = mk_lys scenario_crossing_close
module lys = mk_lys scenario_crossroads
