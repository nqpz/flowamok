import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/athas/matte/colour"

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
