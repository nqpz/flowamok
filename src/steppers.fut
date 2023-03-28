import "option"
import "reduce1"
import "helpers"
import "utils"
import "random"
import "model"

local module stepper_general = {
  local def update_can_be_moved_to_from 'aux [gh][gw]
      (cells: *[gh][gw](cell aux))
      : *[gh][gw](cell aux) =
    let in_bounds = in_bounds gh gw
    let update_can_be_moved_to_from_cell (y: i64) (x: i64): cell aux =
      let cell = cells[y, x]
      in if cell.can_be_moved_to_from.calculated
         then cell
         else let (can_be_moved_to_from_calculated, can_be_moved_to_from_base) =
                if is_occupied cell
                then let (y_next, x_next) = (y + i64.i8 cell.direction.y, x + i64.i8 cell.direction.x)
                     in if in_bounds y_next x_next
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
                                                              in_bounds y_prev x &&
                                                              cells[y_prev, x].direction.y != 0
                                   let cell_prev_x_occupied = cell.underlying.direction.x != 0 &&
                                                              in_bounds y x_prev &&
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
    in tabulate_2d gh gw update_can_be_moved_to_from_cell

  local def move_cell 'aux [gh][gw]
      (cells: [gh][gw](cell aux))
      (choose_direction: i64 -> i64 -> cell aux -> (rng, direction flow))
      (y: i64)
      (x: i64)
      : (cell aux, option (cell_leak aux)) =
    let in_bounds = in_bounds gh gw
    let cell = cells[y, x]
    let (y_next, x_next) = (y + i64.i8 cell.direction.y, x + i64.i8 cell.direction.x)
    let cell' =
      if cell.can_be_moved_to_from.direction.y || cell.can_be_moved_to_from.direction.x
      then let (y_prev, x_prev) = if cell.can_be_moved_to_from.direction.y
                                  then (y - i64.i8 cell.underlying.direction.y, x)
                                  else (y, x - i64.i8 cell.underlying.direction.x)
           in if in_bounds y_prev x_prev
              then let cell_prev = cells[y_prev, x_prev]
                   in if (cell.can_be_moved_to_from.direction.y && cell_prev.underlying.direction.y == cell.underlying.direction.y)
                         || (cell.can_be_moved_to_from.direction.x && cell_prev.underlying.direction.x == cell.underlying.direction.x)
                      then let (flow_y_ok, flow_y_ok_isolated) =
                             if cell.underlying.direction.y != 0 &&
                                in_bounds (y + i64.i8 cell.underlying.direction.y) x
                             then let cell_next = cells[y + i64.i8 cell.underlying.direction.y, x]
                                  let ok_base = cell_next.underlying.direction.y == cell.underlying.direction.y
                                  in (ok_base, ok_base ||
                                               (cell_next.underlying.direction.y == 0 && cell_next.underlying.direction.x == 0))
                             else (false, false)
                           let (flow_x_ok, flow_x_ok_isolated) =
                             if cell.underlying.direction.x != 0 &&
                                in_bounds y (x + i64.i8 cell.underlying.direction.x)
                             then let cell_next = cells[y, x + i64.i8 cell.underlying.direction.x]
                                  let ok_base = cell_next.underlying.direction.x == cell.underlying.direction.x
                                  in (ok_base, ok_base ||
                                               (cell_next.underlying.direction.y == 0 && cell_next.underlying.direction.x == 0))
                             else (false, false)
                           let (rng, direction) =
                             if flow_y_ok && flow_x_ok
                             then choose_direction y x cell
                             else if !flow_x_ok && flow_y_ok_isolated
                             then (cell.underlying.rng, {y=cell.underlying.direction.y, x=0})
                             else if !flow_y_ok && flow_x_ok_isolated
                             then (cell.underlying.rng, {y=0, x=cell.underlying.direction.x})
                             else (cell.underlying.rng, cell_prev.direction)
                           in cell with color = cell_prev.color
                                   with aux = cell_prev.aux
                                   with direction = direction
                                   with underlying.rng = rng
                      else cell
              else cell
      else if in_bounds y_next x_next
      then let cell_next = cells[y_next, x_next]
           in if (cell_next.can_be_moved_to_from.direction.y && cell.direction.y != 0) ||
                 (cell_next.can_be_moved_to_from.direction.x && cell.direction.x != 0)
              then cell with direction = empty_direction 0
              else cell
      else cell with direction = empty_direction 0
    let cell_leak =
      if cell.direction != empty_direction 0 && in_bounds y_next x_next
      then let cell_next = cells[y_next, x_next]
           in if cell_next.underlying.direction == empty_direction 0 &&
                 ((cell_next.can_be_moved_to_from.direction.y && cell.direction.y != 0) ||
                  (cell_next.can_be_moved_to_from.direction.x && cell.direction.x != 0))
              then #some (y_next, x_next, cell)
              else #none
      else #none
    in (cell', cell_leak)

  local def cell_leak_dummy_element 'aux (aux: aux): cell_leak aux =
    (-1, -1, create_cell aux (rnge.rng_from_seed [0]))

  local def move_movable_cells 'aux [gh][gw]
      (choose_direction: i64 -> i64 -> cell aux -> (rng, direction flow))
      (aux_dummy_element: aux)
      (cells: [gh][gw](cell aux))
      : (*[gh][gw](cell aux), [](cell_leak aux)) =
    let (cells, cell_leaks) = unzip (flatten (tabulate_2d gh gw (move_cell cells choose_direction)))
    let cell_leaks = all_somes (copy cell_leaks) (cell_leak_dummy_element aux_dummy_element)
    let cells = unflatten gh gw cells
    in (cells, cell_leaks)

  local def reset_cells 'aux [gh][gw]
    : *[gh][gw](cell aux) -> *[gh][gw](cell aux) =
    map (map (\(cell: cell aux) ->
                cell with can_be_moved_to_from.calculated = false
                     with can_be_moved_to_from.direction.x = false
                     with can_be_moved_to_from.direction.y = false))

  def move_until_fixpoint 'aux [gh][gw]
      (cells: *[gh][gw](cell aux))
      : *[gh][gw](cell aux) =
    let (cells, _n_calculated, _) =
      -- FIXME: In the best case we only need to run a single iteration of this.
      -- In the worst case we need to run this as many times as there are cells,
      -- only updating a single cell on every iteration (inherently sequential
      -- workload).  This is potentially wasteful (at least on non-GPUs) as we
      -- could accomplish the same thing with a sequential loop with finely tuned
      -- checking order.  We would probably need a separate array of pre-computed
      -- sorted grid indexes to accomplish this.  However, this worst-case
      -- handling could negatively effect the potential best case performance.
      -- Maybe there's a balance somewhere.
      loop (cells, n_calculated_prev, running) = (cells, 0, true) while running
      do let cells' = update_can_be_moved_to_from cells
         let n_calculated = i64.sum (map (\cell -> i64.bool (cell.can_be_moved_to_from.calculated)) (flatten cells'))
         in (cells', n_calculated, n_calculated != n_calculated_prev)
    in cells

  def move_and_reset_cells 'aux [gh][gw]
      (choose_direction: i64 -> i64 -> cell aux -> (rng, direction flow))
      (aux_dummy_element: aux)
      (cells: [gh][gw](cell aux))
      : (*[gh][gw](cell aux), [](cell_leak aux)) =
    let (cells, cell_leaks) = move_movable_cells choose_direction aux_dummy_element cells
    let cells = reset_cells cells
    in (cells, cell_leaks)
}

module stepper_quick = {
  def step 'aux
      (gh: i64)
      (gw: i64)
      (choose_direction: i64 -> i64 -> cell aux -> (rng, direction flow))
      (aux_dummy_element: aux)
      (cells: *[gh][gw](cell aux))
      : (*[gh][gw](cell aux), [](cell_leak aux)) =
    let cells = stepper_general.move_until_fixpoint cells
    -- let cells = stepper_general.update_can_be_moved_to_from cells
    in stepper_general.move_and_reset_cells choose_direction aux_dummy_element cells
}

module stepper_perfect = {
  local def resolve_cycles 'aux [n_cycle_checks][gh][gw]
      (cycle_checks: [n_cycle_checks][gh][gw](direction flow))
      (cells: *[gh][gw](cell aux))
      : *[gh][gw](cell aux) =
    let n_cells = gh * gw
    let grids_checked =
      map (\cycle_check_grid ->
             let cells_flat = flatten_to n_cells cells
             let checks_flat = flatten_to n_cells cycle_check_grid
             in if all (\(cell: cell aux, check: direction flow) ->
                          (check.y == 0 && check.x == 0) || (!cell.can_be_moved_to_from.calculated &&
                                                             cell.direction == check))
                       (zip cells_flat checks_flat)
                then let cells_flat =
                       map3 (\(cell: cell aux) (check: direction flow) ((y, x): (i64, i64)) ->
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
                            cells_flat checks_flat (flatten_to n_cells (tabulate_2d gh gw (\y x -> (y, x))))
                     in unflatten gh gw cells_flat
                else cells) cycle_checks
    let merge_checked (grid1: [gh][gw](cell aux)) (grid2: [gh][gw](cell aux)): [gh][gw](cell aux) =
      let merge_cell (cell1: cell aux) (cell2: cell aux): cell aux =
        if cell1.can_be_moved_to_from.calculated
        then cell1
        else cell2
      in map2 (map2 merge_cell) grid1 grid2
    in match reduce1 merge_checked grids_checked
       case #val cells -> cells
       case #neutral -> cells

  -- FIXME: It would be nice with an incremental version of this and not have to
  -- re-run the full detection on the entire grid on every underlying change.
  --
  -- FIXME: Consider if we maybe want to store the resulting mask in a sparse way
  -- instead.  Mostly relevant if we want to save space.
  --
  -- FIXME: This is really slow.
  def find_cycles 'aux [gh][gw] (cells: [gh][gw](cell aux)): [][gh][gw](direction flow) =
    let in_bounds = in_bounds gh gw
    let is_corner (cell: cell aux): bool =
      cell.underlying.direction.y != 0 && cell.underlying.direction.x != 0
    let corners = filter (\(y, x) -> is_corner cells[y, x]) (flatten (tabulate_2d gh gw (\y x -> (y, x))))
    let empties () = replicate gh (replicate gw (empty_direction 0))
    let starts = flatten (map (\(y, x) ->
                                 let cell_dir = cells[y, x].underlying.direction
                                 let (y_next, x_next) = (y + i64.i8 cell_dir.y,
                                                         x + i64.i8 cell_dir.x)
                                 let (y_next_ok, x_next_ok) =
                                   (in_bounds y_next x && cells[y_next, x].underlying.direction.y == cell_dir.y,
                                    in_bounds y x_next && cells[y, x_next].underlying.direction.x == cell_dir.x)
                                 let y_start () = (empties () with [y, x] = {y=cell_dir.y, x=0},
                                                   (y + i64.i8 cell_dir.y, x), (y, x))
                                 let x_start () = (empties () with [y, x] = {y=0, x=cell_dir.x},
                                                   (y, x + i64.i8 cell_dir.x), (y, x))
                                 in if y_next_ok && x_next_ok
                                    then [y_start (), x_start ()]
                                    else if y_next_ok
                                    then [y_start (), (empties (), (-1, -1), (-1, -1))]
                                    else if x_next_ok
                                    then [x_start (), (empties (), (-1, -1), (-1, -1))]
                                    else [(empties (), (-1, -1), (-1, -1)), (empties (), (-1, -1), (-1, -1))]
                              ) corners)
    let starts = filter (\(_, (y, _), _) -> y != -1) starts
    let n_starts = length starts
    let (_, n_grids, grids) =
      loop (cur_grid, n_grids, grids) = (0, n_starts, starts)
      while cur_grid < n_grids
      do let (grid, (y, x), (y_start, x_start)) = grids[cur_grid]
         in if y == y_start && x == x_start -- This means we have a cycle.
            then (cur_grid + 1, n_grids, grids)
            else let grid_cell = grid[y, x]
                 in if grid_cell.y != 0 || grid_cell.x != 0
                    then let grids[cur_grid] = (copy grid, (-1, -1), (y_start, x_start))
                         in (cur_grid + 1, n_grids, grids)
                    else let cell = cells[y, x]
                         let cell_dirs = cell.underlying.direction
                         in if cell_dirs.y != 0 && cell_dirs.x != 0
                            then let (y_next, x_next) = (y + i64.i8 cell_dirs.y, x + i64.i8 cell_dirs.x)
                                 let (y_next_ok, x_next_ok) =
                                   (in_bounds y_next x && cells[y_next, x].underlying.direction.y == cell_dirs.y,
                                    in_bounds y x_next && cells[y, x_next].underlying.direction.x == cell_dirs.x)
                                 let y_grid g = (copy g with [y, x] = {y=cell_dirs.y, x=0},
                                                 (y_next, x), (y_start, x_start))
                                 let x_grid g = (copy g with [y, x] = {y=0, x=cell_dirs.x},
                                                 (y, x_next), (y_start, x_start))
                                 in if y_next_ok && x_next_ok
                                    then let grid_b = copy grid
                                         let grids[cur_grid] = y_grid grid
                                         let grids = grids ++ [x_grid grid_b]
                                         in (cur_grid, n_grids + 1, grids)
                                    else if y_next_ok
                                    then let grids[cur_grid] = y_grid grid
                                         in (cur_grid, n_grids, grids)
                                    else let grids[cur_grid] = x_grid grid
                                         in (cur_grid, n_grids, grids)
                            else if cell_dirs.y != 0 || cell_dirs.x != 0
                            then let grids[cur_grid] = (copy grid with [y, x] = cell_dirs,
                                                        (y + i64.i8 cell_dirs.y, x + i64.i8 cell_dirs.x), (y_start, x_start))
                                 in (cur_grid, n_grids, grids)
                            else let grids[cur_grid] = (copy grid, (-1, -1), (y_start, x_start))
                                 in (cur_grid + 1, n_grids, grids)
    let grids = grids[:n_grids]
    let grids = map (.0) (filter (\(_, (y, _), _) -> y != -1) grids)
    let grids = nub_2d grids
    in grids

  def step 'aux [n_cycle_checks]
      (gh: i64)
      (gw: i64)
      (choose_direction: i64 -> i64 -> cell aux -> (rng, direction flow))
      (cycle_checks: [n_cycle_checks][gh][gw](direction flow))
      (aux_dummy_element: aux)
      (cells: *[gh][gw](cell aux))
      : (*[gh][gw](cell aux), [](cell_leak aux)) =
    let cells = stepper_general.move_until_fixpoint cells
    let cells = resolve_cycles cycle_checks cells
    in stepper_general.move_and_reset_cells choose_direction aux_dummy_element cells
}
