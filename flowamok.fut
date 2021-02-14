import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/lys/lys"

module rnge = pcg32
module dist_i8 = uniform_int_distribution i8 rnge
module dist_f32 = uniform_real_distribution f32 rnge
type rng = rnge.rng

type option 'a = #some a | #none

let all_somes 'a (xs: [](option a)) (ne: a): []a =
  let is_some (x: option a): bool =
    match x
    case #some _ -> true
    case #none -> false
  let remove_some (x: option a): a =
    match x
    case #some a -> a
    case #none -> assert false ne
  in map remove_some (filter is_some xs)

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

type cell 'aux =
  {underlying: underlying,
   direction: direction flow,
   color: argb.colour,
   can_be_moved_to_from: can_be_moved_to_from,
   aux: aux}

let empty_direction 'base (empty: base): direction base =
  {y=empty, x=empty}

let create_underlying (rng: rng): underlying =
  {rng=rng,
   direction=empty_direction 0}

let empty_can_be_moved_to_from: can_be_moved_to_from =
  {calculated=false,
   direction=empty_direction false,
   next_preference_flow_x=false}

let create_cell 'aux (aux: aux) (rng: rng): cell aux =
  {underlying=create_underlying rng,
   direction=empty_direction 0,
   color=argb.white,
   can_be_moved_to_from=empty_can_be_moved_to_from,
   aux=aux}

type cell_leak 'aux = (i64, i64, cell aux)
let cell_leak_dummy_element 'aux (aux: aux): cell_leak aux =
  (-1, -1, create_cell aux (rnge.rng_from_seed [0]))

let has_underlying 'aux (cell: cell aux): bool =
  cell.underlying.direction.y != 0 || cell.underlying.direction.x != 0

let is_occupied 'aux (cell: cell aux): bool =
  cell.direction.y != 0 || cell.direction.x != 0

let flatten_coordinate (gw: i64) (y: i64) (x: i64): i64 =
  y * gw + x

let in_bounds (h: i64) (w: i64) (y: i64) (x: i64): bool =
  y >= 0 && y < h && x >= 0 && x < w

let create_grid 'aux (gh: i64) (gw: i64) (aux: aux) (rng: rng): (rng, *[gh][gw](cell aux)) =
  let n = gh * gw
  let rngs = rnge.split_rng n rng
  let cells = map (create_cell aux) rngs
  in (rnge.join_rng rngs, unflatten gh gw cells)

let add_line_vertical 'aux [gh] [gw] (y_start: i64) (y_end: i64) (x: i64) (flow: flow) (cells: *[gh][gw](cell aux)):
                      *[gh][gw](cell aux) =
  let n = y_end - y_start + 1
  let cells' = flatten cells
  let indexes = map (\t -> flatten_coordinate gw (y_start + t) x) (0..<n)
  let values = map (\i -> cells'[i] with underlying.direction.y = flow) indexes
  in unflatten gh gw (scatter cells' indexes values)

let add_line_horizontal 'aux [gh] [gw] (y: i64) (x_start: i64) (x_end: i64) (flow: flow) (cells: *[gh][gw](cell aux)):
                        *[gh][gw](cell aux) =
  let n = x_end - x_start + 1
  let cells' = flatten cells
  let indexes = map (\t -> flatten_coordinate gw y (x_start + t)) (0..<n)
  let values = map (\i -> cells'[i] with underlying.direction.x = flow) indexes
  in unflatten gh gw (scatter cells' indexes values)

let add_cell 'aux [gh] [gw] (y: i64) (x: i64) (color: argb.colour) (direction: direction flow) (cells: *[gh][gw](cell aux)): *[gh][gw](cell aux) =
  let cell = cells[y, x] with color = color
                         with direction = direction
  in cells with [y, x] = copy cell

let nub_2d [l] [m] [n] (srcs: [l][m][n](direction flow)): [][m][n](direction flow) =
  let srcs' = zip srcs (0..<l)
  let (srcs'', _) = unzip (filter (\(src0, i0) -> all (\(src, i) -> i >= i0 || src0 != src) srcs') srcs')
  in srcs''

-- Taken from https://futhark-lang.org/examples/no-neutral-element.html
type with_neutral 't = #neutral | #val t

let f_with_neutral 't (f: t -> t -> t)
                      (x: with_neutral t)
                      (y: with_neutral t)
                      : with_neutral t =
  match (x, y)
  case (#val x, #val y) -> #val (f x y)
  case (#neutral, _) -> y
  case (_, #neutral) -> x

let reduce1 't (f: t -> t -> t) (ts: []t) : with_neutral t =
  reduce (f_with_neutral f) #neutral (map (\t -> #val t) ts)

-- FIXME: It would be nice with an incremental version of this and not have to
-- re-run the full detection on the entire grid on every underlying change.
--
-- FIXME: Consider if we maybe want to store the resulting mask in a sparse way
-- instead.  Mostly relevant if we want to save space.
--
-- FIXME: This is really slow.
let find_cycles 'aux [gh] [gw] (cells: [gh][gw](cell aux)): [][gh][gw](direction flow) =
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

let step 'aux [n_cycle_checks] (gh: i64) (gw: i64)
                          (cycle_checks: [n_cycle_checks][gh][gw](direction flow))
                          (cells: *[gh][gw](cell aux))
                          (choose_direction: i64 -> i64 -> cell aux -> (rng, direction flow))
                          (aux_dummy_element: aux):
                          (*[gh][gw](cell aux), [](cell_leak aux)) =
  let in_bounds = in_bounds gh gw
  let update_can_be_moved_to_from (cells: *[gh][gw](cell aux)): *[gh][gw](cell aux) =
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

  -- Cycle detection and resolution.
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
  let cells = match reduce1 merge_checked grids_checked
              case #val cells -> cells
              case #neutral -> cells

  let move_cell (y: i64) (x: i64): (cell aux, option (cell_leak aux)) =
    let cell = cells[y, x]
    let (y_next, x_next) = (y + i64.i8 cell.direction.y, x + i64.i8 cell.direction.x)
    let cell' =
      if cell.can_be_moved_to_from.direction.y || cell.can_be_moved_to_from.direction.x
      then let (y_prev, x_prev) = if cell.can_be_moved_to_from.direction.y
                                  then (y - i64.i8 cell.underlying.direction.y, x)
                                  else (y, x - i64.i8 cell.underlying.direction.x)
           in if in_bounds y_prev x_prev
              then let cell_prev = cells[y_prev, x_prev]
                   let (flow_y_ok, flow_y_ok_isolated) =
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

  -- Actually move the movable cells.
  let (cells, cell_leaks) = unzip (flatten (tabulate_2d gh gw move_cell))
  let cell_leaks = all_somes cell_leaks (cell_leak_dummy_element aux_dummy_element)

  let cells = unflatten gh gw cells

  -- Reset can_be_moved_to_from fields.
  let cells = map (map (\(cell: cell aux) ->
                          cell with can_be_moved_to_from.calculated = false
                               with can_be_moved_to_from.direction.x = false
                               with can_be_moved_to_from.direction.y = false)) cells
  in (cells, cell_leaks)

-- Useful for testing unrelated parts of the algorithm.
let choose_direction_random 'aux (_y: i64) (_x: i64) (cell: cell aux): (rng, direction flow) =
  let (rng, choice) = dist_i8.rand (0, 1) cell.underlying.rng
  in (rng, if choice == 0
           then {y=cell.underlying.direction.y, x=0}
           else {y=0, x=cell.underlying.direction.x})

let render 'aux (h: i64) (w: i64) (gh: i64) (gw: i64) (scale: i64) (grid: [gh][gw](cell aux)): [h][w]argb.colour =
  let render_cell (cell: cell aux): (argb.colour, bool, i8, i8) =
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

  let rendered_cells = map (map render_cell) grid

  let flow_draw_at (flow_x: i8) (flow_y: i8) (y: i64) (x: i64): bool =
    let (d2, m2) = (scale / 2, scale % 2)
    let (y, x) = (y - d2,
                  x - d2)
    let (y, x) = if m2 == 0
                 then (y + i64.bool (y >= 0),
                       x + i64.bool (x >= 0))
                 else (y, x)
    let d = d2 - m2
    let (y', x') = (i64.i8 flow_y * y, i64.i8 flow_x * x)
    let (y_abs, x_abs) = (i64.abs y, i64.abs x)
    let (y_draw, x_draw) = (y' == d - x_abs && x_abs <= d,
                            x' == d - y_abs && y_abs <= d)
    in (flow_x == 0 && y_draw) || (flow_y == 0 && x_draw) || (flow_y != 0 && flow_x != 0 && (y_draw || x_draw))

  let render_pixel (y: i64) (x: i64): argb.colour =
    let (y_grid, x_grid) = (y / scale, x / scale)
    in if in_bounds gh gw y_grid x_grid
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

  in tabulate_2d h w render_pixel

module type scenario = {
  val name: () -> string []

  val init [gh] [gw]: *[gh][gw](cell ()) -> *[gh][gw](cell ())

  val step [gh] [gw]: *[gh][gw](cell ()) -> i64 -> rng -> (rng, bool, *[gh][gw](cell ()))
}
