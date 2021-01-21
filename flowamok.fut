import "lib/github.com/diku-dk/lys/lys"
import "model"
import "scenarios"

type text_content = (i32, i32, i32)
module mk_lys (scenario: scenario): lys with text_content = text_content = {
  type~ state =
    {h: i64, w: i64,
     auto: bool,
     rng: rng,
     time: f32,
     steps_auto: i64,
     steps_auto_per_second: i32,
     steps: i64,
     grid: [][]cell,
     cycle_checks: [][][](direction flow)}

  type text_content = text_content

  let text_format () = "FPS: %d\nAuto: %[no|yes]\nSteps per second: %d"

  let text_content (fps: f32) (s: state): text_content =
    (t32 fps, i32.bool s.auto, s.steps_auto_per_second)

  let text_colour = const argb.white

  let scale = 10i64

  let init_grid (h: i64) (w: i64) (rng: rng): (rng, [h][w]cell, [][h][w](direction flow)) =
    let (rng, grid) = create_grid h w rng
    let grid = scenario.init grid
    let cycle_checks = find_cycles grid
    in (rng, grid, cycle_checks)

  let init (seed: u32) (h: i64) (w: i64): state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let (rng, grid, cycle_checks) = init_grid (h / scale) (w / scale) rng
    in {h, w, auto=false, rng, time=0, steps_auto=0, steps_auto_per_second=30, steps=0, grid, cycle_checks}

  let grab_mouse = false

  -- FIXME
  let resize (_h: i64) (_w: i64) (s: state): state =
    s
    -- s with h = h
    --   with w = w
    --   with grid = resize_grid s.grid h w s.rng

  let step (s: state): state =
    let (h, w) = (s.h / scale, s.w / scale)
    let grid = copy (s.grid :> [h][w]cell) -- FIXME: ugly
    let cycle_checks = s.cycle_checks :> [][h][w](direction flow)
    let grid = step h w cycle_checks grid
    let (rng, grid) = scenario.step grid s.steps s.rng
    in s with rng = rng
         with steps = s.steps + 1
         with grid = grid

let event (e: event) (s: state): state =
    match e
    case #step td ->
      if s.auto
      then let time = s.time + td
           let steps_goal = i64.f32 (time * f32.i32 s.steps_auto_per_second)
           let s = loop s for _i < steps_goal - s.steps_auto do step s
           in s with time = time
                with steps_auto = steps_goal
      else s
    case #keydown {key} ->
      if key == SDLK_SPACE
      then step s with auto = false
      else if key == SDLK_a
      then s with auto = !s.auto
      else if key == SDLK_UP
      then s with steps_auto_per_second = s.steps_auto_per_second + 1
      else if key == SDLK_DOWN
      then s with steps_auto_per_second = i32.max 1 (s.steps_auto_per_second - 1)
      else if key == SDLK_r
      then let (rng, grid, cycle_checks) = init_grid (s.h / scale) (s.w / scale) s.rng
           in s with rng = rng
                with grid = grid
                with cycle_checks = cycle_checks
                with auto = false
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
