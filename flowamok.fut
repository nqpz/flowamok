import "lib/github.com/athas/matte/colour"

import "src/helpers"
open import "src/utils"
open import "src/random"
open import "src/model"
open import "src/steppers"

-- Useful for testing unrelated parts of the algorithm.
def choose_direction_random 'aux (_y: i64) (_x: i64) (cell: cell aux): (rng, direction flow) =
  let (rng, choice) = dist_i8.rand (0, 1) cell.underlying.rng
  in (rng, if choice == 0
           then {y=cell.underlying.direction.y, x=0}
           else {y=0, x=cell.underlying.direction.x})

def render 'aux (h: i64) (w: i64) (gh: i64) (gw: i64) (scale: i64) (grid: [gh][gw](cell aux)): [h][w]argb.colour =
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
