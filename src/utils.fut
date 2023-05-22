import "../lib/github.com/athas/matte/colour"

import "random"
import "helpers"
import "model"

def create_cell 'aux (aux: aux) (rng: rng): cell aux =
  {underlying=create_underlying rng,
   direction=empty_direction 0,
   color=argb.white,
   can_be_moved_to_from=empty_can_be_moved_to_from,
   aux=aux}

def create_grid 'aux (gh: i64) (gw: i64) (aux: aux) (rng: rng): (rng, *[gh][gw](cell aux)) =
  let rngs = rnge.split_rng (gh * gw) rng
  let cells = map (create_cell aux) rngs
  in (rnge.join_rng rngs, unflatten cells)

def add_line_vertical 'aux [gh][gw] (y_start: i64) (y_end: i64) (x: i64) (flow: flow) (cells: *[gh][gw](cell aux)):
                      *[gh][gw](cell aux) =
  let n = y_end - y_start + 1
  in loop cells
     for y < n
     do let cell' = copy (cells[y_start + y, x] with underlying.direction.y = flow)
        in cells with [y_start + y, x] = cell'

def add_line_horizontal 'aux [gh][gw] (y: i64) (x_start: i64) (x_end: i64) (flow: flow) (cells: *[gh][gw](cell aux)):
                        *[gh][gw](cell aux) =
  let n = x_end - x_start + 1
  in loop cells
     for x < n
     do let cell' = copy (cells[y, x_start + x] with underlying.direction.x = flow)
        in cells with [y, x_start + x] = cell'

def add_cell 'aux [gh][gw] (y: i64) (x: i64) (color: argb.colour) (direction: direction flow) (cells: *[gh][gw](cell aux)): *[gh][gw](cell aux) =
  let cell = cells[y, x] with color = color
                         with direction = direction
  in cells with [y, x] = copy cell
