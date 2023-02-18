import "../lib/github.com/athas/matte/colour"

import "random"
import "model"

def empty_direction 'base (empty: base): direction base =
  {y=empty, x=empty}

def create_underlying (rng: rng): underlying =
  {rng=rng,
   direction=empty_direction 0}

def empty_can_be_moved_to_from: can_be_moved_to_from =
  {calculated=false,
   direction=empty_direction false,
   next_preference_flow_x=false}

def has_underlying 'aux (cell: cell aux): bool =
  cell.underlying.direction.y != 0 || cell.underlying.direction.x != 0

def is_occupied 'aux (cell: cell aux): bool =
  cell.direction.y != 0 || cell.direction.x != 0

def flatten_coordinate (gw: i64) (y: i64) (x: i64): i64 =
  y * gw + x

def in_bounds (h: i64) (w: i64) (y: i64) (x: i64): bool =
  y >= 0 && y < h && x >= 0 && x < w

def nub_2d [l] [m] [n] (srcs: [l][m][n](direction flow)): [][m][n](direction flow) =
  let srcs' = zip srcs (0..<l)
  let (srcs'', _) = unzip (filter (\(src0, i0) -> all (\(src, i) -> i >= i0 || src0 != src) srcs') srcs')
  in srcs''
