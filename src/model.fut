import "../lib/github.com/diku-dk/lys/lys"

import "random"

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

type cell_leak 'aux = (i64, i64, cell aux)

module type scenario = {
  val name: () -> string []

  val init [gh][gw]: *[gh][gw](cell ()) -> *[gh][gw](cell ())

  val step [gh][gw]: *[gh][gw](cell ()) -> i64 -> rng -> (rng, bool, *[gh][gw](cell ()))
}
