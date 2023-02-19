import "../lib/github.com/diku-dk/cpprandom/random"
import "../lib/github.com/athas/matte/colour"

module rnge = pcg32
module dist_i8 = uniform_int_distribution i8 rnge
module dist_f32 = uniform_real_distribution f32 rnge
type rng = rnge.rng

def random_color (rng: rng): (rng, argb.colour) =
  let (rng, color_r) = dist_f32.rand (0.2, 0.8) rng
  let (rng, color_g) = dist_f32.rand (0.2, 0.8) rng
  let (rng, color_b) = dist_f32.rand (0.2, 0.8) rng
  in (rng, argb.from_rgba color_r color_g color_b 1.0)
