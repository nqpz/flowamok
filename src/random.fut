import "../lib/github.com/diku-dk/cpprandom/random"

module rnge = pcg32
module dist_i8 = uniform_int_distribution i8 rnge
module dist_f32 = uniform_real_distribution f32 rnge
type rng = rnge.rng
