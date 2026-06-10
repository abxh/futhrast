-- ==
-- compiled input @ bunny.in
-- compiled input @ monkey.in
-- compiled input @ penger.in
-- compiled input @ dragon.in
-- compiled input @ lucy.in
-- compiled input @ armadillo.in

import "test_template"

module O : TiledPinedaTriangleRasterizerOptions = {
  module coarse_mask = bitmask_64
  module bin_pattern = diagonal_pattern
  module coarse_pattern = linear_pattern

  def bin_shift : i64 = 7
  def fine_shift : i64 = 4
}

open RTemplate O

def main = main
