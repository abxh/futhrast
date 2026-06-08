-- ==
-- compiled input @ bunny.in
-- compiled input @ monkey.in
-- compiled input @ penger.in
-- compiled input @ dragon.in
-- compiled input @ lucy.in
-- compiled input @ armadillo.in

import "test_template"

def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

module O : TiledPinedaTriangleRasterizerOptions = {
  module coarse_mask = bitmask_64
  module bin_pattern = linear_pattern
  module coarse_pattern = linear_pattern

  def bin_shift : i64 = 7
  def fine_shift : i64 = 4
  def num_intrablocks_shift : i64 = ilog2 256
}

open RTemplate O

def main = main
