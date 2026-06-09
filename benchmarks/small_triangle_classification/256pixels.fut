-- ==
-- compiled input @ bunny.in
-- compiled input @ monkey.in
-- compiled input @ penger.in
-- compiled input @ dragon.in
-- compiled input @ lucy.in
-- compiled input @ armadillo.in

import "test_template"

def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

module O : ImmPinedaTriangleRasterizerOptions = {
  module coarse_mask = bitmask_64
  module fine_mask = bitmask_256
  module small_triangle_mask = bitmask_256

  def bin_shift : i64 = ilog2 128
  def fine_shift : i64 = ilog2 16
  def small_triangle_size_shift : i64 = 8
}

open RTemplate O

def main = main
