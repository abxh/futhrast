-- ==
-- compiled input @ dragon.in

import "../../lib/github.com/abxh/expand_masked/bitmask"
import "triangle_imm_pineda_custom"
import "test_template"

def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

module O : ImmPinedaTriangleRasterizerOptions = {
  module coarse_mask = bitmask_64
  module fine_mask = bitmask_64

  def bin_shift : i64 = ilog2 64
  def fine_shift : i64 = ilog2 8
}

open RTemplate O

def main = main
