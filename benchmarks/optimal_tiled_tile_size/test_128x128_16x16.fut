-- ==
-- compiled input @ dragon.in

import "../../lib/github.com/abxh/expand_masked/bitmask"
import "test_template"

def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

module O : TiledPinedaTriangleRasterizerOptions = {
  module coarse_mask = bitmask_64

  def bin_shift : i64 = ilog2 128
  def fine_shift : i64 = ilog2 16
}

open RTemplate O

def main = main
