-- | ignore

import "bitset"

module bitset = bitset u64 {def max_bits = 256i64}

-- ==
-- entry: size_zero_init
-- input { }
-- output { 0i64 }

entry size_zero_init =
  bitset.init 0 (\_ -> true)
  |> bitset.size

-- ==
-- entry: single_bit_true
-- input { }
-- output { 1i64 }

entry single_bit_true =
  bitset.init 1 (\_ -> true)
  |> bitset.size

-- ==
-- entry: single_bit_false
-- input { }
-- output { 0i64 }

entry single_bit_false =
  bitset.init 1 (\_ -> false)
  |> bitset.size

-- ==
-- entry: single_bit_member_true
-- input { }
-- output { true }

entry single_bit_member_true =
  let bs = bitset.init 1 (\_ -> true)
  in bitset.member 0 bs

-- ==
-- entry: single_bit_member_false
-- input { }
-- output { false }

entry single_bit_member_false =
  let bs = bitset.init 1 (\_ -> false)
  in bitset.member 0 bs

-- ==
-- entry: first_and_last_bits
-- input { }
-- output { [0i64,9i64] }

entry first_and_last_bits =
  bitset.init 10 (\i -> i == 0 || i == 9)
  |> bitset.to_array

-- ==
-- entry: sparse_extremes_size
-- input { }
-- output { 2i64 }

entry sparse_extremes_size =
  bitset.init 256 (\i -> i == 0 || i == 255)
  |> bitset.size

-- ==
-- entry: sparse_extremes_array
-- input { }
-- output { [0i64,255i64] }

entry sparse_extremes_array =
  bitset.init 256 (\i -> i == 0 || i == 255)
  |> bitset.to_array

-- ==
-- entry: dense_tail_bits
-- input { }
-- output { [7i64,8i64,9i64] }

entry dense_tail_bits =
  bitset.init 10 (\i -> i >= 7)
  |> bitset.to_array

-- ==
-- entry: alternating_large
-- input { }
-- output { 128i64 }

entry alternating_large =
  bitset.init 256 (\i -> i % 2 == 0)
  |> bitset.size

-- ==
-- entry: middle_gap
-- input { }
-- output { [0i64,1i64,8i64,9i64] }

entry middle_gap =
  bitset.init 10 (\i -> i < 2 || i > 7)
  |> bitset.to_array

-- ==
-- entry: member_high_boundary
-- input { }
-- output { true }

entry member_high_boundary =
  let bs = bitset.init 256 (\i -> i == 255)
  in bitset.member 255 bs

-- ==
-- entry: member_low_boundary
-- input { }
-- output { true }

entry member_low_boundary =
  let bs = bitset.init 256 (\i -> i == 0)
  in bitset.member 0 bs
