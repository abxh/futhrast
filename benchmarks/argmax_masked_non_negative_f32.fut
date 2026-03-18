-- | ignore

def argmax_masked_non_negative_f32 [n] (mask: [n]bool) (xs: [n]f32) : (bool, f32, i64) =
  reduce_comm (\(m0, x0, i0) (m1, x1, i1) ->
                 let x0' = x0 + (f32.bool m0)
                 let x1' = x1 + (f32.bool m1)
                 in if x0' > x1' || (x0' == x1' && i0 < i1)
                    then (true, x0, i0)
                    else (m0 || m1, x1, i1))
              (false, -1f32, -1i64)
              (zip3 mask xs (iota n))

-- ==
-- input { 0i64 }
-- input { 1i64 }
-- input { 10i64 }
-- input { 100i64 }
-- input { 1000i64 }
-- input { 10000i64 }
-- input { 20000i64 }
-- input { 30000i64 }
-- input { 40000i64 }
-- input { 50000i64 }
-- input { 60000i64 }
-- input { 70000i64 }
-- input { 80000i64 }
-- input { 100000i64 }
-- input { 1000000i64 }
-- input { 10000000i64 }
-- input { 100000000i64 }

entry main (n: i64) : (bool, f32, i64) =
  let xs = map (\i -> f32.i64 i % 100f32) (iota n)
  let mask = map (\i -> i % 2 == 0) (iota n)
  in argmax_masked_non_negative_f32 mask xs
