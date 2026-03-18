
  def argmax_non_negative_f32 [n](xs: [n]f32) : (f32, i64) =
  reduce_comm (\(x0, i0) (x1, i1) ->
                 if x0 > x1 || (x0 == x1 && i0 < i1)
                    then (x0, i0)
                    else (x1, i1))
              (-1f32, -1i64)
              (zip xs (iota n))

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

entry main (n: i64) : (f32, i64) =
  let xs = map (\i -> f32.i64 i % 100f32) (iota n)
  in argmax_non_negative_f32 xs
