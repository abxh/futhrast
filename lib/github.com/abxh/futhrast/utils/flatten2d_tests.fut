import "flatten2d"

-- ==
-- entry: test_linear
-- input {}
-- output { true }
entry test_linear =
  let s = linear_pattern.setup {w, h}
  in reduce (&&) true
     <| map2 (==)
             (tabulate_2d h w (\y x -> (y, x)) |> flatten)
             (tabulate_2d h w (\y x -> linear_pattern.unflatten s <| linear_pattern.flatten s (y, x)) |> flatten)

-- ==
-- entry: test_diagonal
-- input {}
-- output { true }
entry test_diagonal =
  let s = diagonal_pattern.setup {w, h}
  in reduce (&&) true
     <| map2 (==)
             (tabulate_2d h w (\y x -> (y, x)) |> flatten)
             (tabulate_2d h w (\y x -> diagonal_pattern.unflatten s <| diagonal_pattern.flatten s (y, x)) |> flatten)

-- ==
-- entry: test_xshift_offset
-- input {}
-- output { true }
entry test_xshift_offset =
  let s = xshift_offset_pattern.setup {w, h}
  in reduce (&&) true
     <| map2 (==)
             (tabulate_2d h w (\y x -> (y, x)) |> flatten)
             (tabulate_2d h w (\y x -> xshift_offset_pattern.unflatten s <| xshift_offset_pattern.flatten s (y, x)) |> flatten)

-- ==
-- entry: test_morton_u16
-- input {}
-- output { true }
entry test_morton_u16 =
  let s = morton_u16_pattern.setup {w, h}
  in reduce (&&) true
     <| map2 (==)
             (tabulate_2d h w (\y x -> (y, x)) |> flatten)
             (tabulate_2d h w (\y x -> morton_u16_pattern.unflatten s <| morton_u16_pattern.flatten s (y, x)) |> flatten)

-- ==
-- entry: test_morton_u32
-- input {}
-- output { true }
entry test_morton_u32 =
  let s = morton_u32_pattern.setup {w, h}
  in reduce (&&) true
     <| map2 (==)
             (tabulate_2d h w (\y x -> (y, x)) |> flatten)
             (tabulate_2d h w (\y x -> morton_u32_pattern.unflatten s <| morton_u32_pattern.flatten s (y, x)) |> flatten)
