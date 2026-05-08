import "encode_f32"

-- ==
-- entry: test_identity
-- input { 0f32 }
-- output { true }
-- input { 1f32 }
-- output { true }
-- input { -1f32 }
-- output { true }
entry test_identity x =
  x == (encode_f32 >-> decode_f32) x

-- ==
-- entry: test_inf_identity
-- input { }
-- output { true }
entry test_inf_identity =
  f32.inf == (encode_f32 >-> decode_f32) f32.inf

-- ==
-- entry: test_neg_inf_identity
-- input { }
-- output { true }
entry test_neg_inf_identity =
  (-f32.inf) == (encode_f32 >-> decode_f32) (-f32.inf)

-- ==
-- entry: test_ordering
-- input { 0f32 1f32 }
-- output { true }
-- input { 1f32 0f32 }
-- output { false }
-- input { -1f32 1f32 }
-- output { true }
-- input { -1f32 0f32 }
-- output { true }
entry test_ordering x y =
  encode_f32 x < encode_f32 y

-- ==
-- entry: test_inf_ordering
-- input { 0f32 }
-- output { true }
-- input { 1f32 }
-- output { true }
-- input { -1f32 }
-- output { true }
entry test_inf_ordering x =
  encode_f32 x < encode_f32 f32.inf

-- ==
-- entry: test_neg_inf_ordering
-- input { 0f32 }
-- output { true }
-- input { 1f32 }
-- output { true }
-- input { -1f32 }
-- output { true }
entry test_neg_inf_ordering x =
  encode_f32 (-f32.inf) < encode_f32 x

-- ==
-- entry: test_inf_inf_ordering
-- input { }
-- output { true }
entry test_inf_inf_ordering =
  encode_f32 (-f32.inf) < encode_f32 f32.inf
