-- | encode/decode f32 into sortable u32 keys

def encode_f32 (x: f32) : u32 =
  let bits = f32.to_bits x
  let sign = bits >> 31
  let mask = (1u32 << 31) | (0u32 - sign)
  in bits ^ mask

def decode_f32 (x: u32) : f32 =
  let sign = x >> 31
  let mask = (1u32 << 31) | (sign - 1u32)
  let bits = x ^ mask
  in f32.from_bits bits
