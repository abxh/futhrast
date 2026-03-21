-- | fragment type

-- | fragment type (in homogenous clip space)
type fragment 'varying = {pos: {x: f32, y: f32, z: f32, w: f32}, attr: varying}

-- | projected fragment type (in screen space)
type pfragment_generic 'a 'varying = {pos: {x: a, y: a}, depth: f32, Z_inv: f32, attr: varying}

-- | specialization of pfragment for typical use
type pfragment 'varying = pfragment_generic f32 varying

-- | project a fragment
def proj 'varying ({pos = {x, y, z, w}, attr}: fragment varying) : pfragment varying =
  let Z_inv = 1 / w
  let x' = x * Z_inv
  let y' = y * Z_inv
  let depth = z * Z_inv
  in {pos = {x = x', y = y'}, depth, Z_inv, attr}

-- | unproject a fragment
def unproj 'varying ({pos = {x, y}, depth, Z_inv, attr}: pfragment varying) : fragment varying =
  let w' = 1 / Z_inv
  let x' = x * w'
  let y' = y * w'
  let z' = depth * w'
  in {pos = {x = x', y = y', z = z', w = w'}, attr}
