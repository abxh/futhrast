-- | vertex and fragment types

-- | processed vertex type (in homogenous clip space)
type vertex_out 'varying = {pos: {x: f32, y: f32, z: f32, w: f32}, attr: varying}

-- | fragment type (in screen space)
type fragment 'varying = {pos: {x: f32, y: f32}, depth: f32, Z_inv: f32, attr: varying}

-- | project a vertex
def proj 'varying ({pos = {x, y, z, w}, attr}: vertex_out varying) : fragment varying =
  let Z_inv = 1 / w
  let x' = x * Z_inv
  let y' = y * Z_inv
  let depth = z * Z_inv
  in {pos = {x = x', y = y'}, depth, Z_inv, attr}

-- | unproject a vertex
def unproj 'varying ({pos = {x, y}, depth, Z_inv, attr}: fragment varying) : vertex_out varying =
  let w' = 1 / Z_inv
  let x' = x * w'
  let y' = y * w'
  let z' = depth * w'
  in {pos = {x = x', y = y', z = z', w = w'}, attr}
