-- | fragment and related type

-- | processed vertex type (in homogenous clip space)
type vertex_out 'varying = {pos: {x: f32, y: f32, z: f32, w: f32}, attr: varying}

-- | fragment type (in screen space)
type fragment_generic 'a 'varying = {pos: {x: a, y: a}, depth: f32, Z_inv: f32, attr: varying}

-- | specialization of fragment for typical use
type fragment 'varying = fragment_generic f32 varying

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

-- | internal screen to external window function (following SDL conventions)
def map_screen_to_window 'varying
                         {w = w: i64, h = h: i64}
                         ({pos = {x, y}, depth, Z_inv, attr}: fragment varying) : fragment varying =
  -- Map screen [-1;1]x[-1;1] to [0;1]x[0;1]
  let x' = (x + 1) / 2
  let y' = (y + 1) / 2
  -- Map [0;1]x[0;1] to window [0;w]x[-h;0]
  let x'' = x' * f32.i64 w
  let y'' = (-y') * f32.i64 h
  in {pos = {x = x'', y = y''}, depth, Z_inv, attr}
