import "math/vec"

-- | fragment type (in homogenous clip space)
type fragment 'varying = {pos: {x: f32, y: f32, z: f32, w: f32}, attr: varying}

-- | projected fragment type (in screen space)
type pfragment 'varying = {pos: {x: f32, y: f32}, depth: f32, Z_inv: f32, attr: varying}

-- user-defined varying specification
module type VaryingSpec = {
  -- | user-defined varying type
  type t

  -- | dummy zero value. additive identity
  val zero : t

  -- | user-defined addition implementation
  val (+) : t -> t -> t

  -- | user-defined subtraction implementation
  val (-) : t -> t -> t

  -- | user-defined scale implementation
  val (*) : f32 -> t -> t

  -- | user-defined scale inverse implementation
  val (/) : t -> f32 -> t
}

local
-- | derived fragment operations specification
module type derive_fragment_ops_spec = {
  -- | varying type
  type varying

  -- | fragment type
  type fragment

  -- | projected fragment type
  type pfragment

  -- | dummy fragment value. additive identity
  val zero_fragment : fragment

  -- | dummy projected fragment value. additive identity
  val zero_pfragment : pfragment

  -- | project a fragment
  val proj : fragment -> pfragment

  -- | unproject a fragment
  val unproj : pfragment -> fragment

  -- | linearly interpolate a fragment
  val lerp : fragment -> fragment -> f32 -> fragment

  -- | linearly interpolate fragments with perspective correction
  val lerp_with_Z_inv : pfragment -> pfragment -> f32 -> pfragment

  -- | mix triangle fragments with perspective correction
  val barycentric : (pfragment, pfragment, pfragment) -> (f32, f32, f32) -> pfragment

  -- | map from screen space to window space (with y pointing downwards)
  val map_screen_to_window : {w: i64, h: i64} -> pfragment -> pfragment
}

-- | module for deriving fragment operations
module derive_fragment_ops (Varying: VaryingSpec)
  : derive_fragment_ops_spec
    with varying = Varying.t
    with fragment = fragment Varying.t
    with pfragment = pfragment Varying.t = {
  type varying = Varying.t
  type fragment = fragment Varying.t
  type pfragment = pfragment Varying.t

  def zero_fragment =
    {pos = {x = 0f32, y = 0f32, z = 0f32, w = 1f32}, attr = Varying.zero}

  def zero_pfragment =
    {pos = {x = 0f32, y = 0f32}, depth = (-1f32), Z_inv = 1f32, attr = Varying.zero}

  def proj ({pos = {x, y, z, w}, attr}: fragment) : pfragment =
    let Z_inv = 1 / w
    let x' = x * Z_inv
    let y' = y * Z_inv
    let depth = z * Z_inv
    in {pos = {x = x', y = y'}, depth, Z_inv, attr}

  def unproj ({pos = {x, y}, depth, Z_inv, attr}: pfragment) : fragment =
    let w' = 1 / Z_inv
    let x' = x * w'
    let y' = y * w'
    let z' = depth * w'
    in {pos = {x = x', y = y', z = z', w = w'}, attr}

  def lerp (lhs: fragment) (rhs: fragment) (t: f32) : fragment =
    let w0 = (1 - t)
    let w1 = t
    let pos = (vec4f.+) ((vec4f.*) w0 lhs.pos) ((vec4f.*) w1 rhs.pos)
    let attr = (Varying.+) ((Varying.*) w0 lhs.attr) ((Varying.*) w1 rhs.attr)
    in {pos, attr}

  def lerp_with_Z_inv (lhs: pfragment) (rhs: pfragment) (t: f32) : pfragment =
    let w0 = (1 - t)
    let w1 = t
    let pos = (vec2f.+) ((vec2f.*) w0 lhs.pos) ((vec2f.*) w1 rhs.pos)
    let depth = (f32.+) ((f32.*) w0 lhs.depth) ((f32.*) w1 rhs.depth)
    let Z_inv = (f32.+) ((f32.*) w0 lhs.Z_inv) ((f32.*) w1 rhs.Z_inv)
    let attr = (Varying.+) ((Varying.*) w0 lhs.attr) ((Varying.*) w1 rhs.attr)
    in {pos, depth, Z_inv, attr}

  def barycentric (v0: pfragment, v1: pfragment, v2: pfragment) (alpha: f32, beta: f32, gamma: f32) : pfragment =
    let f = \x y z -> alpha * x + beta * y + gamma * z
    let x = f v0.pos.x v1.pos.x v2.pos.x
    let y = f v0.pos.y v1.pos.y v2.pos.y
    let depth = f v0.depth v1.depth v2.depth
    let Z_inv = f v0.Z_inv v1.Z_inv v2.Z_inv
    let attr0 = (Varying.*) (alpha * v0.Z_inv) v0.attr
    let attr1 = (Varying.*) (beta * v1.Z_inv) v1.attr
    let attr2 = (Varying.*) (gamma * v2.Z_inv) v2.attr
    let attr = (Varying./) ((Varying.+) ((Varying.+) attr0 attr1) attr2) Z_inv
    in {pos = {x, y}, depth, Z_inv, attr}

  def map_screen_to_window {w = w: i64, h = h: i64} ({pos = {x, y}, depth, Z_inv, attr}: pfragment) : pfragment =
    -- Map screen [-1;1]x[-1;1] to [0;1]x[0;1]
    let x' = (x + 1) / 2
    let y' = (y + 1) / 2
    -- Map [0;1]x[0;1] to window [0;w]x[-h;0]
    let x'' = x' * f32.i64 w
    let y'' = (-y') * f32.i64 h
    in {pos = {x = x'', y = y''}, depth, Z_inv, attr}
}
