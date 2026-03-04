import "math/vec"

-- | fragment type (in homogenous clip space)
type fragment 'varying = {pos: {x: f32, y: f32, z: f32, w: f32}, attr: varying}

-- | projected fragment type (in screen space)
type pfragment 'varying = {pos: {x: f32, y: f32}, depth: f32, Z_inv: f32, attr: varying}

-- user-defined varying specification
module type VaryingSpec = {
  -- | user-defined varying type
  type t

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
module type derive_fragment_ops_spec = (V: VaryingSpec) ->
  {
   -- | project a fragment
   val proj : fragment V.t -> pfragment V.t

   -- | unproject a fragment
   val unproj : pfragment V.t -> fragment V.t

   -- | linearly interpolate a fragment
   val lerp : fragment V.t -> fragment V.t -> f32 -> fragment V.t

   -- | linearly interpolate fragments with perspective correction
   val lerp_with_Z_inv : pfragment V.t -> pfragment V.t -> f32 -> pfragment V.t

   -- | mix triangle fragments with perspective correction
   val barycentric : (pfragment V.t, pfragment V.t, pfragment V.t) -> (f32, f32, f32) -> pfragment V.t

   -- | map from screen space to window space (with y pointing downwards)
   val map_screen_to_window : {w: i64, h: i64} -> pfragment V.t -> pfragment V.t
  }

-- | module for deriving fragment operations
module derive_fragment_ops : derive_fragment_ops_spec = \(Varying: VaryingSpec) ->
  {
    local type fragment_t = fragment Varying.t
    local type pfragment_t = pfragment Varying.t

    def proj ({pos = {x, y, z, w}, attr}: fragment_t) : pfragment_t =
      let Z_inv = 1 / w
      let x' = x * Z_inv
      let y' = y * Z_inv
      let depth = z * Z_inv
      in {pos = {x = x', y = y'}, depth, Z_inv, attr}

    def unproj ({pos = {x, y}, depth, Z_inv, attr}: pfragment_t) : fragment_t =
      let w' = 1 / Z_inv
      let x' = x * w'
      let y' = y * w'
      let z' = depth * w'
      in {pos = {x = x', y = y', z = z', w = w'}, attr}

    def lerp (lhs: fragment_t) (rhs: fragment_t) (tval: f32) : fragment_t =
      let w0 = (1 - tval)
      let w1 = tval
      let pos = (vec4f.+) ((vec4f.*) w0 lhs.pos) ((vec4f.*) w1 rhs.pos)
      let attr = (Varying.+) ((Varying.*) w0 lhs.attr) ((Varying.*) w1 rhs.attr)
      in {pos, attr}

    def lerp_with_Z_inv (lhs: pfragment_t) (rhs: pfragment_t) (tval: f32) : pfragment_t =
      let w0 = (1 - tval)
      let w1 = tval
      let pos = (vec2f.+) ((vec2f.*) w0 lhs.pos) ((vec2f.*) w1 rhs.pos)
      let depth = (f32.+) ((f32.*) w0 lhs.depth) ((f32.*) w1 rhs.depth)
      let Z_inv = (f32.+) ((f32.*) w0 lhs.Z_inv) ((f32.*) w1 rhs.Z_inv)
      let attr = (Varying.+) ((Varying.*) w0 lhs.attr) ((Varying.*) w1 rhs.attr)
      in {pos, depth, Z_inv, attr}

    def barycentric (v0: pfragment_t, v1: pfragment_t, v2: pfragment_t) (alpha: f32, beta: f32, gamma: f32) : pfragment_t =
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

    def map_screen_to_window {w = w: i64, h = h: i64} ({pos = {x, y}, depth, Z_inv, attr}: pfragment_t) : pfragment_t =
      -- Map screen [-1;1]x[-1;1] to [0;1]x[0;1]
      let x' = (x + 1) / 2
      let y' = (y + 1) / 2
      -- Map [0;1]x[0;1] to window [0;w]x[-h;0]
      let x'' = x' * f32.i64 w
      let y'' = (-y') * f32.i64 h
      in {pos = {x = x'', y = y''}, depth, Z_inv, attr}
  }
