-- | vertex, fragment and varying types

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

-- | varying specification
module type VaryingSpec = {
  -- | user-defined varying type
  type t

  -- | idenity element
  val one : t

  -- | user-defined addition implementation
  val (+) : t -> t -> t

  -- | user-defined scale implementation
  val (*) : f32 -> t -> t
}

-- | varying extensions spec
module type VaryingExtensionsSpec =
  (V: VaryingSpec)
  -> {
       type t

       val one : t
       val zero : t

       val (+) : t -> t -> t
       val (-) : t -> t -> t
       val (*) : f32 -> t -> t
       val (/) : t -> f32 -> t

       val lerp : t -> t -> f32 -> t
       val lerp_perspective_corrected : (t, f32) -> (t, f32) -> f32 -> t
       val lerp_perspective_corrected_w_Z_inv_t : f32 -> (t, f32) -> (t, f32) -> f32 -> t

       val barycentric : t -> t -> t -> (f32, f32, f32) -> t
       val barycentric_perspective_corrected : (t, f32) -> (t, f32) -> (t, f32) -> (f32, f32, f32) -> t
       val barycentric_perspective_corrected_w_Z_inv_t : f32 -> (t, f32) -> (t, f32) -> (t, f32) -> (f32, f32, f32) -> t
     }
     with t = V.t

-- | varying extensions
module VaryingExtensions : VaryingExtensionsSpec = \(V: VaryingSpec) ->
  {
    open V

    def zero = 0 * one

    def (-) x y = x + (-1 * y)
    def (/) x s = (1 / s) * x

    def lerp (lhs: t) (rhs: t) (t: f32) =
      ((f32.-) 1 t) * lhs + t * rhs

    def lerp_perspective_corrected_w_Z_inv_t (Z_inv_t: f32) (lhs: t, Z_inv0: f32) (rhs: t, Z_inv1: f32) (t: f32) =
      (((f32.*) Z_inv0 ((f32.-) 1 t)) * lhs + ((f32.*) t Z_inv1) * rhs) / Z_inv_t

    def lerp_perspective_corrected (lhs: t, Z_inv0: f32) (rhs: t, Z_inv1: f32) (t: f32) =
      let f = lerp_perspective_corrected_w_Z_inv_t (f32.lerp Z_inv0 Z_inv1 t)
      in f (lhs, Z_inv0) (rhs, Z_inv1) t

    def barycentric (v0: t) (v1: t) (v2: t) ((w0, w1, w2): (f32, f32, f32)) : t =
      w0 * v0 + w1 * v1 + w2 * v2

    def barycentric_perspective_corrected_w_Z_inv_t (Z_inv_t: f32)
                                                    ((v0, Z_inv0): (t, f32))
                                                    ((v1, Z_inv1): (t, f32))
                                                    ((v2, Z_inv2): (t, f32))
                                                    ((w0, w1, w2): (f32, f32, f32)) : t =
      ((Z_inv0 f32.* w0 * v0) + (Z_inv1 f32.* w1 * v1) + (Z_inv2 f32.* w2 * v2)) / Z_inv_t

    def barycentric_perspective_corrected ((v0, Z_inv0): (t, f32))
                                          ((v1, Z_inv1): (t, f32))
                                          ((v2, Z_inv2): (t, f32))
                                          ((w0, w1, w2): (f32, f32, f32)) : t =
      let Z_inv_t = (w0 f32.* Z_inv0) f32.+ (w1 f32.* Z_inv1) f32.+ (w2 f32.* Z_inv2)
      in barycentric_perspective_corrected_w_Z_inv_t Z_inv_t (v0, Z_inv0) (v1, Z_inv1) (v2, Z_inv2) (w0, w1, w2)
  }
