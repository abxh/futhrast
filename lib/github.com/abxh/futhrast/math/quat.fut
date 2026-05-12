-- | normalized quaternion implementation
--
-- Based on:
-- https://marctenbosch.com/quaternions/

import "vec"

module quat = {
  type t = vec4f.t

  -- | 'stack' two rotations on top of each other
  def (*) (lhs: t) (rhs: t) =
    let lhs_xyz = {x = lhs.x, y = lhs.y, z = lhs.z}
    let rhs_xyz = {x = rhs.x, y = rhs.y, z = rhs.z}
    let v =
      (lhs.w vec3f.* rhs_xyz)
      vec3f.+ (rhs.w vec3f.* lhs_xyz)
      vec3f.+ (vec3f.cross lhs_xyz rhs_xyz)
    let r =
      { x = v.x
      , y = v.y
      , z = v.z
      , w = lhs.w * rhs.w - (lhs_xyz `vec3f.dot` rhs_xyz)
      }
    -- note: always normalize for precision reasons. this could be made more sophisticated
    in vec4f.normalize r

  -- | rotation identity element
  def one : t = {x = 0, y = 0, z = 0, w = 1}

  -- | inverse element
  def conj (v: t) = {x = (-v.x), y = (-v.y), z = (-v.z), w = v.w}

  -- | apply the rotation to arbitary 3d vector
  def apply (q: vec4f.t) (v: vec3f.t) =
    let v = {x = v.x, y = v.y, z = v.z, w = 0}
    let r = q * v * (conj q)
    in {x = r.x, y = r.y, z = r.z}

  -- | construct quaternion from some axis and angle
  def axis_angle (axis: vec3f.t) (radians: f32) =
    -- note: normalize the axis in case it is not already normalized
    let axis = vec3f.normalize axis
    let v = f32.sin (radians / 2) vec3f.* axis
    in { x = v.x
       , y = v.y
       , z = v.z
       , w = f32.cos (radians / 2)
       }

  -- | rotate the YZ plane
  def rotate_x (radians: f32) = axis_angle {x = 1, y = 0, z = 0} (-radians)

  -- | rotate the XZ plane
  def rotate_y (radians: f32) = axis_angle {x = 0, y = 1, z = 0} (-radians)

  -- | rotate the XY plane
  def rotate_z (radians: f32) = axis_angle {x = 0, y = 0, z = 1} (-radians)

  -- | extract quaternion
  def to_vec (q: t) : vec4f.t = q

  -- | extract axis and angle
  def to_axis_angle (q: t) =
    let half_angle = f32.acos (q.w)
    in if f32.abs (half_angle) <= 0.0001
       then ({x = 1, y = 0, z = 0}, 2 f32.* half_angle)
       else let c = f32.sin half_angle
            in ({x = q.x / c, y = q.y / c, z = q.z / c}, 2 f32.* half_angle)

  -- | extract matrix
  def to_mat (q: t) =
    let x_col = apply q {x = 1, y = 0, z = 0}
    let y_col = apply q {x = 0, y = 1, z = 0}
    let z_col = apply q {x = 0, y = 0, z = 1}
    in [ [x_col.x, y_col.x, z_col.x, 0]
       , [x_col.y, y_col.y, z_col.y, 0]
       , [x_col.z, y_col.z, z_col.z, 0]
       , [0, 0, 0, 1]
       ]
}
