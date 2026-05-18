-- | basic affine transformation building blocks (except rotation, provided by quaternions)

import "../../../diku-dk/linalg/linalg"

import "vec"

local module linalg_f32 = mk_ordered_linalg f32

module transform = {
  def (*) = linalg_f32.matmul
  def inverse = linalg_f32.inv
  def transpose = transpose

  -- | apply to position vector
  def apply_to_pos (p: vec3f.t) (m: [4][4]f32) =
    let v = [p.x, p.y, p.z, 1]
    in linalg_f32.matvecmul_row m v |> vec4f.from_array

  -- | apply to position vector
  def apply_to_dir (p: vec3f.t) (m: [4][4]f32) =
    let v = [p.x, p.y, p.z, 0]
    in linalg_f32.matvecmul_row m v |> vec4f.from_array

  -- | (x,y,z) |-> (x, y, z)
  def identity : [4][4]f32 =
    [ [1, 0, 0, 0]
    , [0, 1, 0, 0]
    , [0, 0, 1, 0]
    , [0, 0, 0, 1]
    ]

  -- | (x,y,z) |-> (x + dx, y + dy, z + dz)
  def translate (dx: f32) (dy: f32) (dz: f32) : [4][4]f32 =
    [ [1, 0, 0, dx]
    , [0, 1, 0, dy]
    , [0, 0, 1, dz]
    , [0, 0, 0, 1]
    ]

  def translate_x dx = translate dx 0 0
  def translate_y dy = translate 0 dy 0
  def translate_z dz = translate 0 0 dz

  def translate_vec (v: {x: f32, y: f32, z: f32}) = translate v.x v.y v.z
  def translate_tup (dx: f32, dy: f32, dz: f32) = translate dx dy dz

  -- (x,y,z) |-> (x * sx, y * sy, z * sz)
  def scale (sx: f32) (sy: f32) (sz: f32) : [4][4]f32 =
    let sx = assert (sx != 0) sx
    let sy = assert (sy != 0) sy
    let sz = assert (sz != 0) sz
    in [ [sx, 0, 0, 0]
       , [0, sy, 0, 0]
       , [0, 0, sz, 0]
       , [0, 0, 0, 1]
       ]

  def scale_x sx = scale sx 0 0
  def scale_y sy = scale 0 sy 0
  def scale_z sz = scale 0 0 sz

  def reflect_x = scale (-1) 0 0
  def reflect_y = scale 0 (-1) 0
  def reflect_z = scale 0 0 (-1)

  def scale_vec (v: {x: f32, y: f32, z: f32}) = scale v.x v.y v.z
  def scale_tup (sx: f32, sy: f32, sz: f32) = scale sx sy sz

  -- (x,y,z) |-> (x + s * z, y + t * z, z)
  def sheer_xy (s: f32) (t: f32) : [4][4]f32 =
    [ [1, 0, s, 0]
    , [0, 1, t, 0]
    , [0, 0, 1, 0]
    , [0, 0, 0, 1]
    ]

  -- (x,y,z) |-> (x + s * y, y, z + t * y)
  def sheer_xz (s: f32) (t: f32) : [4][4]f32 =
    [ [1, s, 0, 0]
    , [0, 1, 0, 0]
    , [0, t, 1, 0]
    , [0, 0, 0, 1]
    ]

  -- (x,y,z) |-> (x, y + s * x, z + t * x)
  def sheer_yz (s: f32) (t: f32) : [4][4]f32 =
    [ [1, 0, 0, 0]
    , [s, 1, 0, 0]
    , [t, 0, 1, 0]
    , [0, 0, 0, 1]
    ]
}
