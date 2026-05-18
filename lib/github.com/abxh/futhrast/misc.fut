-- | misc constants and transformation matrices

type vec2f = {x: f32, y: f32}
type vec3f = {x: f32, y: f32, z: f32}

-- | right axis
def right : vec3f = {x = 1, y = 0, z = 0}

-- | up axis
def up : vec3f = {x = 0, y = 1, z = 0}

-- | forward axis
def forward : vec3f = {x = 0, y = 0, z = 1}

-- | bounds for NDC (Normalized Device Coordinates)
-- This the bounding box, vertex positions are to be mapped to,
-- before vertices outside of it are clipped / culled.
def ndc_bounds : {min: vec3f, max: vec3f} =
  {min = {x = (-1), y = (-1), z = 0}, max = {x = 1, y = 1, z = 1}}

-- | screen boundaries
def screen_bounds : {min: vec2f, max: vec2f} =
  {min = {x = (-1), y = (-1)}, max = {x = 1, y = 1}}

-- | make orthographic matrix, mapping to NDC space
def make_orthographic (near: f32)
                      (far: f32)
                      (aspect_ratio: f32)
                      (depth_type: #normal_z | #reversed_z) : [4][4]f32 =
  let inv = 1f32 / (far - near)
  let sx = 1f32 / aspect_ratio
  let sy = 1f32
  let sz =
    match depth_type
    case #normal_z -> inv
    case #reversed_z -> -inv
  let tz =
    match depth_type
    case #normal_z -> far * inv
    case #reversed_z -> -near * inv
  in [ [sx, 0, 0, 0]
     , [0, sy, 0, 0]
     , [0, 0, sz, tz]
     , [0, 0, 0, 1]
     ]

-- | make perspective matrix, mapping to NDC space
def make_perspective (near: f32)
                     (far: f32)
                     (fovy_rad: f32)
                     (aspect_ratio: f32)
                     (depth_type: #normal_z | #reversed_z) : [4][4]f32 =
  let tan_half_fov = f32.tan (fovy_rad / 2f32)
  let sx = tan_half_fov * aspect_ratio
  let sy = tan_half_fov
  let a =
    match depth_type
    case #normal_z -> -near / (far - near)
    case #reversed_z -> near / (far - near)
  let b =
    match depth_type
    case #normal_z -> -far * a
    case #reversed_z -> -far * a
  in [ [1 / sx, 0, 0, 0]
     , [0, 1 / sy, 0, 0]
     , [0, 0, a, b]
     , [0, 0, 1, 0]
     ]
