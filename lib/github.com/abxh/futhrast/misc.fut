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

-- | make orthographic matrix with custom camera bounds, mapping to NDC space
def make_orthographic_custom (near: f32)
                             (far: f32)
                             (aspect_ratio: f32)
                             (depth_type: #normal_z | #reversed_z)
                             (min: vec2f)
                             (max: vec2f) : [4][4]f32 =
  let center_x = (min.x + max.x) * 0.5
  let center_y = (min.y + max.y) * 0.5
  let height = max.y - min.y
  let width = height * aspect_ratio
  let left = center_x - width * 0.5
  let right = center_x + width * 0.5
  let bottom = center_y - height * 0.5
  let top = center_y + height * 0.5
  let sx = 2f32 / (right - left)
  let sy = 2f32 / (top - bottom)
  let tx = -(right + left) / (right - left)
  let ty = -(top + bottom) / (top - bottom)
  let sz =
    match depth_type
    case #normal_z -> 1f32 / (far - near)
    case #reversed_z -> -1f32 / (far - near)
  let tz =
    match depth_type
    case #normal_z -> far / (far - near)
    case #reversed_z -> -near / (far - near)
  in [ [sx, 0, 0, tx]
     , [0, sy, 0, ty]
     , [0, 0, sz, tz]
     , [0, 0, 0, 1.0]
     ]

-- | make orthographic matrix, mapping to NDC space
def make_orthographic (near: f32)
                      (far: f32)
                      (aspect_ratio: f32)
                      (depth_type: #normal_z | #reversed_z) : [4][4]f32 =
  make_orthographic_custom near
                           far
                           aspect_ratio
                           depth_type
                           screen_bounds.min
                           screen_bounds.max

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
