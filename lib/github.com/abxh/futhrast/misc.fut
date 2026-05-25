-- | misc constants and transformation matrices
--
-- NDC bounds : -1 <= x <= 1, -1 <= y <= 1, 0 <= depth(z) <= 1, where depth(z_far) = 0, depth(z_far) = 1.
-- Screen bounds: -1 <= x <= 1, -1 <= y <= 1

-- | right axis
def right = {x = -1f32, y = 0f32, z = 0f32}

-- | up axis
def up = {x = 0f32, y = 1f32, z = 0f32}

-- | forward axis
def forward = {x = 0f32, y = 0f32, z = 1f32}

-- | compute the orthographic depth (for shading purposes)
def depth_orthographic (near: f32) (far: f32) (z: f32) =
  let a = -1 / (far - near)
  let b = far / (far - near)
  in a * z + b

-- | compute the perspective depth (for shading purposes)
def depth_perspective (near: f32) (far: f32) (z: f32) =
  let a = -near / (far - near)
  let b = (near * far) / (far - near)
  in a + b / z

-- | compute the perspective depth with infinite far-plane (for shading purposes)
def depth_perspective_inf (near: f32) (z: f32) =
  let b = near
  in 0 + b / z

-- | make orthographic matrix, mapping to NDC space
def make_orthographic (near: f32)
                      (far: f32)
                      (aspect_ratio: f32) : [4][4]f32 =
  let near = assert (near > 0) near
  let far = assert (far > near) far
  let a = -1 / (far - near)
  let b = far / (far - near)
  in [ [1 / aspect_ratio, 0, 0, 0]
     , [0, 1, 0, 0]
     , [0, 0, a, b]
     , [0, 0, 0, 1]
     ]

-- | make perspective matrix, mapping to NDC space
def make_perspective (near: f32)
                     (far: f32)
                     (fovy_rad: f32)
                     (aspect_ratio: f32) : [4][4]f32 =
  let near = assert (near > 0) near
  let far = assert (far > near) far
  let c = f32.tan (fovy_rad / 2)
  let a = -near / (far - near)
  let b = (near * far) / (far - near)
  in [ [1 / (c * aspect_ratio), 0, 0, 0]
     , [0, 1 / c, 0, 0]
     , [0, 0, a, b]
     , [0, 0, 1, 0]
     ]

-- | make perspective matrix with infinite far plane, mapping to NDC space
def make_perspective_inf (near: f32)
                         (fovy_rad: f32)
                         (aspect_ratio: f32) : [4][4]f32 =
  let near = assert (near > 0) near
  let c = f32.tan (fovy_rad / 2)
  let b = near
  in [ [1 / (c * aspect_ratio), 0, 0, 0]
     , [0, 1 / c, 0, 0]
     , [0, 0, 0, b]
     , [0, 0, 1, 0]
     ]
