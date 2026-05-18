-- | Clipping related functions

import "fragment"
import "varying"
import "math/vec"

local
def clip_eps : f32 = 1e-5

local
#[inline]
def plane_dist (plane: i32) ({x, y, z, w}: vec4f.t) : f32 =
  -- inside iff plane_dist(i, v) >= 0
  -- left:   x + w >= 0
  -- right:  w - x >= 0
  -- bottom: y + w >= 0
  -- top:    w - y >= 0
  -- near:       z >= 0
  -- far:    w - z >= 0
  match plane
  case 0 -> x + w
  case 1 -> w - x
  case 2 -> y + w
  case 3 -> w - y
  case 4 -> z
  case _ -> w - z

-- | check whether to cull point, depending on whether it is in NDC space
def test_point_bounds 'varying (f: vertex_out varying) =
  #[inline]
  loop res = true
  for i < 6 do
    res && (plane_dist i f.pos >= 0)

local
-- | Liang-Barsky algorithm implementation
--
-- https://en.wikipedia.org/wiki/Liang%E2%80%93Barsky_algorithm
-- https://github.com/Larry57/WinForms3D/blob/master/WinForms3D/Clipping/LiangBarskyClippingHomogeneous.cs
#[inline]
def line_in_bounds_1d (q: f32) (p: f32) (t0: f32) (t1: f32) : (bool, f32, f32) =
  -- given i
  -- q =  plane_dist(i, p0)      (signed distance to border)
  -- p = -plane_dist(i, p1 - p0) (which is sign flipped to face border)
  if f32.abs p < clip_eps
  then (q >= 0, t0, t1)
  else let t = q / p
       in if p >= 0
          then (t >= t0, t0, f32.min t t1)
          else (t1 >= t, f32.max t t0, t1)

def test_line_bounds 'varying ((f0, f1): (vertex_out varying, vertex_out varying)) =
  let delta = f1.pos vec4f.- f0.pos
  in loop s = (true, 0, 1)
     for i < 6 do
       if !s.0
       then s
       else let q = plane_dist i f0.pos
            let p = -plane_dist i delta
            in line_in_bounds_1d q p s.1 s.2

local
-- | construct mask to check whether a point is *outside*
#[inline]
def clip_mask ({x, y, z, w}: vec4f.t) : u8 =
  0
  | (u8.bool (x < -w) << 0)
  | (u8.bool (x > w) << 1)
  | (u8.bool (y < -w) << 2)
  | (u8.bool (y > w) << 3)
  | (u8.bool (z < 0) << 4)
  | (u8.bool (z > w) << 5)

def test_triangle_is_partially_inside 'varying
                                      ((f0, f1, f2): ( vertex_out varying
                                                     , vertex_out varying
                                                     , vertex_out varying
                                                     )
                                      ) =
  let m0 = clip_mask f0.pos
  let m1 = clip_mask f1.pos
  let m2 = clip_mask f2.pos
  let and_mask = m0 & m1 & m2
  in and_mask == 0

def test_triangle_is_fully_inside 'varying
                                  ((f0, f1, f2): ( vertex_out varying
                                                 , vertex_out varying
                                                 , vertex_out varying
                                                 )
                                  ) =
  let m0 = clip_mask f0.pos
  let m1 = clip_mask f1.pos
  let m2 = clip_mask f2.pos
  let or_mask = m0 | m1 | m2
  in or_mask == 0

local
#[inline]
def safe_clip_t_value (fa: f32) (fb: f32) : f32 =
  let denom = fa - fb
  in if f32.abs denom < clip_eps
     then 0
     else (0 `f32.max` (fa / denom)) `f32.min` 1

local
-- | Sutherland-Hodgeman Algorithm
--
-- https://en.wikipedia.org/wiki/Sutherland%E2%80%93Hodgman_algorithm
#[inline]
def clip_against_plane 'varying
                       (lerp_vertex: vertex_out varying -> vertex_out varying -> f32 -> vertex_out varying)
                       (plane: i32)
                       { count = count: i64
                       , verts = verts: [9](vertex_out varying)
                       } =
  loop out = {count = 0i64, verts = copy verts}
  for i < count do
    let a = verts[(i + count - 1) %% count]
    let b = verts[i]
    let fa = plane_dist plane a.pos
    let fb = plane_dist plane b.pos
    let a_in = fa >= 0
    let b_in = fb >= 0
    in if a_in && b_in
       then out with count = out.count + 1
                with verts[out.count] = b
       else if a_in && !b_in
       then let t = safe_clip_t_value fa fb
            let intersection = lerp_vertex a b t
            in out with count = out.count + 1
                   with verts[out.count] = intersection
       else if !a_in && b_in
       then let t = safe_clip_t_value fa fb
            let intersection = lerp_vertex a b t
            in out with count = out.count + 2
                   with verts[out.count] = intersection
                   with verts[out.count + 1] = b
       else out

def clip_triangle 'varying
                  (lerp_vertex: vertex_out varying -> vertex_out varying -> f32 -> vertex_out varying)
                  (verts: [9](vertex_out varying)) =
  let clip_against_plane = clip_against_plane lerp_vertex
  in {count = 3, verts}
     |> clip_against_plane 0
     |> clip_against_plane 1
     |> clip_against_plane 2
     |> clip_against_plane 3
     |> clip_against_plane 4
     |> clip_against_plane 5
