-- | Clipping related functions

import "fragment"
import "varying"
import "math/vec"

import "../../diku-dk/segmented/segmented"

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
  -- near:   w - z >= 0
  -- far:        z >= 0
  match plane
  case 0 -> x + w
  case 1 -> w - x
  case 2 -> y + w
  case 3 -> w - y
  case 4 -> w - z
  case _ -> z

-- | check whether to cull point, depending on whether it is in NDC space
def test_point_bounds 'varying (f: vertex_out varying) =
  #[inline]
  loop res = true for i < 6 do res && plane_dist i f.pos >= 0

local
-- | Liang-Barsky algorithm implementation
--
-- https://en.wikipedia.org/wiki/Liang%E2%80%93Barsky_algorithm
-- https://github.com/Larry57/WinForms3D/blob/master/WinForms3D/Clipping/LiangBarskyClippingHomogeneous.cs
#[inline]
def line_in_bounds_1d (q: f32) (p: f32) (t0: f32) (t1: f32) : (bool, (f32, f32)) =
  -- given i
  -- q : signed distance to border
  -- p : delta distance to border which is sign flipped to face border
  if f32.abs p < clip_eps
  then (q >= 0, (t0, t1))
  else let t = q / p
       in if p >= 0
          then (t >= t0, (t0, f32.min t t1))
          else (t1 >= t, (f32.max t t0, t1))

def test_line_bounds 'varying ((f0, f1): (vertex_out varying, vertex_out varying)) =
  let f i (t0, t1) =
    let d0 = plane_dist i f0.pos
    let d1 = plane_dist i f1.pos
    let p = d0 - d1
    let q = d0
    in line_in_bounds_1d q p t0 t1
  in (loop (i, (accept, res)) = (0, (true, (0, 1)))
      while i < 6 && accept do
        (i + 1, f i res)).1

def clip_lines 'varying
               (lerp_vertex: vertex_out varying -> vertex_out varying -> f32 -> vertex_out varying)
               (lines: [](vertex_out varying, vertex_out varying)) =
  lines
  |> map (\l -> (l, test_line_bounds l))
  |> filter ((.1) >-> (.0))
  |> map (\((f0, f1), (_, (t0, t1))) -> (lerp_vertex f0 f1 t0, lerp_vertex f0 f1 t1))

local
-- | construct mask to check whether a point is *outside*
#[inline]
def clip_mask ({x, y, z, w}: vec4f.t) : u8 =
  0
  | (u8.bool (x < -w) << 0)
  | (u8.bool (x > w) << 1)
  | (u8.bool (y < -w) << 2)
  | (u8.bool (y > w) << 3)
  | (u8.bool (z > w) << 4)
  | (u8.bool (z < 0) << 5)

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
  in and_mask == 0u8

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

type triangle 'varying =
  (vertex_out varying, vertex_out varying, vertex_out varying)

local
#[inline]
def safe_clip_t_value (da: f32) (db: f32) : f32 =
  let denom = da - db
  in if f32.abs denom < clip_eps
     then da / (f32.sgn da * clip_eps)
     else da / denom

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
            let tmp_count = out.count
            in out with count = out.count + 2
                   with verts[tmp_count] = intersection
                   with verts[tmp_count + 1] = b
       else out

def clip_triangles 'varying
                   (lerp_vertex: vertex_out varying -> vertex_out varying -> f32 -> vertex_out varying)
                   (tris: [](vertex_out varying, vertex_out varying, vertex_out varying)) =
  let (accepted_tris, partially_accepted_tris) =
    tris
    |> filter test_triangle_is_partially_inside
    |> partition test_triangle_is_fully_inside
  let g i =
    let global_index = i / 9
    let local_index = i %% 9
    in match local_index
       case 0 -> partially_accepted_tris[global_index].0
       case 1 -> partially_accepted_tris[global_index].1
       case _ -> partially_accepted_tris[global_index].2
  let clipped_tris =
    tabulate (length partially_accepted_tris * 9) g
    |> unflatten
    |> map (\verts ->
              loop res = {count = 3, verts}
              for i < 6 do
                clip_against_plane lerp_vertex i res)
    |> expand (\{count, verts = _} -> i64.bool (count > 2) * (count - 2))
              (\{count = _, verts} i -> (verts[0], verts[i + 1], verts[i + 2]))
  in accepted_tris ++ clipped_tris
