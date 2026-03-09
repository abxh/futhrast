import "../../diku-dk/segmented/segmented"
import "../../diku-dk/sorts/radix_sort"

import "math/vec"
import "math/fixedpoint"
import "framebuffer"
import "config"
import "fragment"

type triangle 'varying =
  ( pfragment varying
  , pfragment varying
  , pfragment varying
  )

local
module tri_functions = {
  type pfragment_fp 'varying = {pos: {x: fixedpoint.t, y: fixedpoint.t}, depth: f32, Z_inv: f32, attr: varying}

  type triangle_fp 'varying =
    ( pfragment_fp varying
    , pfragment_fp varying
    , pfragment_fp varying
    )

  type triangle_fp_pos = (vec2fp.t, vec2fp.t, vec2fp.t)

  def conv_to_triangle_fp 'varying ((v0, v1, v2): triangle varying) : triangle_fp varying =
    let f = \v -> {pos = (vec2f.map) fixedpoint.f32 v.pos, depth = v.depth, Z_inv = v.Z_inv, attr = v.attr}
    in (f v0, f v1, f v2)

  def conv_to_triangle_float 'varying ((v0, v1, v2): triangle_fp varying) : triangle varying =
    let f = \v -> {pos = (vec2fp.map) fixedpoint.to_f32 v.pos, depth = v.depth, Z_inv = v.Z_inv, attr = v.attr}
    in (f v0, f v1, f v2)

  def get_triangle_fp_pos 'varying (t: triangle_fp varying) : triangle_fp_pos = (t.0.pos, t.1.pos, t.2.pos)

  def calc_tri_area_2 ((v0, v1, v2): (vec2fp.t, vec2fp.t, vec2fp.t)) : fixedpoint.t =
    -- assuming counterclockwise orientation
    let v0v1 = (vec2fp.-) v1 v0
    let v0v2 = (vec2fp.-) v2 v0
    in (vec2fp.cross) v0v1 v0v2
}

local
module wcoeffs_vec2fp = {
  local open vec2fp
  local open tri_functions

  -- | calculate coefficients used to determine whether a point is in a triangle
  def calc_wcoeffs ((v0, v1, v2): triangle_fp_pos) (p: vec2fp.t) : vec3fp.t =
    -- (unoptimized) calculation of:
    --    p = (x, y)
    --   w0 = cross(v1v2, v1p) // opposite of v0 is v1v2
    --   w1 = cross(v2v0, v2p) // opposite of v1 is v2v0
    --   w2 = cross(v0v1, v0p) // opposite of v2 is v0v1
    --
    --            ,.v2_
    --          .'   |´.
    --        .'        ´.
    --      .'     p      ´.
    --    |_                `
    --   v0----------------->v1
    --
    -- note: In SDL, y points *downwards*, so clockwise-oriented triangles should be
    -- passed to the rasterizer, if directly passed.

    let v0p = p - v0
    let v1p = p - v1
    let v2p = p - v2
    let v1v2 = v2 - v1
    let v2v0 = v0 - v2
    let v0v1 = v1 - v0
    in (cross v1v2 v1p, cross v2v0 v2p, cross v0v1 v0p) |> vec3fp.from_tuple

  -- | calculate delta terms to be reused with `w_at` function
  def calc_delta ((v0, v1, v2): triangle_fp_pos) : (vec3fp.t, vec3fp.t) =
    -- more optimised calculation of delta terms
    --
    -- note:
    --   cross(lhs, rhs) = lhs.x * rhs.y - rhs.x * lhs.y, for "2D" vectors
    --
    -- cross product terms:
    --   cross(vivj, {1, 0} + min - vi) - cross(vivj, min - vi) = -vivj.y
    --   cross(vivj, {0, 1} + min - vi) - cross(vivj, min - vi) = +vivj.x

    let v1v2 = v2 - v1
    let v2v0 = v0 - v2
    let v0v1 = v1 - v0
    let delta_wx = (fixedpoint.neg v1v2.y, fixedpoint.neg v2v0.y, fixedpoint.neg v0v1.y) |> vec3fp.from_tuple
    let delta_wy = (v1v2.x, v2v0.x, v0v1.x) |> vec3fp.from_tuple
    in (delta_wx, delta_wy)
}

local
module wcoeffs_vec3fp = {
  local open vec3fp

  def w_at (w_min: vec3fp.t) ((delta_wx, delta_wy): (vec3fp.t, vec3fp.t)) (b: vec2fp.t) : vec3fp.t =
    -- usage of delta terms:
    w_min + b.x * delta_wx + b.y * delta_wy
}

local
module wcoeffs_fp = {
  local open fixedpoint
  local open tri_functions

  -- small epsilon
  local def eps = fixedpoint.raw 1

  -- | calculate edge bias to handle two triangles sharing edges
  def calc_edge_bias (src: vec2fp.t)
                     (dest: vec2fp.t) : fixedpoint.t =
    let edge = (vec2fp.-) dest src
    -- note: `points_up` dependent on choice of coordinate system
    let points_up = edge.y > fixedpoint.zero
    let points_right = edge.x > fixedpoint.zero
    let (is_left_edge, is_top_edge) = (points_up, (fixedpoint.abs edge.y) <= eps && points_right)
    in if is_left_edge || is_top_edge then fixedpoint.zero else neg eps

  -- | calculate edge bias of each edge in triangle with respect to winding order
  def calc_triangle_edge_bias ((v0, v1, v2): triangle_fp_pos) : vec3fp.t =
    { x = calc_edge_bias v1 v2
    , y = calc_edge_bias v2 v0
    , z = calc_edge_bias v0 v1
    }

  -- | check if inside the triangle, given w coefficients
  def is_inside_triangle {x = w0: fixedpoint.t, y = w1: fixedpoint.t, z = w2: fixedpoint.t} : bool =
    w0 >= fixedpoint.zero && w1 >= fixedpoint.zero && w2 >= fixedpoint.zero

  -- | check if outside the triangle, given w coefficients
  def is_outside_triangle {x = w0: fixedpoint.t, y = w1: fixedpoint.t, z = w2: fixedpoint.t} : bool =
    w0 < fixedpoint.zero && w1 < fixedpoint.zero && w2 < fixedpoint.zero
}

module mk_rasterizer (Config: ConfigSpec) (Varying: VaryingSpec) (Target: {type t}) = {
  local module Framebuffer = mk_framebuffer Config Target
  local module Fragment = derive_fragment_ops Varying

  type^ plot_t = Fragment.pfragment_t -> Target.t
  type~ framebuffer_t = Framebuffer.t

  local open Config
  local open wcoeffs_fp
  local open wcoeffs_vec2fp
  local open wcoeffs_vec3fp
  local open tri_functions

  local
  type bbox2D 'a =
    { xmin: a
    , xmax: a
    , ymin: a
    , ymax: a
    }

  type triangle_fp_t = triangle_fp Varying.t

  local
  type triangle_info_t =
    { tri: triangle_fp_t
    , bbox: bbox2D i64
    , inv_area_2: f32
    , w_min: vec3fp.t
    , w_delta: (vec3fp.t, vec3fp.t)
    , w_bias: vec3fp.t
    }

  local type tile_t = framebuffer_tile [Config.tile_size] Target.t

  def calc_tri_value_at (p: vec2fp.t) (tri: triangle_info_t) =
    let w = p |> w_at tri.w_min tri.w_delta |> (vec3fp.+) tri.w_bias
    in if is_inside_triangle w
       then let weights = (vec3f.*) tri.inv_area_2 ((vec3fp.map) fixedpoint.to_f32 w) |> vec3f.to_tuple
            let pf = (Fragment.barycentric) (conv_to_triangle_float tri.tri) weights
            in (pf, pf.depth)
       else (Fragment.zero_pfragment, f32.lowest)

  def argmax [n] (xs: []f32) : (f32, i64) =
    reduce_comm (\(vx, ix) (vy, iy) ->
                   if vx > vy || (vx == vy && ix < iy)
                   then (vx, ix)
                   else (vy, iy))
                (f32.lowest, n)
                (zip xs (iota n))

  def rasterize_tile (plot: plot_t)
                     (tile: tile_t)
                     (tris: []triangle_info_t) : tile_t =
    let plot_f (by: i64) (bx: i64) =
      let px = (fixedpoint.+) (fixedpoint.i64 (bx + tile.bbox.xmin)) (fixedpoint.f32 0.5)
      let py = (fixedpoint.+) (fixedpoint.i64 (by + tile.bbox.ymin)) (fixedpoint.f32 0.5)
      let p = {x = px, y = py}
      let (pfs, depths) = map (calc_tri_value_at p) tris |> unzip
      let (best_depth, i) = argmax depths
      in if (f32.>) best_depth tile.depth[by][bx]
         then (plot pfs[i], best_depth)
         else (tile.target[by][bx], tile.depth[by][bx])
    let (target, depth) =
      tabulate_2d tile_size tile_size plot_f
      |> flatten
      |> unzip
      |> (\(x, y) -> (unflatten x, unflatten y))
    in tile with target = target with depth = depth

  def calc_tile_tri_mask (tile_bbox: bbox2D i64) (tri: triangle_info_t) =
    let w0 = w_at tri.w_min tri.w_delta <| {x = fixedpoint.i64 tile_bbox.xmin, y = fixedpoint.i64 tile_bbox.ymin}
    let w1 = w_at tri.w_min tri.w_delta <| {x = fixedpoint.i64 tile_bbox.xmin, y = fixedpoint.i64 tile_bbox.ymax}
    let w2 = w_at tri.w_min tri.w_delta <| {x = fixedpoint.i64 tile_bbox.xmax, y = fixedpoint.i64 tile_bbox.ymin}
    let w3 = w_at tri.w_min tri.w_delta <| {x = fixedpoint.i64 tile_bbox.xmax, y = fixedpoint.i64 tile_bbox.ymax}
    let tri_inside_tile =
      tile_bbox.xmin <= tri.bbox.xmin
      && tri.bbox.xmax <= tile_bbox.xmax
      && tile_bbox.ymin <= tri.bbox.ymin
      && tri.bbox.ymax <= tile_bbox.ymax
    in is_inside_triangle w0
       || is_inside_triangle w1
       || is_inside_triangle w2
       || is_inside_triangle w3
       || tri_inside_tile

  def calc_triangle_info (tri: triangle_fp_t) : triangle_info_t =
    let tup = get_triangle_fp_pos tri
    let inv_area_2 = calc_tri_area_2 tup |> fixedpoint.to_f32 |> (\x -> (f32./) 1 x)
    let w_bias = calc_triangle_edge_bias tup
    let w_delta = calc_delta tup
    let (p0, p1, p2) = tup
    let xmin = fixedpoint.min (fixedpoint.min p0.x p1.x) p2.x
    let xmax = fixedpoint.max (fixedpoint.max p0.x p1.x) p2.x
    let ymin = fixedpoint.min (fixedpoint.min p0.y p1.y) p2.y
    let ymax = fixedpoint.max (fixedpoint.max p0.y p1.y) p2.y
    let w_min = calc_wcoeffs tup {x = fixedpoint.i32 0, y = fixedpoint.i32 0}
    in { bbox =
           { xmin = fixedpoint.to_i64 xmin
           , xmax = fixedpoint.to_i64 xmax
           , ymin = fixedpoint.to_i64 ymin
           , ymax = fixedpoint.to_i64 ymax
           }
       , tri = (tri.0 with pos = p0, tri.1 with pos = p1, tri.2 with pos = p2)
       , inv_area_2
       , w_min
       , w_delta
       , w_bias
       }

  def rasterize (plot: plot_t)
                (tris: []triangle Varying.t)
                (fb: Framebuffer.t) : Framebuffer.t =
    let tris = map (conv_to_triangle_fp >-> calc_triangle_info) tris
    let tiles' = map (map (\tile -> rasterize_tile plot tile (filter (calc_tile_tri_mask tile.bbox) tris))) fb.tiles
    in fb with tiles = tiles'
}
