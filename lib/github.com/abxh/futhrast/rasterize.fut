import "../../diku-dk/segmented/segmented"

import "math/vec"
import "math/fixedpoint"
import "framebuffer"
import "config"
import "fragment"
import "utils/bitset"

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

module mk_rasterizer (Varying: VaryingSpec) (FB: FramebufferSpec) = {
  local module Fragment = derive_fragment_ops Varying
  local module Config = FB.Config

  local open wcoeffs_fp
  local open wcoeffs_vec2fp
  local open tri_functions

  local
  type bbox2D 'a =
    { xmin: a
    , xmax: a
    , ymin: a
    , ymax: a
    }

  type triangle_t = triangle Varying.t
  type triangle_fp_t = triangle_fp Varying.t

  local
  type triangle_info_t =
    { verts: triangle_t
    , verts_fp: triangle_fp_t
    , bbox: bbox2D i64
    , inv_area_2: f32
    , w_min: vec3fp.t
    , w_delta: (vec3fp.t, vec3fp.t)
    , w_bias: vec3fp.t
    }

  local type tile_t = tile [FB.tile_size] FB.target

  type^ plot_t = Fragment.pfragment -> FB.target

  local
  module tri_mask = {
    local open fixedpoint

    def calc_tile_tri_mask (tile_bbox: bbox2D i64) (tri: triangle_info_t) =
      let tri_pos = get_triangle_fp_pos tri.verts_fp
      let w0 = calc_wcoeffs tri_pos {x = fixedpoint.i64 tile_bbox.xmin, y = fixedpoint.i64 tile_bbox.ymin}
      let w1 = calc_wcoeffs tri_pos {x = fixedpoint.i64 tile_bbox.xmin, y = fixedpoint.i64 tile_bbox.ymax}
      let w2 = calc_wcoeffs tri_pos {x = fixedpoint.i64 tile_bbox.xmax, y = fixedpoint.i64 tile_bbox.ymin}
      let w3 = calc_wcoeffs tri_pos {x = fixedpoint.i64 tile_bbox.xmax, y = fixedpoint.i64 tile_bbox.ymax}
      let is_outside proj = proj w0 < zero && proj w1 < zero && proj w2 < zero && proj w3 < zero
      in !(is_outside (.x) || is_outside (.y) || is_outside (.z))
  }

  local open tri_mask

  def calc_triangle_info (tri: triangle_t) : triangle_info_t =
    let verts_fp = conv_to_triangle_fp tri
    let tup = get_triangle_fp_pos verts_fp
    let inv_area_2 = calc_tri_area_2 tup |> fixedpoint.to_f32 |> (\x -> (f32./) 1 x)
    let w_bias = calc_triangle_edge_bias tup
    let w_delta = calc_delta tup
    let (p0, p1, p2) = tup
    let xmin = p0.x `fixedpoint.min` p1.x `fixedpoint.min` p2.x
    let xmax = p0.x `fixedpoint.max` p1.x `fixedpoint.max` p2.x
    let ymin = p0.y `fixedpoint.min` p1.y `fixedpoint.min` p2.y
    let ymax = p0.y `fixedpoint.max` p1.y `fixedpoint.max` p2.y
    let w_min = calc_wcoeffs tup {x = fixedpoint.i32 0, y = fixedpoint.i32 0}
    in { bbox =
           { xmin = fixedpoint.to_i64 xmin
           , xmax = fixedpoint.to_i64 xmax
           , ymin = fixedpoint.to_i64 ymin
           , ymax = fixedpoint.to_i64 ymax
           }
       , verts = tri
       , verts_fp
       , inv_area_2
       , w_min
       , w_delta
       , w_bias
       }

  def tri_pixel_at ({x, y}: vec2fp.t) (tri: triangle_info_t) =
    let w =
      (vec3fp.+) ((vec3fp.*) x tri.w_delta.0) ((vec3fp.*) y tri.w_delta.1)
      |> (vec3fp.+) tri.w_min
      |> (vec3fp.+) tri.w_bias
    in if is_inside_triangle w
       then let weight0 = tri.inv_area_2 * fixedpoint.to_f32 w.x
            let weight1 = tri.inv_area_2 * fixedpoint.to_f32 w.y
            let weight2 = 1 - (weight0 + weight1)
            in (Fragment.barycentric) tri.verts (weight0, weight1, weight2)
       else Fragment.zero_pfragment

  def argmax_non_negative_f32 [n] (xs: []f32) : (f32, i64) =
    reduce_comm (\(vx, ix) (vy, iy) ->
                   if vx > vy || (vx == vy && ix < iy)
                   then (vx, ix)
                   else (vy, iy))
                (-1, n)
                (zip xs (iota n))

  def best_pixel_at (plot: plot_t) (prev_target, prev_depth) (p: vec2fp.t) (tris: []triangle_info_t) =
    let (fs, possible_depths) =
      tris
      |> map (tri_pixel_at p)
      |> map (\f -> (f, f.depth))
      |> unzip
    let (best_depth, i) = argmax_non_negative_f32 possible_depths
    in if (f32.>) best_depth prev_depth
       then (plot fs[i], best_depth)
       else (prev_target, prev_depth)

  def rasterize_fine (plot: plot_t)
                     (tile: tile_t)
                     (tris: []triangle_info_t) : tile_t =
    let p =
      (vec2fp.+) {x = fixedpoint.i64 tile.bbox.xmin, y = fixedpoint.i64 tile.bbox.ymin}
                 {x = fixedpoint.f32 0.5, y = fixedpoint.f32 0.5}
    let buffer' =
      zip tile.buffer (indices tile.buffer)
      |> map (\(prev, i: i64) ->
                let dy = i / Config.tile_size
                let dx = i % Config.tile_size
                let offset = {x = fixedpoint.i64 dx, y = fixedpoint.i64 dy}
                in best_pixel_at plot prev ((vec2fp.+) p offset) tris)
    in tile with buffer = buffer'

  local module bitset = bitset u16 {def max_bits = Config.tri_block_size}

  def rasterize_coarse [k] [r]
                       (plot: plot_t)
                       (tris: [k * Config.tri_block_size + r]triangle_info_t)
                       (tiles: []tile_t) : []tile_t =
    let (blocks, rest) = split tris
    let tiles' =
      tiles
      |> map (\tile ->
                blocks
                |> unflatten
                |> map (\tri_block ->
                          bitset.init (length tri_block) (\i -> calc_tile_tri_mask tile.bbox tri_block[i])
                          |> bitset.to_array
                          |> map (\i -> tri_block[i])
                          |> rasterize_fine plot tile)
                |> reduce FB.merge_tiles (FB.default_tile tile.bbox)
                |> FB.merge_tiles (rasterize_fine plot tile rest))
    in tiles'

  def rasterize [n]
                (plot: plot_t)
                (tris: [n]triangle Varying.t)
                (fb: FB.t) : FB.t =
    let n_blocks = n / Config.tri_block_size
    let rest = n % Config.tri_block_size
    let tris = map calc_triangle_info tris |> sized (n_blocks * Config.tri_block_size + rest)
    let tiles' = rasterize_coarse plot tris <| FB.tiles fb
    in FB.set_tiles fb tiles'
}
