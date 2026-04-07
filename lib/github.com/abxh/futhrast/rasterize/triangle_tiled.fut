-- tiled triangle rasterizer

-- can assert on get method?
-- ilog2 to limit number of bits used

import "../../../diku-dk/segmented/segmented"
import "../../../diku-dk/sorts/radix_sort"

import "../types"
import "../utils/bitmask"
import "../math/vec"

-- | triangle rasterizer specfication
module type TriangleRasterizerSpec =
  (V: VaryingSpec)
  -> {
    -- | rasterize triangle given plot function, depth comparision function,
    -- triangle fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: fragment V.t -> target)
      -> (depth_cmp: f32 -> f32 -> #left | #right)
      -> (ne: (target, f32))
      -> [h][w](target, f32)
      -> [n](fragment V.t, fragment V.t, fragment V.t)
      -> [h][w](target, f32)
  }

-- | tiled triangle rasterizer with binning
module TriangleTiledRasterizer : TriangleRasterizerSpec = \(V: VaryingSpec) ->
  {
    local module V = VaryingExtensions (V)

    local module F32 = VaryingExtensions (f32)

    type fragment_generic 'a 'varying =
      {pos: {x: a, y: a}, depth: f32, Z_inv: f32, attr: varying}

    local
    type triangle =
      ( fragment_generic i32 V.t
      , fragment_generic i32 V.t
      , fragment_generic i32 V.t
      )

    local
    type bbox 'a =
      { xmin: a
      , xmax: a
      , ymin: a
      , ymax: a
      }

    local def bin_size : i64 = 128i64
    local def fine_size : i64 = 16i64
    local def coarse_size : i64 = bin_size / fine_size

    def barycentric = F32.barycentric
    def barycentric_affine = F32.barycentric_perspective_corrected_w_Z_inv_t
    def barycentric_affine_attr = V.barycentric_perspective_corrected_w_Z_inv_t

    local
    module wcoeffs = {
      local open vec2i32

      def calc_wcoeffs ((v0, v1, v2): (vec2i32.t, vec2i32.t, vec2i32.t)) (p: vec2i32.t) : vec3i32.t =
        let v0p = p - v0
        let v1p = p - v1
        let v2p = p - v2
        let v1v2 = v2 - v1
        let v2v0 = v0 - v2
        let v0v1 = v1 - v0
        in (cross v1v2 v1p, cross v2v0 v2p, cross v0v1 v0p) |> vec3i32.from_tuple

      def calc_wdelta ((v0, v1, v2): (vec2i32.t, vec2i32.t, vec2i32.t)) : (vec3i32.t, vec3i32.t) =
        let v1v2 = v2 - v1
        let v2v0 = v0 - v2
        let v0v1 = v1 - v0
        let delta_wx = (i32.neg v1v2.y, i32.neg v2v0.y, i32.neg v0v1.y) |> vec3i32.from_tuple
        let delta_wy = (v1v2.x, v2v0.x, v0v1.x) |> vec3i32.from_tuple
        in (delta_wx, delta_wy)
    }

    open wcoeffs

    def hist_count [n] k (as: [n]i64) = hist (+) 0i64 k as (replicate n 1)

    def div_ceil (n: i64) (m: i64) = n / m + i64.bool ((n > 0) == (m > 0) && (n % m) != 0)

    def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

    def round_fragment (f: fragment_generic f32 V.t) : fragment_generic i32 V.t =
      { pos = {x = i32.f32 (f.pos.x + 0.5), y = i32.f32 (f.pos.y + 0.5)}
      , depth = f.depth
      , Z_inv = f.Z_inv
      , attr = f.attr
      }

    def calc_signed_tri_area_2 ((f0, f1, f2): triangle) : i32 =
      let (v0, v1, v2) = (f0.pos, f1.pos, f2.pos)
      let v0v1 = v1 vec2i32.- v0
      let v0v2 = v2 vec2i32.- v0
      let signed_area_2 = v0v1 `vec2i32.cross` v0v2
      in signed_area_2

    def ensure_cclockwise_winding_order ((f0, f1, f2): triangle) : triangle =
      if calc_signed_tri_area_2 (f0, f1, f2) >= 0 then (f0, f1, f2) else (f0, f2, f1)

    def calc_tri_bbox ((f0, f1, f2): triangle) : bbox i64 =
      let (p0, p1, p2) = (f0.pos, f1.pos, f2.pos)
      in { xmin = i64.i32 (p0.x `i32.min` p1.x `i32.min` p2.x)
         , ymin = i64.i32 (p0.y `i32.min` p1.y `i32.min` p2.y)
         , xmax = i64.i32 (p0.x `i32.max` p1.x `i32.max` p2.x) + 1
         , ymax = i64.i32 (p0.y `i32.max` p1.y `i32.max` p2.y) + 1
         }

    def bbox_overlaps (a: bbox i64) (b: bbox i64) =
      !(a.xmax <= b.xmin || a.xmin >= b.xmax || a.ymax <= b.ymin || a.ymin >= b.ymax)

    def tri_overlaps_bbox (bbox: bbox i64) (tri_bbox: bbox i64) ((f0, f1, f2): triangle) =
      if !bbox_overlaps bbox tri_bbox
      then false
      else let p = (f0.pos, f1.pos, f2.pos)
           let w0 = calc_wcoeffs p {x = i32.i64 bbox.xmin, y = i32.i64 bbox.ymin}
           let w1 = calc_wcoeffs p {x = i32.i64 bbox.xmin, y = i32.i64 bbox.ymax}
           let w2 = calc_wcoeffs p {x = i32.i64 bbox.xmax, y = i32.i64 bbox.ymin}
           let w3 = calc_wcoeffs p {x = i32.i64 bbox.xmax, y = i32.i64 bbox.ymax}
           let is_outside proj = proj w0 < 0 && proj w1 < 0 && proj w2 < 0 && proj w3 < 0
           in !(is_outside (.x) || is_outside (.y) || is_outside (.z))

    def calc_tile_info {tile_xmin = tile_xmin: i64, tile_ymin = tile_ymin: i64} ((f0, f1, f2): triangle) =
      let verts = (f0.pos, f1.pos, f2.pos)
      let g pixel_index =
        let y = tile_ymin + (pixel_index / fine_size) |> i32.i64
        let x = tile_xmin + (pixel_index % fine_size) |> i32.i64
        let w = calc_wcoeffs verts {x, y}
        in w.x >= 0 && w.y >= 0 && w.z >= 0
      let mask = bitmask_256.from_pred g
      in ((f0, f1, f2), mask)

    def index_tile_pixel 'target
                         (plot: (fragment V.t -> target))
                         {tile_xmin = tile_xmin: i64, tile_ymin = tile_ymin: i64}
                         ((f0, f1, f2): triangle, mask)
                         set_pixel_index =
      let pixel_index = bitmask_256.find_ith_set_bit mask set_pixel_index
      let pixel_x = pixel_index % fine_size
      let pixel_y = pixel_index / fine_size
      let x = pixel_x + tile_xmin
      let y = pixel_y + tile_ymin
      let verts = (f0.pos, f1.pos, f2.pos)
      let area_2 = calc_signed_tri_area_2 (f0, f1, f2)
      let (w0, w1, w2) = calc_wcoeffs verts {x = i32.i64 x, y = i32.i64 y} |> vec3i32.to_tuple
      let (w0, w1, w2) = (f32.i32 w0 / f32.i32 area_2, f32.i32 w1 / f32.i32 area_2, f32.i32 w2 / f32.i32 area_2)
      let w = (w0, w1, w2)
      let pos = {x = f32.i64 x, y = f32.i64 y}
      let Z_inv = barycentric f0.Z_inv f1.Z_inv f2.Z_inv w
      let depth = barycentric_affine Z_inv (f0.depth, f0.Z_inv) (f1.depth, f1.Z_inv) (f2.depth, f2.Z_inv) w
      let attr = barycentric_affine_attr Z_inv (f0.attr, f0.Z_inv) (f1.attr, f1.Z_inv) (f2.attr, f2.Z_inv) w
      in ((pixel_y, pixel_x), plot {pos, Z_inv, depth, attr}, depth)

    def coarse_rasterize {w = w: i64, h = h: i64}
                         {bin_xmin = bin_xmin: i64, bin_ymin = bin_ymin: i64}
                         (frags: []triangle) : ([]triangle, []i64) =
      let fb_bbox = {xmin = 0, ymin = 0, xmax = w, ymax = h}
      let (tile_values, tile_indices) =
        frags
        |> map (\tri ->
                  let f (tile_index: i64) =
                    let tile_bbox =
                      let xmin = (tile_index % coarse_size) * fine_size + bin_xmin
                      let ymin = (tile_index / coarse_size) * fine_size + bin_ymin
                      let xmax = xmin + fine_size
                      let ymax = ymin + fine_size
                      in {xmin, ymin, xmax, ymax}
                    in if !bbox_overlaps tile_bbox fb_bbox
                       then false
                       else let tri_bbox = calc_tri_bbox tri
                            in tri_overlaps_bbox tile_bbox tri_bbox tri
                  let mask = bitmask_64.from_pred f
                  in mask)
        |> zip (indices frags)
        |> expand (\(_, mask) -> bitmask_64.size mask)
                  (\(tri_index, mask) set_tile_index ->
                     let tile_index = bitmask_64.find_ith_set_bit mask set_tile_index
                     in (tri_index, tile_index))
        |> radix_sort_by_key (.1) (i32.i64 (ilog2 (coarse_size * coarse_size))) i64.get_bit
        |> map (\(tri_index, tile_index) -> (frags[tri_index], tile_index))
        |> unzip
      in (tile_values, hist_count (coarse_size * coarse_size) tile_indices)

    def bin_rasterize {bins_h = bins_h: i64, bins_w = bins_w: i64} (frags: []triangle) : ([]triangle, []i64) =
      let (bin_values, bin_indices) =
        frags
        |> map (\tri ->
                  let tri_bbox = calc_tri_bbox tri
                  let xmin = (tri_bbox.xmin / bin_size) `i64.max` 0
                  let ymin = (tri_bbox.ymin / bin_size) `i64.max` 0
                  let xmax = (tri_bbox.xmax `div_ceil` bin_size) `i64.min` bins_w
                  let ymax = (tri_bbox.ymax `div_ceil` bin_size) `i64.min` bins_h
                  in {xmin, ymin, xmax, ymax})
        |> zip (indices frags)
        |> expand (\(_, bbox) ->
                     let bbox_h = bbox.ymax - bbox.ymin
                     let bbox_w = bbox.xmax - bbox.xmin
                     in (bbox_h `i64.max` 0) * (bbox_w `i64.max` 0))
                  (\(tri_index, bbox) bbox_index ->
                     let bbox_w = bbox.xmax - bbox.xmin
                     let dy = bbox_index / bbox_w
                     let dx = bbox_index % bbox_w
                     let x = bbox.xmin + dx
                     let y = bbox.ymin + dy
                     let bin_index = y * bins_w + x
                     in (tri_index, bin_index))
        |> radix_sort_by_key (.1) (i32.i64 (ilog2 (bins_h * bins_w))) i64.get_bit
        |> map (\(tri_index, bin_index) -> (frags[tri_index], bin_index))
        |> unzip
      in (bin_values, hist_count (bins_h * bins_w) bin_indices)

    def rasterize 'target [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_cmp: f32 -> f32 -> #left | #right)
                  (ne: (target, f32))
                  (dest: [h][w](target, f32))
                  (frags: [](fragment V.t, fragment V.t, fragment V.t)) : [h][w](target, f32) =
      let bins_h = h `div_ceil` bin_size
      let bins_w = w `div_ceil` bin_size
      let (bin_values, bin_counts) =
        frags
        |> map (\(f0, f1, f2) ->
                  ( round_fragment f0
                  , round_fragment f1
                  , round_fragment f2
                  ))
        |> filter (calc_signed_tri_area_2 >-> (i32.!=) 0)
        |> map ensure_cclockwise_winding_order
        |> bin_rasterize {bins_h, bins_w}
      let bin_offsets = bin_counts |> exscan (+) 0
      let cmp f0 f1 = match depth_cmp f0.1 f1.1 case #left -> f0 case #right -> f1
      let binned_tiles =
        iota (bins_h * bins_w)
        |> map (\bin_index ->
                  let bin_xmin = (bin_index % bins_w) * bin_size
                  let bin_ymin = (bin_index / bins_w) * bin_size
                  let (tile_values, tile_counts) =
                    bin_values
                    |> drop bin_offsets[bin_index]
                    |> take bin_counts[bin_index]
                    |> coarse_rasterize {w, h} {bin_xmin, bin_ymin}
                  let tile_offsets = tile_counts |> exscan (+) 0
                  let tiles =
                    iota (coarse_size * coarse_size)
                    |> map (\tile_index ->
                              let tile_xmin = (tile_index % coarse_size) * fine_size + bin_xmin
                              let tile_ymin = (tile_index / coarse_size) * fine_size + bin_ymin
                              let (is, target_values, depth_values) =
                                tile_values
                                |> drop tile_offsets[tile_index]
                                |> take tile_counts[tile_index]
                                |> map (calc_tile_info {tile_xmin, tile_ymin})
                                |> expand (\(_, mask) -> bitmask_256.size mask)
                                          (index_tile_pixel plot {tile_xmin, tile_ymin})
                                |> unzip3
                              let as = zip target_values depth_values
                              let f pixel_y pixel_x =
                                let y = pixel_y + tile_ymin
                                let x = pixel_x + tile_xmin
                                in if y < h && x < w then dest[y, x] else ne
                              let res = reduce_by_index_2d (tabulate_2d fine_size fine_size f) cmp ne is as
                              in res)
                    |> unflatten
                  in tiles)
        |> unflatten
      let f y x =
        let bx = (x / bin_size)
        let by = (y / bin_size)
        let tx = (x % bin_size) / fine_size
        let ty = (y % bin_size) / fine_size
        let px = (x % fine_size)
        let py = (y % fine_size)
        in binned_tiles[by, bx, ty, tx, py, px]
      in tabulate_2d h w f
  }

-- | triangle rasterizer for testing purposes. can use the REPL for this
module TriangleTiledRasterizerTest = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = TriangleTiledRasterizer (V)

  def rasterize_triangle_tiled_test [n] (h: i64) (w: i64) (vs: [n]((f32, f32), (f32, f32), (f32, f32))) : [h][w]i32 =
    let dest = replicate h (replicate w (false, -f32.inf))
    let frags =
      vs
      |> map (\(f0, f1, f2) ->
                ( {pos = {x = f0.0, y = f0.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f1.0, y = f1.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f2.0, y = f2.1}, depth = 1, Z_inv = 1, attr = true}
                ))
    let plot = (\(f: fragment bool) -> f.attr)
    let depth_cmp (x: f32) (y: f32) = if x > y then #left else #right
    let (target_buf, _) =
      M.rasterize plot depth_cmp (false, -f32.inf) dest frags
      |> flatten
      |> unzip
    in target_buf
       |> unflatten
       |> map (map i32.bool)
}

open TriangleTiledRasterizerTest
