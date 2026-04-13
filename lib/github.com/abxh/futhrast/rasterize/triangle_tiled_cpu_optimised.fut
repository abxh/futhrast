-- tiled triangle rasterizer optimised for cpu

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

-- | tiled triangle rasterizer optimised for the cpu
module TriangleTiledRasterizerCPUOptimised : TriangleRasterizerSpec = \(V: VaryingSpec) ->
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

    module coarse_mask = bitmask_64
    module fine_mask = bitmask_256

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
    }

    -- def calc_wdelta ((v0, v1, v2): (vec2i32.t, vec2i32.t, vec2i32.t)) : (vec3i32.t, vec3i32.t) =
    --   let v1v2 = v2 - v1
    --   let v2v0 = v0 - v2
    --   let v0v1 = v1 - v0
    --   let delta_wx = (i32.neg v1v2.y, i32.neg v2v0.y, i32.neg v0v1.y) |> vec3i32.from_tuple
    --   let delta_wy = (v1v2.x, v2v0.x, v0v1.x) |> vec3i32.from_tuple
    --   in (delta_wx, delta_wy)

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

    def tri_overlaps_bbox (bbox: bbox i64) ((f0, f1, f2): triangle) =
      let p = (f0.pos, f1.pos, f2.pos)
      let w0 = calc_wcoeffs p {x = i32.i64 bbox.xmin, y = i32.i64 bbox.ymin}
      let w1 = calc_wcoeffs p {x = i32.i64 bbox.xmin, y = i32.i64 bbox.ymax}
      let w2 = calc_wcoeffs p {x = i32.i64 bbox.xmax, y = i32.i64 bbox.ymin}
      let w3 = calc_wcoeffs p {x = i32.i64 bbox.xmax, y = i32.i64 bbox.ymax}
      let is_outside proj = proj w0 < 0 && proj w1 < 0 && proj w2 < 0 && proj w3 < 0
      in !(is_outside (.x) || is_outside (.y) || is_outside (.z))

    def fine_rasterize [n] 'target [h] [w]
                       (plot: (fragment V.t -> target))
                       (depth_cmp: f32 -> f32 -> #left | #right)
                       (ne: (target, f32))
                       (dest: [h][w](target, f32))
                       (tris: []triangle)
                       ((tile_ids, tri_indices): ([n]i64, [n]i64)) : [h][w](target, f32) =
      let bins_w = w `div_ceil` bin_size
      let bins_h = h `div_ceil` bin_size
      let tiles_per_bin =
        assert (fine_size * fine_size == fine_mask.num_bits) (coarse_size * coarse_size)
      let num_tiles = bins_w * bins_h * tiles_per_bin
      let (tile_ids, tri_indices) =
        zip tile_ids tri_indices
        |> radix_sort_by_key (.0) (i32.i64 (ilog2 num_tiles)) i64.get_bit
        |> unzip
      let cmp = (\f0 f1 -> match depth_cmp f0.1 f1.1 case #left -> f0 case #right -> f1)
      let active_tiles =
        spread num_tiles false tile_ids (replicate n true)
        |> zip (iota num_tiles)
        |> filter (\(_, flag) -> flag)
        |> map (.0)
      let counts = segmented_reduce (+) 0 (map2 (!=) tile_ids (rotate (-1) tile_ids)) (replicate n 1)
      let offsets = counts |> exscan (+) 0
      let is =
        let f tile_id =
          let bin_index = tile_id / tiles_per_bin
          let tile_index = tile_id %% tiles_per_bin
          let bin_x = bin_index %% bins_w
          let bin_y = bin_index / bins_w
          let tile_x = tile_index %% coarse_size
          let tile_y = tile_index / coarse_size
          let f pixel_y pixel_x =
            let x = bin_x * bin_size + tile_x * fine_size + pixel_x
            let y = bin_y * bin_size + tile_y * fine_size + pixel_y
            in (y, x)
          in tabulate_2d fine_size fine_size f |> flatten
        in active_tiles |> map f |> flatten
      in zip active_tiles (indices active_tiles)
         |> map (\(tile_id, i) ->
                   let bin_index = tile_id / tiles_per_bin
                   let tile_index = tile_id %% tiles_per_bin
                   let bin_xmin = (bin_index %% bins_w) * bin_size
                   let bin_ymin = (bin_index / bins_w) * bin_size
                   let tile_xmin = (tile_index %% coarse_size) * fine_size + bin_xmin
                   let tile_ymin = (tile_index / coarse_size) * fine_size + bin_ymin
                   let tile =
                     tabulate_2d fine_size fine_size (\dy dx ->
                                                        let y = tile_ymin + dy
                                                        let x = tile_xmin + dx
                                                        in if y < h && x < w then dest[y, x] else ne)
                   let (is, target_values, depth_values) =
                     tri_indices
                     |> drop offsets[i]
                     |> take counts[i]
                     |> map (\tri_index ->
                               let (f0, f1, f2) = tris[tri_index]
                               let verts = (f0.pos, f1.pos, f2.pos)
                               let g pixel_index =
                                 let y = tile_ymin + (pixel_index / fine_size) |> i32.i64
                                 let x = tile_xmin + (pixel_index %% fine_size) |> i32.i64
                                 let w = calc_wcoeffs verts {x, y}
                                 in w.x >= 0 && w.y >= 0 && w.z >= 0
                               let mask = fine_mask.from_pred g
                               in (tri_index, mask))
                     |> expand (\((_, mask)) -> fine_mask.size mask)
                               (\((tri_index, mask)) set_pixel_index ->
                                  let pixel_index = fine_mask.find_ith_set_bit mask set_pixel_index
                                  let pixel_x = pixel_index %% fine_size
                                  let pixel_y = pixel_index / fine_size
                                  let x = pixel_x + tile_xmin
                                  let y = pixel_y + tile_ymin
                                  let (f0, f1, f2) = tris[tri_index]
                                  let verts = (f0.pos, f1.pos, f2.pos)
                                  let area_2 = calc_signed_tri_area_2 (f0, f1, f2)
                                  let (w0, w1, w2) = calc_wcoeffs verts {x = i32.i64 x, y = i32.i64 y} |> vec3i32.to_tuple
                                  let (w0, w1, w2) = (f32.i32 w0 / f32.i32 area_2, f32.i32 w1 / f32.i32 area_2, f32.i32 w2 / f32.i32 area_2)
                                  let w = (w0, w1, w2)
                                  let pos = {x = f32.i64 x, y = f32.i64 y}
                                  let Z_inv = barycentric f0.Z_inv f1.Z_inv f2.Z_inv w
                                  let depth = barycentric_affine Z_inv (f0.depth, f0.Z_inv) (f1.depth, f1.Z_inv) (f2.depth, f2.Z_inv) w
                                  let attr = barycentric_affine_attr Z_inv (f0.attr, f0.Z_inv) (f1.attr, f1.Z_inv) (f2.attr, f2.Z_inv) w
                                  in ((pixel_y, pixel_x), plot {pos, Z_inv, depth, attr}, depth))
                     |> unzip3
                   let as = zip target_values depth_values
                   in reduce_by_index_2d tile cmp ne is as |> flatten)
         |> flatten
         |> scatter_2d (copy dest) is

    def coarse_rasterize [n] {h = h: i64, w = w: i64} (tris: []triangle) ((bin_indices, tri_indices): ([n]i64, [n]i64)) =
      let fb_bbox = {xmin = 0, ymin = 0, xmax = w, ymax = h}
      let bins_w = w `div_ceil` bin_size
      let tiles_per_bin =
        assert (coarse_size * coarse_size == coarse_mask.num_bits) (coarse_size * coarse_size)
      in zip bin_indices tri_indices
         |> map (\(bin_index, tri_index) ->
                   let tri = tris[tri_index]
                   let tri_bbox = calc_tri_bbox tri
                   let bin_xmin = (bin_index %% bins_w) * bin_size
                   let bin_ymin = (bin_index / bins_w) * bin_size
                   let f (tile_index: i64) =
                     let tile_bbox =
                       let xmin = (tile_index %% coarse_size) * fine_size + bin_xmin
                       let ymin = (tile_index / coarse_size) * fine_size + bin_ymin
                       let xmax = xmin + fine_size
                       let ymax = ymin + fine_size
                       in {xmin, ymin, xmax, ymax}
                     in if !bbox_overlaps tile_bbox fb_bbox || !bbox_overlaps tile_bbox tri_bbox
                        then false
                        else tri_overlaps_bbox tile_bbox tri
                   let mask = coarse_mask.from_pred f
                   in (bin_index, mask))
         |> zip (iota n)
         |> expand (\(_, (_, mask)) -> coarse_mask.size mask)
                   (\(tri_index, (bin_index, mask)) set_tile_index ->
                      let tile_index = coarse_mask.find_ith_set_bit mask set_tile_index
                      let tile_id = bin_index * tiles_per_bin + tile_index
                      in (tile_id, tri_indices[tri_index]))
         |> unzip

    def bin_rasterize {h = h: i64, w = w: i64} (tris: []triangle) =
      let bins_h = h `div_ceil` bin_size
      let bins_w = w `div_ceil` bin_size
      in indices tris
         |> map (\tri_index ->
                   let tri_bbox = calc_tri_bbox tris[tri_index]
                   let xmin = (tri_bbox.xmin / bin_size) `i64.max` 0
                   let ymin = (tri_bbox.ymin / bin_size) `i64.max` 0
                   let xmax = (tri_bbox.xmax `div_ceil` bin_size) `i64.min` bins_w
                   let ymax = (tri_bbox.ymax `div_ceil` bin_size) `i64.min` bins_h
                   in (tri_index, {xmin, ymin, xmax, ymax}))
         |> expand (\(_, bbox) ->
                      let bbox_h = bbox.ymax - bbox.ymin
                      let bbox_w = bbox.xmax - bbox.xmin
                      in (bbox_h `i64.max` 0) * (bbox_w `i64.max` 0))
                   (\(tri_index, bbox) bbox_index ->
                      let bbox_w = bbox.xmax - bbox.xmin
                      let dy = bbox_index / bbox_w
                      let dx = bbox_index %% bbox_w
                      let x = bbox.xmin + dx
                      let y = bbox.ymin + dy
                      let bin_index = y * bins_w + x
                      in (bin_index, tri_index))
         |> filter (\(bin_index, tri_index) ->
                      let bin_bbox =
                        let xmin = (bin_index %% bins_w) * bin_size
                        let ymin = (bin_index / bins_w) * bin_size
                        let xmax = xmin + bin_size
                        let ymax = ymin + bin_size
                        in {xmin, ymin, xmax, ymax}
                      in tri_overlaps_bbox bin_bbox tris[tri_index])
         |> unzip

    def rasterize 'target [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_cmp: f32 -> f32 -> #left | #right)
                  (ne: (target, f32))
                  (dest: [h][w](target, f32))
                  (frags: [](fragment V.t, fragment V.t, fragment V.t)) : [h][w](target, f32) =
      let tris =
        frags
        |> map (\(f0, f1, f2) ->
                  ( round_fragment f0
                  , round_fragment f1
                  , round_fragment f2
                  ))
        |> filter (calc_signed_tri_area_2 >-> (i32.!=) 0)
        |> map ensure_cclockwise_winding_order
      in bin_rasterize {h, w} tris
         |> coarse_rasterize {h, w} tris
         |> fine_rasterize plot depth_cmp ne dest tris
  }
