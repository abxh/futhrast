-- tiled triangle rasterizer
-- assumes non-zero triangle area and counterclockwise winding order

import "../../../diku-dk/segmented/segmented"
import "../../../diku-dk/sorts/radix_sort"

import "../utils/bitmask"
import "../utils/segmented_radix_sort"

import "../types"
import "../math/vec"

-- | triangle rasterizer specfication
module type TriangleRasterizerSpec =
  (V: VaryingSpec)
  -> {
    -- | rasterize triangle given plot function, depth selection function,
    -- triangle fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: fragment V.t -> target)
      -> (depth_select: f32 -> f32 -> f32)
      -> (ne: (target, f32))
      -> ([h][w]target, [h][w]f32)
      -> [n](fragment V.t, fragment V.t, fragment V.t)
      -> ([h][w]target, [h][w]f32)
  }

-- | tiled triangle rasterizer with binning
module TiledTriangleRasterizer : TriangleRasterizerSpec = \(V: VaryingSpec) ->
  {
    local module V = VaryingExtensions (V)

    local module F32 = VaryingExtensions (f32)

    local
    type triangle = (fragment V.t, fragment V.t, fragment V.t)

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
      local open vec2f

      def calc_wcoeffs ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) (p: vec2f.t) : vec3f.t =
        let v0p = p - v0
        let v1p = p - v1
        let v2p = p - v2
        let v1v2 = v2 - v1
        let v2v0 = v0 - v2
        let v0v1 = v1 - v0
        in (cross v1v2 v1p, cross v2v0 v2p, cross v0v1 v0p) |> vec3f.from_tuple

      def calc_wdelta ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) : {x: vec3f.t, y: vec3f.t} =
        let v1v2 = v2 - v1
        let v2v0 = v0 - v2
        let v0v1 = v1 - v0
        let delta_wx = (f32.neg v1v2.y, f32.neg v2v0.y, f32.neg v0v1.y) |> vec3f.from_tuple
        let delta_wy = (v1v2.x, v2v0.x, v0v1.x) |> vec3f.from_tuple
        in {x = delta_wx, y = delta_wy}
    }

    open wcoeffs

    def div_ceil (n: i64) (m: i64) = (n + (m - 1)) / m

    def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

    def calc_signed_tri_area_2 ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) : f32 =
      let v0v1 = v1 vec2f.- v0
      let v0v2 = v2 vec2f.- v0
      let signed_area_2 = v0v1 `vec2f.cross` v0v2
      in signed_area_2

    def ensure_cclockwise_winding_order ((f0, f1, f2): triangle) : triangle =
      if calc_signed_tri_area_2 (f0.pos, f1.pos, f2.pos) >= 0 then (f0, f1, f2) else (f0, f2, f1)

    def fine_rasterize [n] [m]
                       {h = _: i64, w = w: i64}
                       (tris: []triangle)
                       ((active_tile_ids, tile_flags, tile_ids, tri_indices): ([m]u16, [n]bool, [n]u16, [n]i64)) =
      let bins_w = w `div_ceil` bin_size
      let tiles_per_bin =
        assert (fine_size * fine_size == fine_mask.num_bits)
        (coarse_size * coarse_size)
      let f (tile_id, tri_index) =
        let bin_index = i64.u16 tile_id / tiles_per_bin
        let tile_index = i64.u16 tile_id %% tiles_per_bin
        let bin_xmin = (bin_index %% bins_w) * bin_size
        let bin_ymin = (bin_index / bins_w) * bin_size
        let tile_xmin = (tile_index %% coarse_size) * fine_size + bin_xmin
        let tile_ymin = (tile_index / coarse_size) * fine_size + bin_ymin
        let (f0, f1, f2) = tris[tri_index]
        let verts = (f0.pos, f1.pos, f2.pos)
        let wzero = calc_wcoeffs verts {x = f32.i64 tile_xmin, y = f32.i64 tile_ymin}
        let wdelta = calc_wdelta verts
        let g pixel_index =
          let y = 0.5 + f32.i64 (pixel_index / fine_size)
          let x = 0.5 + f32.i64 (pixel_index %% fine_size)
          let w = wzero vec3f.+ (x vec3f.* wdelta.x) vec3f.+ (y vec3f.* wdelta.y)
          in w.x >= 0 && w.y >= 0 && w.z >= 0
        let mask = fine_mask.from_pred_seq g
        in (tile_id, tri_index, mask)
      let sz ((_, _, mask)) = fine_mask.size mask
      let get ((tile_id, tri_index, mask)) set_pixel_index =
        let bin_index = i64.u16 tile_id / tiles_per_bin
        let tile_index = i64.u16 tile_id %% tiles_per_bin
        let bin_xmin = (bin_index %% bins_w) * bin_size
        let bin_ymin = (bin_index / bins_w) * bin_size
        let tile_xmin = (tile_index %% coarse_size) * fine_size + bin_xmin
        let tile_ymin = (tile_index / coarse_size) * fine_size + bin_ymin
        let pixel_index = fine_mask.find_ith_set_bit mask set_pixel_index
        let pixel_x = pixel_index %% fine_size
        let pixel_y = pixel_index / fine_size
        let x = pixel_x + tile_xmin
        let y = pixel_y + tile_ymin
        let (f0, f1, f2) = tris[tri_index]
        let verts = (f0.pos, f1.pos, f2.pos)
        let area_2 = calc_signed_tri_area_2 verts
        let pos = {x = 0.5 + f32.i64 x, y = 0.5 + f32.i64 y}
        let (w0, w1, w2) = calc_wcoeffs verts pos |> vec3f.to_tuple
        let (w0, w1, w2) = (w0 / area_2, w1 / area_2, w2 / area_2)
        let w = (w0, w1, w2)
        let Z_inv = barycentric f0.Z_inv f1.Z_inv f2.Z_inv w
        let depth = barycentric_affine Z_inv (f0.depth, f0.Z_inv) (f1.depth, f1.Z_inv) (f2.depth, f2.Z_inv) w
        let attr = barycentric_affine_attr Z_inv (f0.attr, f0.Z_inv) (f1.attr, f1.Z_inv) (f2.attr, f2.Z_inv) w
        in ((y - tile_ymin, x - tile_xmin), {pos, Z_inv, depth, attr}, depth)
      let arr = zip tile_ids tri_indices |> map f
      let szs = map sz arr
      let tile_counts = segmented_reduce (+) 0 tile_flags szs
      let (is, frags, depths) =
        repl_segm_iota szs
        |> uncurry (map2 (\i j -> get arr[i] j))
        |> unzip3
      in (active_tile_ids, tile_counts, is, frags, depths)

    def calc_tri_bbox ((f0, f1, f2): triangle) : bbox i64 =
      let (p0, p1, p2) = (f0.pos, f1.pos, f2.pos)
      in { xmin = (p0.x `f32.min` p1.x `f32.min` p2.x) |> f32.floor >-> i64.f32
         , ymin = (p0.y `f32.min` p1.y `f32.min` p2.y) |> f32.floor >-> i64.f32
         , xmax = (p0.x `f32.max` p1.x `f32.max` p2.x) |> f32.ceil >-> i64.f32
         , ymax = (p0.y `f32.max` p1.y `f32.max` p2.y) |> f32.ceil >-> i64.f32
         }

    def bbox_overlaps (a: bbox i64) (b: bbox i64) =
      !(a.xmax <= b.xmin || a.xmin >= b.xmax || a.ymax <= b.ymin || a.ymin >= b.ymax)

    def tri_overlaps_bbox (bbox: bbox i64) (wzero: vec3f.t) (wdelta: {x: vec3f.t, y: vec3f.t}) =
      let w0 = wzero vec3f.+ (f32.i64 bbox.xmin vec3f.* wdelta.x) vec3f.+ (f32.i64 bbox.ymin vec3f.* wdelta.y)
      let w1 = w0 vec3f.+ (f32.i64 bin_size vec3f.* wdelta.y)
      let w2 = w0 vec3f.+ (f32.i64 bin_size vec3f.* wdelta.x)
      let w3 = w2 vec3f.+ (f32.i64 bin_size vec3f.* wdelta.y)
      let is_outside proj = proj w0 < 0 && proj w1 < 0 && proj w2 < 0 && proj w3 < 0
      in !(is_outside (.x) || is_outside (.y) || is_outside (.z))

    def calc_segment_flags_u8 [n] (is: [n]u8) : [n]bool =
      if n == 0
      then replicate n true
      else replicate n true with [1:n] = map2 (!=) (tail is) (init is)

    def calc_segment_flags_u16 [n] (is: [n]u16) : [n]bool =
      if n == 0
      then replicate n true
      else replicate n true with [1:n] = map2 (!=) (tail is) (init is)

    def coarse_rasterize [n]
                         {h = h: i64, w = w: i64}
                         (tris: []triangle)
                         ((bin_flags, bin_indices, tri_indices): ([n]bool, [n]u8, [n]i64)) =
      let fb_bbox = {xmin = 0, ymin = 0, xmax = w, ymax = h}
      let bins_w = w `div_ceil` bin_size
      let tiles_per_bin = coarse_size * coarse_size
      let num_bits =
        assert (tiles_per_bin == coarse_mask.num_bits && tiles_per_bin <= i64.u8 u8.highest)
        ilog2 tiles_per_bin
      let f (bin_index, tri_index) =
        let (f0, f1, f2) = tris[tri_index]
        let tri_bbox = calc_tri_bbox (f0, f1, f2)
        let bin_xmin = (i64.u8 bin_index %% bins_w) * bin_size
        let bin_ymin = (i64.u8 bin_index / bins_w) * bin_size
        let verts = (f0.pos, f1.pos, f2.pos)
        let wzero = calc_wcoeffs verts {x = 0, y = 0}
        let wdelta = calc_wdelta verts
        let f (tile_index: i64) =
          let tile_bbox =
            let xmin = (tile_index %% coarse_size) * fine_size + bin_xmin
            let ymin = (tile_index / coarse_size) * fine_size + bin_ymin
            let xmax = xmin + fine_size
            let ymax = ymin + fine_size
            in {xmin, ymin, xmax, ymax}
          in if !bbox_overlaps tile_bbox fb_bbox || !bbox_overlaps tile_bbox tri_bbox
             then false
             else tri_overlaps_bbox tile_bbox wzero wdelta
        let mask = coarse_mask.from_pred_seq f
        in (bin_index, mask)
      let sz (_, (_, mask)) = coarse_mask.size mask
      let get (tri_index, (bin_index, mask)) set_tile_index =
        let tile_index = coarse_mask.find_ith_set_bit mask set_tile_index
        in (bin_index, u8.i64 tile_index, tri_indices[tri_index])
      let arr =
        zip bin_indices tri_indices
        |> map f
        |> zip (iota n)
      let szs = map sz arr
      let bin_counts = segmented_reduce (+) 0 bin_flags szs
      let (tile_ids, tri_indices) =
        repl_segm_iota szs
        |> uncurry (map2 (\i j -> get arr[i] j))
        |> segmented_radix_sort_by_key (.1) (i32.i64 num_bits) u8.get_bit bin_counts
        |> map (\(bin_index, tile_index, tri_index) ->
                  let tile_id = u16.u8 bin_index * u16.i64 tiles_per_bin + u16.u8 tile_index
                  in (tile_id, tri_index))
        |> unzip
      let tile_flags = calc_segment_flags_u16 tile_ids
      let active_tile_ids = segmented_reduce u16.max u16.lowest tile_flags tile_ids
      in (active_tile_ids, tile_flags, tile_ids, tri_indices)

    def bin_rasterize {h = h: i64, w = w: i64} (tris: []triangle) =
      let bins_h = h `div_ceil` bin_size
      let bins_w = w `div_ceil` bin_size
      let num_bits =
        assert (ilog2 (bins_h * bins_w) <= i64.i32 u8.num_bits)
        ilog2 (bins_h * bins_w)
      let f tri_index =
        let tri_bbox = calc_tri_bbox tris[tri_index]
        let bin_bbox =
          let xmin = (tri_bbox.xmin / bin_size) `i64.max` 0
          let ymin = (tri_bbox.ymin / bin_size) `i64.max` 0
          let xmax = (tri_bbox.xmax `div_ceil` bin_size) `i64.min` bins_w
          let ymax = (tri_bbox.ymax `div_ceil` bin_size) `i64.min` bins_h
          in {xmin, ymin, xmax, ymax}
        in (tri_index, bin_bbox)
      let sz (_, bbox) =
        let bbox_h = bbox.ymax - bbox.ymin
        let bbox_w = bbox.xmax - bbox.xmin
        in (bbox_h `i64.max` 0) * (bbox_w `i64.max` 0)
      let get (tri_index, bbox) bbox_index =
        let bbox_w = bbox.xmax - bbox.xmin
        let dy = bbox_index / bbox_w
        let dx = bbox_index %% bbox_w
        let x = bbox.xmin + dx
        let y = bbox.ymin + dy
        let bin_index = y * bins_w + x
        in (u8.i64 bin_index, tri_index)
      let (bin_indices, tri_indices) =
        indices tris
        |> map f
        |> expand sz get
        |> radix_sort_by_key (.0) (i32.i64 num_bits) u8.get_bit
        |> unzip
      let bin_flags = calc_segment_flags_u8 bin_indices
      in (bin_flags, bin_indices, tri_indices)

    def rasterize 'target [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_select: f32 -> f32 -> f32)
                  ((ne_target, ne_depth): (target, f32))
                  ((target_buffer, depth_buffer): ([h][w]target, [h][w]f32))
                  (tris: [](fragment V.t, fragment V.t, fragment V.t)) : ([h][w]target, [h][w]f32) =
      let bins_w = w `div_ceil` bin_size
      let tiles_per_bin = coarse_size * coarse_size
      let tile_size = fine_size * fine_size
      let tris = tris |> map ensure_cclockwise_winding_order
      let (active_tile_ids, tile_counts, is, frag_values, depth_values) =
        bin_rasterize {h, w} tris
        |> coarse_rasterize {h, w} tris
        |> fine_rasterize {h, w} tris
      let tile_offsets = exscan (+) 0 tile_counts
      let tabulate_tile {tile_xmin, tile_ymin} proj =
        let f pixel_y pixel_x =
          let x = pixel_x + tile_xmin
          let y = pixel_y + tile_ymin
          in proj {x, y}
        in tabulate_2d fine_size fine_size f
      let f i =
        let tile_id = active_tile_ids[i]
        let tri_count = tile_counts[i]
        let tri_offset = tile_offsets[i]
        let num_chunks = tri_count `div_ceil` tile_size
        let bin_index = i64.u16 tile_id / tiles_per_bin
        let tile_index = i64.u16 tile_id %% tiles_per_bin
        let bin_xmin = (bin_index %% bins_w) * bin_size
        let bin_ymin = (bin_index / bins_w) * bin_size
        let tile_xmin = (tile_index %% coarse_size) * fine_size + bin_xmin
        let tile_ymin = (tile_index / coarse_size) * fine_size + bin_ymin
        let depth_tile =
          loop depth_tile = tabulate_tile {tile_xmin, tile_ymin} (\{x, y} -> depth_buffer[y, x])
          for chunk_index < num_chunks do
            let g j =
              let tri_index = chunk_index * tile_size + j
              in if tri_index < tri_count
                 then (is[tri_offset + tri_index], depth_values[tri_offset + tri_index])
                 else ((-1, -1), depth_values[tri_offset + tri_index])
            let (is, depth_values) =
              tabulate tile_size g
              |> unzip
            in reduce_by_index_2d depth_tile depth_select ne_depth is depth_values
        let target_tile =
          loop target_tile = tabulate_tile {tile_xmin, tile_ymin} (\{x, y} -> target_buffer[y, x])
          for chunk_index < num_chunks do
            let g j =
              let tri_index = chunk_index * tile_size + j
              in if tri_index < tri_count
                 then (is[tri_offset + tri_index], frag_values[tri_offset + tri_index])
                 else ((-1, -1), frag_values[tri_offset + tri_index])
            let (is, target_values) =
              tabulate tile_size g
              |> map (\((y, x), f) ->
                        if (0 <= x && x < fine_size) && (0 <= y && y < fine_size) && depth_tile[y, x] == f.depth
                        then ((y, x), plot f)
                        else ((-1, -1), ne_target))
              |> unzip
            in scatter_2d target_tile is target_values
        in zip (flatten target_tile) (flatten depth_tile)
      let tabulate_tile_indices tile_id =
        let bin_index = i64.u16 tile_id / tiles_per_bin
        let tile_index = i64.u16 tile_id %% tiles_per_bin
        let bin_xmin = (bin_index %% bins_w) * bin_size
        let bin_ymin = (bin_index / bins_w) * bin_size
        let tile_xmin = (tile_index %% coarse_size) * fine_size + bin_xmin
        let tile_ymin = (tile_index / coarse_size) * fine_size + bin_ymin
        let f pixel_y pixel_x =
          let x = pixel_x + tile_xmin
          let y = pixel_y + tile_ymin
          in (y, x)
        in tabulate_2d fine_size fine_size f |> flatten
      let active_tiles = #[incremental_flattening(only_intra)] tabulate (length active_tile_ids) f
      let dest =
        zip (flatten target_buffer) (flatten depth_buffer) |> unflatten
      let is = (map (\i -> tabulate_tile_indices active_tile_ids[i]) (iota (length active_tile_ids))) |> flatten
      let (tbuf, dbuf) =
        active_tiles
        |> flatten
        |> scatter_2d (copy dest) is
        |> flatten
        |> unzip
      in (unflatten tbuf, unflatten dbuf)
  }

-- | triangle rasterizer for testing purposes. can use the REPL for this
module TiledTriangleRasterizerTest = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = TiledTriangleRasterizer (V)

  def rasterize_triangle_tiled_test [n] (h: i64) (w: i64) (vs: [n]((f32, f32), (f32, f32), (f32, f32))) : [h][w]i32 =
    let target_buffer = replicate h (replicate w false)
    let depth_buffer = replicate h (replicate w (-f32.inf))
    let frags =
      vs
      |> map (\(f0, f1, f2) ->
                ( {pos = {x = f0.0, y = f0.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f1.0, y = f1.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f2.0, y = f2.1}, depth = 1, Z_inv = 1, attr = true}
                ))
    let plot = (\(f: fragment bool) -> f.attr)
    let depth_select (lhs: f32) (rhs: f32) = if lhs > rhs then lhs else rhs
    in M.rasterize plot depth_select (false, -f32.inf) (target_buffer, depth_buffer) frags
       |> (.0)
       |> map (map i32.bool)
}

open TiledTriangleRasterizerTest
