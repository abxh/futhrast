-- tiled triangle rasteri zer
-- assumes non-zero triangle area

import "../../../diku-dk/segmented/segmented"
import "../../../diku-dk/sorts/radix_sort"

import "../utils/bitmask"

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

    local
    module F32 = VaryingExtensions (
      {
        open f32
      })

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
    module fine_mask = bitmask_64

    local def bin_shift : i64 = 6
    local def fine_shift : i64 = 3
    local def coarse_shift : i64 = bin_shift - fine_shift
    local def frag_block_shift : i64 = 7

    local def bin_size : i64 = 1 << bin_shift
    local def fine_size : i64 = 1 << fine_shift
    local def coarse_size : i64 = 1 << coarse_shift
    local def frag_block_size : i64 = 1 << frag_block_shift

    def barycentric = F32.barycentric
    def barycentric_pc = F32.barycentric_perspective_corrected_w_Z_inv_t
    def barycentric_pc_attr = V.barycentric_perspective_corrected_w_Z_inv_t

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

    def is_pow2 (n: i64) : bool = n > 0 && n & (n - 1) == 0

    def ilog2_ceil (n: i64) : i64 = i64.bool (!is_pow2 n) + i64.i32 (63 - i64.clz n)

    def calc_signed_tri_area_2 ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) : f32 =
      let v0v1 = v1 vec2f.- v0
      let v0v2 = v2 vec2f.- v0
      let signed_area_2 = v0v1 `vec2f.cross` v0v2
      in signed_area_2

    def ensure_cclockwise_winding_order ((f0, f1, f2): triangle) : triangle =
      if calc_signed_tri_area_2 (f0.pos, f1.pos, f2.pos) >= 0 then (f0, f1, f2) else (f0, f2, f1)

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

    def calc_segment_flags_u16 [n] (is: [n]u16) : [n]bool =
      if n == 0
      then replicate n true
      else replicate n true with [1:n] = map2 (!=) (tail is) (init is)

    def fine_rasterize [n]
                       {h = _: i64, w = w: i64}
                       (tris: []triangle)
                       ((bin_idxs, tile_idxs, tri_idxs): ([n]u16, [n]u8, [n]i64)) =
      let bins_w =
        assert (fine_size * fine_size == fine_mask.num_bits)
        ((w + bin_size - 1) >> bin_shift)
      let f (bin_index, tile_index, tri_index) =
        let bin_xmin = (i64.u16 bin_index %% bins_w) << bin_shift
        let bin_ymin = (i64.u16 bin_index / bins_w) << bin_shift
        let tile_xmin = ((i64.u8 tile_index & (coarse_size - 1)) << fine_shift) + bin_xmin
        let tile_ymin = ((i64.u8 tile_index >> coarse_shift) << fine_shift) + bin_ymin
        let (f0, f1, f2) = tris[tri_index]
        let verts = (f0.pos, f1.pos, f2.pos)
        let wzero = calc_wcoeffs verts {x = f32.i64 tile_xmin, y = f32.i64 tile_ymin}
        let wdelta = calc_wdelta verts
        let inv_area_2 = 1 / calc_signed_tri_area_2 verts
        let g pixel_index =
          let y = 0.5 + f32.i64 (pixel_index >> fine_shift)
          let x = 0.5 + f32.i64 (pixel_index & (fine_size - 1))
          let w = wzero vec3f.+ (x vec3f.* wdelta.x) vec3f.+ (y vec3f.* wdelta.y)
          in w.x >= 0 && w.y >= 0 && w.z >= 0
        let mask = fine_mask.from_pred_seq g
        in ({tile_xmin, tile_ymin}, {tri_index, wzero, wdelta, inv_area_2}, mask)
      let sz ((_, _, mask)) = fine_mask.size mask
      let get (({tile_xmin, tile_ymin}, {tri_index, wzero, wdelta, inv_area_2}, mask)) set_pixel_index =
        let pixel_index = fine_mask.find_ith_set_bit mask set_pixel_index
        let pixel_x = pixel_index & (fine_size - 1)
        let pixel_y = pixel_index >> fine_shift
        let x = pixel_x + tile_xmin
        let y = pixel_y + tile_ymin
        let pos = {x = 0.5 + f32.i64 x, y = 0.5 + f32.i64 y}
        let w =
          wzero
          vec3f.+ ((0.5 + f32.i64 pixel_x) vec3f.* wdelta.x)
          vec3f.+ ((0.5 + f32.i64 pixel_y) vec3f.* wdelta.y)
          |> (inv_area_2 vec3f.*)
          |> vec3f.to_tuple
        let (f0, f1, f2) = tris[tri_index]
        let Z_inv = barycentric f0.Z_inv f1.Z_inv f2.Z_inv w
        let depth = barycentric_pc Z_inv (f0.depth, f0.Z_inv) (f1.depth, f1.Z_inv) (f2.depth, f2.Z_inv) w
        let attr = barycentric_pc_attr Z_inv (f0.attr, f0.Z_inv) (f1.attr, f1.Z_inv) (f2.attr, f2.Z_inv) w
        in ((y, x), {pos, Z_inv, depth, attr}, depth)
      let arr = zip3 bin_idxs tile_idxs tri_idxs |> map f
      let szs = map sz arr
      let bin_flags = calc_segment_flags_u16 bin_idxs
      let (bin_idxs, bin_counts) =
        segmented_reduce (\(i0, s0) (i1, s1) -> (i0 `u16.min` i1, s0 + s1))
                         (u16.highest, 0)
                         bin_flags
                         (zip bin_idxs szs)
        |> unzip
      let (is, frag_values, depth_values) =
        repl_segm_iota szs
        |> uncurry (map2 (\i j -> get arr[i] j))
        |> unzip3
      in (bin_idxs, bin_counts, is, frag_values, depth_values)

    def coarse_rasterize [n]
                         {h = h: i64, w = w: i64}
                         (tris: []triangle)
                         ((bin_idxs, tri_idxs): ([n]u16, [n]i64)) =
      let fb_bbox = {xmin = 0, ymin = 0, xmax = w, ymax = h}
      let bins_w =
        assert (coarse_size * coarse_size == coarse_mask.num_bits
                && coarse_size * coarse_size - 1 <= i64.u8 u8.highest)
        ((w + bin_size - 1) >> bin_shift)
      let f (bin_index, tri_index) =
        let (f0, f1, f2) = tris[tri_index]
        let tri_bbox = calc_tri_bbox (f0, f1, f2)
        let bin_xmin = (i64.u16 bin_index %% bins_w) << bin_shift
        let bin_ymin = (i64.u16 bin_index / bins_w) << bin_shift
        let verts = (f0.pos, f1.pos, f2.pos)
        let wzero = calc_wcoeffs verts {x = 0, y = 0}
        let wdelta = calc_wdelta verts
        let f (tile_index: i64) =
          let tile_bbox =
            let xmin = ((tile_index & (coarse_size - 1)) << fine_shift) + bin_xmin
            let ymin = ((tile_index >> coarse_shift) << fine_shift) + bin_ymin
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
        in (bin_index, u8.i64 tile_index, tri_idxs[tri_index])
      let (bin_idxs, tile_idxs, tri_idxs) =
        zip bin_idxs tri_idxs
        |> map f
        |> zip (iota n)
        |> expand sz get
        |> unzip3
      in (bin_idxs, tile_idxs, tri_idxs)

    def bin_rasterize {h = h: i64, w = w: i64} (tris: []triangle) =
      let bins_w = (w + bin_size - 1) >> bin_shift
      let bins_h = (h + bin_size - 1) >> bin_shift
      let num_bits =
        assert (bins_h * bins_w - 1 <= i64.u16 u16.highest)
        ilog2_ceil (bins_h * bins_w)
      let f tri_index =
        let tri_bbox = calc_tri_bbox tris[tri_index]
        let bin_bbox =
          let xmin = (tri_bbox.xmin >> bin_shift) `i64.max` 0
          let ymin = (tri_bbox.ymin >> bin_shift) `i64.max` 0
          let xmax = ((tri_bbox.xmax + bin_size - 1) >> bin_shift) `i64.min` bins_w
          let ymax = ((tri_bbox.ymax + bin_size - 1) >> bin_shift) `i64.min` bins_h
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
        in (u16.i64 bin_index, tri_index)
      let (bin_idxs, tri_idxs) =
        indices tris
        |> map f
        |> expand sz get
        |> radix_sort_by_key (.0) (i32.i64 num_bits) u16.get_bit
        |> unzip
      in (bin_idxs, tri_idxs)

    def rasterize 'target [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_select: f32 -> f32 -> f32)
                  ((ne_target, ne_depth): (target, f32))
                  ((target_buffer, depth_buffer): ([h][w]target, [h][w]f32))
                  (tris: [](fragment V.t, fragment V.t, fragment V.t)) : ([h][w]target, [h][w]f32) =
      let bins_w = (w + bin_size - 1) >> bin_shift
      let tris = tris |> map ensure_cclockwise_winding_order
      let (bin_idxs, bin_counts, is, frag_values, depth_values) =
        bin_rasterize {h, w} tris
        |> coarse_rasterize {h, w} tris
        |> fine_rasterize {h, w} tris
      let bin_offsets = exscan (+) 0 bin_counts
      let bin_bufs =
        bin_idxs
        |> map (\bin_index ->
                  let bin_xmin = (i64.u16 bin_index %% bins_w) << bin_shift
                  let bin_ymin = (i64.u16 bin_index / bins_w) << bin_shift
                  let f pixel_index =
                    let pixel_x = pixel_index & (bin_size - 1)
                    let pixel_y = pixel_index >> bin_shift
                    let x = pixel_x + bin_xmin
                    let y = pixel_y + bin_ymin
                    in if x < w && y < h then depth_buffer[y, x] else ne_depth
                  in tabulate (bin_size * bin_size) f)
      let f i =
        let bin_index = bin_idxs[i]
        let bin_xmin = (i64.u16 bin_index %% bins_w) << bin_shift
        let bin_ymin = (i64.u16 bin_index / bins_w) << bin_shift
        let frag_count = bin_counts[i]
        let frag_offset = bin_offsets[i]
        let num_chunks = (frag_count + frag_block_size - 1) >> frag_block_shift
        let bin_width = bin_size
        in loop bin_buf = copy bin_bufs[i]
           for chunk_index < num_chunks do
             let g j =
               let frag_index = chunk_index * frag_block_size + j
               in if frag_index < frag_count
                  then let global_frag_index = frag_offset + frag_index
                       let (y, x) = is[global_frag_index]
                       let depth = depth_values[global_frag_index]
                       let (pixel_y, pixel_x) = (y - bin_ymin, x - bin_xmin)
                       in (pixel_y * bin_width + pixel_x, depth)
                  else (-1, ne_depth)
             let (is, depth_values) =
               tabulate frag_block_size g
               |> unzip2
             in reduce_by_index bin_buf depth_select ne_depth is depth_values
      let g i =
        let bin_index = bin_idxs[i >> (2 * bin_shift)]
        let bin_xmin = (i64.u16 bin_index %% bins_w) << bin_shift
        let bin_ymin = (i64.u16 bin_index / bins_w) << bin_shift
        let pixel_index = i & (bin_size * bin_size - 1)
        let pixel_y = pixel_index >> bin_shift
        let pixel_x = pixel_index & (bin_size - 1)
        let x = pixel_x + bin_xmin
        let y = pixel_y + bin_ymin
        in if x < w && y < h
           then y * w + x
           else -1
      let depth_buffer =
        #[incremental_flattening(only_intra)]
        tabulate (length bin_idxs) f
        |> flatten
        |> scatter (copy (flatten depth_buffer))
                   (tabulate (length bin_idxs * (bin_size * bin_size)) g)
        |> unflatten
      let (is, target_values) =
        zip is frag_values
        |> map (\((y, x), f) ->
                  if (0 <= x && x < w) && (0 <= y && y < h) && depth_buffer[y, x] == f.depth
                  then ((y, x), plot f)
                  else ((-1, -1), ne_target))
        |> unzip2
      let target_buffer = scatter_2d (copy target_buffer) is target_values
      in (target_buffer, depth_buffer)
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
