-- tiled triangle rasterizer
-- assumes non-zero triangle area

open import "../../../abxh/expand_masked/bitmask"

import "../../../diku-dk/segmented/segmented"
import "../../../diku-dk/sorts/radix_sort"

import "../../../abxh/expand_masked/expand_masked"

import "../fragment"
import "../varying"
import "../math/vec"
import "../math/fixedpoint"

-- | triangle rasterizer specfication
module type TriangleRasterizerSpec =
  (V: VaryingSpec)
  -> {
    -- | rasterize triangle given plot function, triangle fragments, and the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: fragment V.t -> target)
      -> ([h][w]target, [h][w]f32)
      -> [n](fragment V.t, fragment V.t, fragment V.t)
      -> ([h][w]target, [h][w]f32)
  }

module type TiledPinedaTriangleRasterizerOptions = {
  module coarse_mask: bitmask

  val bin_shift : i64
  val fine_shift : i64
}

module TiledPinedaTriangleRasterizerDefaultOptions : TiledPinedaTriangleRasterizerOptions = {
  module coarse_mask = bitmask_16

  def bin_shift : i64 = 5
  def fine_shift : i64 = 3
}

-- | tiled triangle rasterizer
module CustomTiledPinedaTriangleRasterizer (O: TiledPinedaTriangleRasterizerOptions) : TriangleRasterizerSpec = \(V: VaryingSpec) ->
  {
    local module Attr = VaryingExtensions V
    local module F32 = VaryingExtensions f32

    local
    type triangle = (fragment V.t, fragment V.t, fragment V.t)

    local
    type bbox 'a =
      { xmin: a
      , xmax: a
      , ymin: a
      , ymax: a
      }

    open O

    local def coarse_shift : i64 = bin_shift - fine_shift
    local def bin_size : i64 = 1 << bin_shift
    local def fine_size : i64 = 1 << fine_shift
    local def coarse_size : i64 = 1 << coarse_shift

    local module expand_masked_coarse = expand_masked_generic coarse_mask
    local def expand_masked_coarse = expand_masked_coarse.expand_masked

    def highest_tri_count : i64 = (1 << 33) - 1
    def encode_depth d = f32.to_bits d
    def encode_depth_index d tri_index = (u64.u32 (encode_depth d) << 33) | (u64.i64 (tri_index + 1) & ((1 << 33) - 1))
    def decode_depth dvis = dvis >> 33 |> u32.u64 |> f32.from_bits
    def decode_index dvis = dvis & ((1 << 33) - 1) |> i64.u64 |> (i64.- 1)
    def ne_dvis = encode_depth_index 0 (-1)

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

    local
    module wcoeffs_fp = {
      local open vec2fp

      local def eps = fixedpoint.raw 1

      def calc_wcoeffs_fp ((v0, v1, v2): (vec2fp.t, vec2fp.t, vec2fp.t)) (p: vec2fp.t) : vec3fp.t =
        let v0p = p - v0
        let v1p = p - v1
        let v2p = p - v2
        let v1v2 = v2 - v1
        let v2v0 = v0 - v2
        let v0v1 = v1 - v0
        in (cross v1v2 v1p, cross v2v0 v2p, cross v0v1 v0p) |> vec3fp.from_tuple

      def calc_wdelta_fp ((v0, v1, v2): (vec2fp.t, vec2fp.t, vec2fp.t)) : {x: vec3fp.t, y: vec3fp.t} =
        let v1v2 = v2 - v1
        let v2v0 = v0 - v2
        let v0v1 = v1 - v0
        let delta_wx =
          ( fixedpoint.neg v1v2.y
          , fixedpoint.neg v2v0.y
          , fixedpoint.neg v0v1.y
          )
          |> vec3fp.from_tuple
        let delta_wy = (v1v2.x, v2v0.x, v0v1.x) |> vec3fp.from_tuple
        in {x = delta_wx, y = delta_wy}

      def calc_edge_bias (src: vec2fp.t)
                         (dest: vec2fp.t) : fixedpoint.t =
        let edge = (vec2fp.-) dest src
        let points_down = edge.y fixedpoint.< fixedpoint.zero
        let points_left = edge.x fixedpoint.> fixedpoint.zero
        let is_horizontal = (fixedpoint.abs edge.y) fixedpoint.<= eps
        let is_left_edge = points_down
        let is_top_edge = is_horizontal && points_left
        in if is_left_edge || is_top_edge then fixedpoint.zero else fixedpoint.neg eps

      def calc_tri_edge_bias ((v0, v1, v2): (vec2fp.t, vec2fp.t, vec2fp.t)) : vec3fp.t =
        {x = calc_edge_bias v1 v2, y = calc_edge_bias v2 v0, z = calc_edge_bias v0 v1}
    }

    open wcoeffs_fp

    def is_pow2 (n: i64) : bool = n & (n - 1) == 0

    def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

    def ilog2_ceil (n: i64) : i64 = i64.bool (!is_pow2 n) + ilog2 n

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
      let w1 = w0 vec3f.+ (f32.i64 (bbox.ymax - bbox.ymin) vec3f.* wdelta.y)
      let w2 = w0 vec3f.+ (f32.i64 (bbox.xmax - bbox.xmin) vec3f.* wdelta.x)
      let w3 = w2 vec3f.+ (f32.i64 (bbox.ymax - bbox.ymin) vec3f.* wdelta.y)
      let is_outside proj = proj w0 < 0 && proj w1 < 0 && proj w2 < 0 && proj w3 < 0
      in !(is_outside (.x) || is_outside (.y) || is_outside (.z))

    def calc_segment_flags [n] (is: [n]u32) : [n]bool =
      map (\i -> if i == 0 then true else is[i] != is[i - 1]) (iota n)

    def coarse_rasterize [n]
                         {h = _: i64, w = w: i64}
                         (tris: []triangle)
                         ((bin_idxs, tri_idxs): ([n]u16, [n]i64)) =
      let bins_w = ((w + bin_size - 1) >> bin_shift)
      let get (bin_index, tri_index) tile_index =
        let tile_id = (u32.u16 bin_index << u32.i64 (2 * coarse_shift)) + u32.i64 tile_index
        in (tile_id, tri_index)
      let pred (bin_index, tri_index) (tile_index: i64) =
        let bin_xmin = (i64.u16 bin_index %% bins_w) << bin_shift
        let bin_ymin = (i64.u16 bin_index / bins_w) << bin_shift
        let (f0, f1, f2) = tris[tri_index]
        let verts = (f0.pos, f1.pos, f2.pos)
        let wzero = calc_wcoeffs verts {x = 0, y = 0}
        let wdelta = calc_wdelta verts
        let tile_bbox =
          let xmin = ((tile_index & (coarse_size - 1)) << fine_shift) + bin_xmin
          let ymin = ((tile_index >> coarse_shift) << fine_shift) + bin_ymin
          let xmax = xmin + fine_size
          let ymax = ymin + fine_size
          in {xmin, ymin, xmax, ymax}
        in tri_overlaps_bbox tile_bbox wzero wdelta
      in zip bin_idxs tri_idxs
         |> expand_masked_coarse (\(_, _) -> coarse_mask.num_bits) get pred
         |> unzip

    def bin_rasterize [n]
                      {h = _: i64, w = w: i64}
                      (tris: [n]triangle) =
      let bins_w = (w + bin_size - 1) >> bin_shift
      let f tri_index = (tri_index, calc_tri_bbox tris[tri_index])
      let g (tri_index, tri_bbox) =
        let bin_bbox =
          let xmin = tri_bbox.xmin >> bin_shift
          let ymin = tri_bbox.ymin >> bin_shift
          let xmax = (tri_bbox.xmax + bin_size - 1) >> bin_shift
          let ymax = (tri_bbox.ymax + bin_size - 1) >> bin_shift
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
        in (u16.i64 <| y * bins_w + x, tri_index)
      in indices tris
         |> map (f >-> g)
         |> expand sz get
         |> unzip

    def rasterize_tiled [n] 'target [h] [w]
                        (plot: (fragment V.t -> target))
                        ((target_buffer, depth_buffer): (*[h][w]target, *[h][w]f32))
                        (tris: []triangle)
                        ((tile_ids, tri_idxs): ([n]u32, [n]i64)) =
      let ne_target = copy target_buffer[0, 0]
      let dvis_buffer = map (map (\v -> encode_depth_index v (-1))) depth_buffer
      let bins_w = ((w + bin_size - 1) >> bin_shift)
      let tri_infos =
        tris
        |> map (\(f0, f1, f2) ->
                  let verts = (f0.pos, f1.pos, f2.pos)
                  let inv_area_2 = 1 / calc_signed_tri_area_2 verts
                  let verts_fp =
                    ( vec2f.map fixedpoint.f32 f0.pos
                    , vec2f.map fixedpoint.f32 f1.pos
                    , vec2f.map fixedpoint.f32 f2.pos
                    )
                  let wzero = calc_wcoeffs_fp verts_fp {x = fixedpoint.i64 0, y = fixedpoint.i64 0}
                  let wdelta = calc_wdelta_fp verts_fp
                  let wbias = calc_tri_edge_bias verts_fp
                  in {tri = (f0, f1, f2), wzero, wdelta, wbias, inv_area_2})
      let tile_flags = calc_segment_flags tile_ids
      let (unique_tile_ids, unique_tile_counts) =
        segmented_reduce (\(i0, s0) (i1, s1) -> (i0 `u32.min` i1, s0 + s1))
                         (u32.highest, 0)
                         tile_flags
                         (zip tile_ids (replicate n 1))
        |> unzip
      let unique_tile_offsets = exscan (+) 0 unique_tile_counts
      let f segment_index =
        let tile_id = unique_tile_ids[segment_index]
        let tri_count = unique_tile_counts[segment_index]
        let tri_offset = unique_tile_offsets[segment_index]
        let bin_index = i64.u32 (tile_id >> u32.i64 (2 * coarse_shift))
        let tile_index = i64.u32 (tile_id & u32.i64 (coarse_size * coarse_size - 1))
        let bin_xmin = (bin_index %% bins_w) << bin_shift
        let bin_ymin = (bin_index / bins_w) << bin_shift
        let tile_xmin = ((tile_index & (coarse_size - 1)) << fine_shift) + bin_xmin
        let tile_ymin = ((tile_index >> coarse_shift) << fine_shift) + bin_ymin
        let g pixel_index =
          let pixel_x = pixel_index & (fine_size - 1)
          let pixel_y = pixel_index >> fine_shift
          let x = pixel_x + tile_xmin
          let y = pixel_y + tile_ymin
          in if x < w && y < h then dvis_buffer[y, x] else ne_dvis
        let tile_size = fine_size * fine_size
        let dvis_tile =
          loop tile_buf = tabulate tile_size g
          for i < tri_count do
            let tri_index = tri_idxs[tri_offset + i]
            let {tri = (f0, f1, f2), wzero, wdelta, wbias, inv_area_2} = tri_infos[tri_index]
            let h j =
              let pixel_x = j & (fine_size - 1)
              let pixel_y = j >> fine_shift
              let x = (fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 (pixel_x + tile_xmin)
              let y = (fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 (pixel_y + tile_ymin)
              let w =
                wzero vec3fp.+ (x vec3fp.* wdelta.x) vec3fp.+ (y vec3fp.* wdelta.y)
                vec3fp.+ wbias
              in if ((w.x fixedpoint.>= fixedpoint.i64 0)
                     && (w.y fixedpoint.>= fixedpoint.i64 0)
                     && (w.z fixedpoint.>= fixedpoint.i64 0))
                 then let w = {x = w.x, y = w.y}
                      let wbias = {x = wbias.x, y = wbias.y}
                      let (w0, w1) =
                        w vec2fp.- wbias
                        |> vec2fp.map fixedpoint.to_f32
                        |> (inv_area_2 vec2f.*)
                        |> vec2f.to_tuple
                      let w = (w0, w1, 1 - w0 - w1)
                      let depth = F32.barycentric f0.depth f1.depth f2.depth w
                      in encode_depth_index depth tri_index
                 else ne_dvis
            in map2 u64.max (tabulate tile_size h) tile_buf
        let g pixel_index =
          let pixel_x = pixel_index & (fine_size - 1)
          let pixel_y = pixel_index >> fine_shift
          let x = pixel_x + tile_xmin
          let y = pixel_y + tile_ymin
          in if x < w && y < h then (target_buffer[y, x], depth_buffer[y, x]) else (ne_target, 0)
        let h j =
          let v = dvis_tile[j]
          let depth = decode_depth v
          let tri_index = decode_index v
          in if tri_index == -1
             then (-1, (ne_target, 0))
             else let pixel_x = j & (fine_size - 1)
                  let pixel_y = j >> fine_shift
                  let x = pixel_x + tile_xmin
                  let y = pixel_y + tile_ymin
                  let pos = {x = 0.5 + f32.i64 x, y = 0.5 + f32.i64 y}
                  let {tri = (f0, f1, f2), wzero, wdelta, wbias = _, inv_area_2} = tri_infos[tri_index]
                  let wzero = {x = wzero.x, y = wzero.y}
                  let wdelta_x = {x = wdelta.x.x, y = wdelta.x.y}
                  let wdelta_y = {x = wdelta.y.x, y = wdelta.y.y}
                  let (w0, w1) =
                    wzero
                    vec2fp.+ (((fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 x) vec2fp.* wdelta_x)
                    vec2fp.+ (((fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 y) vec2fp.* wdelta_y)
                    |> vec2fp.map fixedpoint.to_f32
                    |> (inv_area_2 vec2f.*)
                    |> vec2f.to_tuple
                  let W = (w0, w1, 1 - w0 - w1)
                  let Z_inv = F32.barycentric f0.Z_inv f1.Z_inv f2.Z_inv W
                  let attr = Attr.barycentric_pc_w_Zinv Z_inv (f0.attr, f0.Z_inv) (f1.attr, f1.Z_inv) (f2.attr, f2.Z_inv) W
                  in (pixel_y * fine_size + pixel_x, (plot {pos, Z_inv, depth, attr}, depth))
        let (is, xs) = tabulate tile_size h |> unzip
        in scatter (tabulate tile_size g) is xs
      let g i =
        let tile_id = unique_tile_ids[i >> (2 * fine_shift)]
        let pixel_index = i & (fine_size * fine_size - 1)
        let tile_index = i64.u32 (tile_id & u32.i64 (coarse_size * coarse_size - 1))
        let bin_index = i64.u32 (tile_id >> u32.i64 (2 * coarse_shift))
        let bin_xmin = (bin_index %% bins_w) << bin_shift
        let bin_ymin = (bin_index / bins_w) << bin_shift
        let tile_xmin = ((tile_index & (coarse_size - 1)) << fine_shift) + bin_xmin
        let tile_ymin = ((tile_index >> coarse_shift) << fine_shift) + bin_ymin
        let pixel_y = pixel_index >> fine_shift
        let pixel_x = pixel_index & (fine_size - 1)
        let x = pixel_x + tile_xmin
        let y = pixel_y + tile_ymin
        in if x < w && y < h
           then y * w + x
           else -1
      let k = length unique_tile_ids
      let dest =
        let xs =
          #[incremental_flattening(only_intra)]
          tabulate k f |> flatten
        let is = tabulate (k * (fine_size * fine_size)) g
        in scatter (copy (zip (flatten target_buffer) (flatten depth_buffer))) is xs
      let (target_buf, depth_buf) = dest |> unzip
      in (unflatten target_buf, unflatten depth_buf)

    def rasterize 'target [h] [n] [w]
                  (plot: (fragment V.t -> target))
                  ((target_buffer, depth_buffer): ([h][w]target, [h][w]f32))
                  (tris: [n](fragment V.t, fragment V.t, fragment V.t)) : ([h][w]target, [h][w]f32) =
      let (target_buffer, depth_buffer) = (copy target_buffer, copy depth_buffer)
      let tris = assert (n < highest_tri_count) tris
      let tris = tris |> map ensure_cclockwise_winding_order
      let bins_w = (w + bin_size - 1) >> bin_shift
      let bins_h = (h + bin_size - 1) >> bin_shift
      let (bins_h, bins_w) = assert (bins_h * bins_w - 1 <= i64.u16 u16.highest) (bins_h, bins_w)
      let coarse_size = assert (coarse_size * coarse_size == coarse_mask.num_bits) coarse_size
      let coarse_size = assert (coarse_size * coarse_size - 1 <= i64.u8 u8.highest) coarse_size
      let total_tiles = bins_w * bins_h * (coarse_size * coarse_size)
      let num_bits_to_sort = ilog2_ceil total_tiles
      let (tile_ids, tri_idxs) =
        bin_rasterize {h, w} tris
        |> coarse_rasterize {h, w} tris
        |> uncurry zip
        |> radix_sort_by_key (.0) (i32.i64 num_bits_to_sort) u32.get_bit
        |> unzip
      let (target_buffer, depth_buffer) =
        rasterize_tiled plot
                        (target_buffer, depth_buffer)
                        tris
                        (tile_ids, tri_idxs)
      in (target_buffer, depth_buffer)
  }

module TiledPinedaTriangleRasterizer = CustomTiledPinedaTriangleRasterizer TiledPinedaTriangleRasterizerDefaultOptions

-- | triangle rasterizer for testing purposes. can use the REPL for this
module TiledPinedaTriangleRasterizerTest = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = TiledPinedaTriangleRasterizer V

  def test_triangle_tiled_pineda [n] (h: i64) (w: i64) (vs: [n]((f32, f32), (f32, f32), (f32, f32))) : [h][w]i32 =
    let target_buffer = replicate h (replicate w false)
    let depth_buffer = replicate h (replicate w 0)
    let frags =
      vs
      |> map (\(f0, f1, f2) ->
                ( {pos = {x = f0.0, y = f0.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f1.0, y = f1.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f2.0, y = f2.1}, depth = 1, Z_inv = 1, attr = true}
                ))
    let plot = (\(f: fragment bool) -> f.attr)
    in M.rasterize plot (target_buffer, depth_buffer) frags
       |> (.0)
       |> map (map i32.bool)
}

open TiledPinedaTriangleRasterizerTest
