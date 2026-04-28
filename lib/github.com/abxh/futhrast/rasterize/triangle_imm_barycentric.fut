-- immediate-mode barycentric triangle rasterizer
-- assumes non-zero triangle area

import "../../../diku-dk/segmented/segmented"

import "../utils/bitmask"

import "../types"
import "../math/vec"
import "../math/fixedpoint"

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

-- | immediate-mode barycentric triangle rasterizer with binning
module ImmBarycentricTriangleRasterizer : TriangleRasterizerSpec = \(V: VaryingSpec) ->
  {
    local module V = VaryingExtensions V
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

    module coarse_mask = bitmask_64
    module fine_mask = bitmask_64

    local def bin_shift : i64 = 6
    local def fine_shift : i64 = 3
    local def coarse_shift : i64 = bin_shift - fine_shift

    local def bin_size : i64 = 1 << bin_shift
    local def fine_size : i64 = 1 << fine_shift
    local def coarse_size : i64 = 1 << coarse_shift

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
        let points_up = edge.y fixedpoint.> fixedpoint.zero
        let points_right = edge.x fixedpoint.> fixedpoint.zero
        let is_left_edge = points_up
        let is_top_edge = (fixedpoint.abs edge.y) fixedpoint.<= eps && points_right
        in if is_left_edge || is_top_edge then fixedpoint.zero else fixedpoint.neg eps

      def calc_tri_edge_bias ((v0, v1, v2): (vec2fp.t, vec2fp.t, vec2fp.t)) : vec3fp.t =
        {x = calc_edge_bias v1 v2, y = calc_edge_bias v2 v0, z = calc_edge_bias v0 v1}
    }

    open wcoeffs_fp

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

    def fine_rasterize [n]
                       {h = _: i64, w = w: i64}
                       (tris: []triangle)
                       ((tile_ids, tri_idxs): ([n]u32, [n]i64)) =
      let bins_w =
        assert (fine_size * fine_size == fine_mask.num_bits)
        ((w + bin_size - 1) >> bin_shift)
      let f (tile_id, tri_index) =
        let bin_index = i64.u32 tile_id >> (2 * coarse_shift)
        let tile_index = i64.u32 tile_id & (coarse_size * coarse_size - 1)
        let bin_xmin = (bin_index %% bins_w) << bin_shift
        let bin_ymin = (bin_index / bins_w) << bin_shift
        let tile_xmin = ((tile_index & (coarse_size - 1)) << fine_shift) + bin_xmin
        let tile_ymin = ((tile_index >> coarse_shift) << fine_shift) + bin_ymin
        let (f0, f1, f2) = tris[tri_index]
        let verts = (f0.pos, f1.pos, f2.pos)
        let inv_area_2 = 1 / calc_signed_tri_area_2 verts
        let verts_fp =
          ( vec2f.map fixedpoint.f32 f0.pos
          , vec2f.map fixedpoint.f32 f1.pos
          , vec2f.map fixedpoint.f32 f2.pos
          )
        let wzero = calc_wcoeffs_fp verts_fp {x = fixedpoint.i64 tile_xmin, y = fixedpoint.i64 tile_ymin}
        let wdelta = calc_wdelta_fp verts_fp
        let wbias = calc_tri_edge_bias verts_fp
        let g pixel_index =
          let y = (fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 (pixel_index >> fine_shift)
          let x = (fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 (pixel_index & (fine_size - 1))
          let w = wzero vec3fp.+ wbias vec3fp.+ (x vec3fp.* wdelta.x) vec3fp.+ (y vec3fp.* wdelta.y)
          in w.x fixedpoint.>= (fixedpoint.i64 0)
             && w.y fixedpoint.>= (fixedpoint.i64 0)
             && w.z fixedpoint.>= (fixedpoint.i64 0)
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
          vec3fp.+ (((fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 pixel_x) vec3fp.* wdelta.x)
          vec3fp.+ (((fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 pixel_y) vec3fp.* wdelta.y)
          |> vec3fp.map fixedpoint.to_f32
          |> (inv_area_2 vec3f.*)
          |> vec3f.to_tuple
        let (f0, f1, f2) = tris[tri_index]
        let Z_inv = barycentric f0.Z_inv f1.Z_inv f2.Z_inv w
        let depth = barycentric_pc Z_inv (f0.depth, f0.Z_inv) (f1.depth, f1.Z_inv) (f2.depth, f2.Z_inv) w
        let attr = barycentric_pc_attr Z_inv (f0.attr, f0.Z_inv) (f1.attr, f1.Z_inv) (f2.attr, f2.Z_inv) w
        in ((y, x), {pos, Z_inv, depth, attr}, depth)
      in zip tile_ids tri_idxs
         |> map f
         |> expand sz get
         |> unzip3

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
        let tile_id = (u32.u16 bin_index << u32.i64 (2 * coarse_shift)) + u32.i64 tile_index
        in (tile_id, tri_idxs[tri_index])
      in zip bin_idxs tri_idxs
         |> map f
         |> zip (iota n)
         |> expand sz get
         |> unzip

    def bin_rasterize {h = h: i64, w = w: i64} (tris: []triangle) =
      let (bins_h, bins_w) =
        let bins_h = (h + bin_size - 1) >> bin_shift
        let bins_w = (w + bin_size - 1) >> bin_shift
        in assert (bins_h * bins_w - 1 <= i64.u16 u16.highest) (bins_h, bins_w)
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
      in indices tris
         |> map f
         |> expand sz get
         |> unzip

    def rasterize 'target [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_select: f32 -> f32 -> f32)
                  ((ne_target, ne_depth): (target, f32))
                  ((target_buffer, depth_buffer): ([h][w]target, [h][w]f32))
                  (tris: [](fragment V.t, fragment V.t, fragment V.t)) : ([h][w]target, [h][w]f32) =
      let tris = tris |> map ensure_cclockwise_winding_order
      let (is, frag_values, depth_values) =
        bin_rasterize {h, w} tris
        |> coarse_rasterize {h, w} tris
        |> fine_rasterize {h, w} tris
      let depth_buffer = reduce_by_index_2d (copy depth_buffer) depth_select ne_depth is depth_values
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
