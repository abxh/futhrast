-- tiled-hybrid triangle rasterizer
-- assumes non-zero triangle area

import "../../../diku-dk/segmented/segmented"
import "../../../diku-dk/sorts/radix_sort"

import "../utils/bitmask"
import "../utils/flatten2d"
import "../utils/encode_f32"

import "../fragment"
import "../varying"

import "../math/vec"
import "../math/fixedpoint"

-- | triangle rasterizer specfication
module type TriangleRasterizerSpec =
  (V: VaryingSpec)
  -> {
    -- | rasterize triangle given plot function, depth type,
    -- triangle fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: fragment V.t -> target)
      -> (depth_type: #normal_z | #reversed_z)
      -> (ne: (target, f32))
      -> ([h][w]target, [h][w]f32)
      -> [n](fragment V.t, fragment V.t, fragment V.t)
      -> ([h][w]target, [h][w]f32)
  }

module type HybridTriangleRasterizerOptions = {
  module coarse_mask: bitmask
  module small_triangle_mask: bitmask
  module bin_pattern: index_pattern
  module coarse_pattern: index_pattern

  val bin_shift : i64
  val fine_shift : i64
  val num_intrablocks_shift : i64
  val small_triangle_size_shift : i64
}

module HybridTriangleRasterizerDefaultOptions : HybridTriangleRasterizerOptions = {
  module coarse_mask = bitmask_64
  module small_triangle_mask = bitmask_64
  module bin_pattern = morton_u16_pattern
  module coarse_pattern = morton_u16_pattern

  def bin_shift : i64 = 7
  def fine_shift : i64 = 4
  def num_intrablocks_shift : i64 = 8
  def small_triangle_size_shift : i64 = 6
}

-- | tiled-hybrid triangle rasterizer
module HybridTriangleRasterizer (O: HybridTriangleRasterizerOptions) : TriangleRasterizerSpec = \(V: VaryingSpec) ->
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

    open O

    local def coarse_shift : i64 = bin_shift - fine_shift
    local def bin_size : i64 = 1 << bin_shift
    local def fine_size : i64 = 1 << fine_shift
    local def coarse_size : i64 = 1 << coarse_shift
    local def num_intrablocks : i64 = 1 << num_intrablocks_shift
    local def small_triangle_size : i64 = 1 << small_triangle_size_shift

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

    def is_pow2 (n: i64) : bool = n & (n - 1) == 0

    def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

    def ilog2_ceil (n: i64) : i64 = i64.bool (!is_pow2 n) + ilog2 n

    def round_up_pow2 (n: i64) : i64 = 1 << ilog2_ceil n

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
                         bin_index_setup
                         tile_index_setup
                         {h = h: i64, w = w: i64}
                         (tris: []triangle)
                         ((bin_idxs, tri_idxs): ([n]u16, [n]u32)) =
      let f (bin_index, tri_index) =
        let (f0, f1, f2) = tris[i64.u32 tri_index]
        let tri_bbox = calc_tri_bbox (f0, f1, f2)
        let (bin_y, bin_x) = bin_pattern.unflatten bin_index_setup (i64.u16 bin_index)
        let bin_xmin = bin_x << bin_shift
        let bin_ymin = bin_y << bin_shift
        let verts = (f0.pos, f1.pos, f2.pos)
        let wzero = calc_wcoeffs verts {x = 0, y = 0}
        let wdelta = calc_wdelta verts
        let f (tile_index: i64) =
          let tile_bbox =
            let (tile_y, tile_x) = coarse_pattern.unflatten tile_index_setup tile_index
            let xmin = (tile_x << fine_shift) + bin_xmin
            let ymin = (tile_y << fine_shift) + bin_ymin
            let xmax = xmin + fine_size
            let ymax = ymin + fine_size
            in {xmin, ymin, xmax, ymax}
          in if !bbox_overlaps tile_bbox {xmin = 0, ymin = 0, xmax = w, ymax = h}
             || !bbox_overlaps tile_bbox tri_bbox
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

    def bin_rasterize [n]
                      bin_index_setup
                      {h = h: i64, w = w: i64}
                      (tri_idxs: [n]u32)
                      (tris: [n]triangle) =
      let f tri_index =
        let tri_bbox = calc_tri_bbox tris[i64.u32 tri_index]
        let tri_bbox' =
          let xmin = (tri_bbox.xmin `i64.max` 0)
          let ymin = (tri_bbox.ymin `i64.max` 0)
          let xmax = (tri_bbox.xmax `i64.min` w)
          let ymax = (tri_bbox.ymax `i64.min` h)
          in {xmin, ymin, xmax, ymax}
        in (tri_index, tri_bbox')
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
        in (u16.i64 <| bin_pattern.flatten bin_index_setup (y, x), tri_index)
      let (small_partition, other_partition) =
        tri_idxs
        |> map f
        |> partition (\(_, bbox) ->
                        let bbox_h = bbox.ymax - bbox.ymin
                        let bbox_w = bbox.xmax - bbox.xmin
                        in (bbox_h `i64.max` 0) * (bbox_w `i64.max` 0) <= small_triangle_size)
      in ( small_partition
         , other_partition
           |> map g
           |> expand sz get
           |> unzip
         )

    def rasterize_small_triangles 'target [h] [w]
                                  (depth_type: #normal_z | #reversed_z)
                                  (ne_depth: f32)
                                  (dvis_buffer: *[h][w]u64)
                                  tri_infos
                                  (pairings: [](u32, bbox i64)) =
      let encode_depth d = (encode_f32 d) ^ (u32.bool (depth_type == #reversed_z) * u32.highest)
      let encode_depth_index d tri_index = (u64.u32 (encode_depth d) << 32) | u64.u32 tri_index
      let ne_dvis = encode_depth_index ne_depth u32.highest
      let f (tri_index, bbox: bbox i64) =
        let bbox_h = bbox.ymax - bbox.ymin
        let bbox_w = bbox.xmax - bbox.xmin
        let bbox_size = (bbox_h `i64.max` 0) * (bbox_w `i64.max` 0)
        let {tri = _, wzero, wdelta, wbias, inv_area_2 = _} = tri_infos[i64.u32 tri_index]
        let wdelta: {x: vec3fp.t, y: vec3fp.t} = wdelta
        let g bbox_index =
          let bbox_x = bbox_index %% bbox_w
          let bbox_y = bbox_index / bbox_w
          let x = (fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 (bbox_x + bbox.xmin)
          let y = (fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 (bbox_y + bbox.ymin)
          let w = wzero vec3fp.+ wbias vec3fp.+ (x vec3fp.* wdelta.x) vec3fp.+ (y vec3fp.* wdelta.y)
          in w.x fixedpoint.>= (fixedpoint.i64 0)
             && w.y fixedpoint.>= (fixedpoint.i64 0)
             && w.z fixedpoint.>= (fixedpoint.i64 0)
        let mask =
          loop b = small_triangle_mask.empty
          for pos in 0..<bbox_size do
            small_triangle_mask.set b pos (g pos)
        in (bbox, tri_index, mask)
      let sz ((_, _, mask)) = small_triangle_mask.size mask
      let get ((bbox, tri_index, mask)) set_bbox_index =
        let bbox_w = bbox.xmax - bbox.xmin
        let bbox_index = small_triangle_mask.find_ith_set_bit mask set_bbox_index
        let bbox_x = bbox_index %% bbox_w
        let bbox_y = bbox_index / bbox_w
        let x = bbox_x + bbox.xmin
        let y = bbox_y + bbox.ymin
        let {tri = (f0, f1, f2), wzero, wdelta, wbias = _, inv_area_2} = tri_infos[i64.u32 tri_index]
        let (f0, f1, f2): triangle = (f0, f1, f2)
        let w =
          wzero
          vec3fp.+ (((fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 x) vec3fp.* wdelta.x)
          vec3fp.+ (((fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 y) vec3fp.* wdelta.y)
          |> vec3fp.map fixedpoint.to_f32
          |> (inv_area_2 vec3f.*)
          |> vec3f.to_tuple
        let Z_inv = barycentric f0.Z_inv f1.Z_inv f2.Z_inv w
        let depth = barycentric_pc Z_inv (f0.depth, f0.Z_inv) (f1.depth, f1.Z_inv) (f2.depth, f2.Z_inv) w
        in ((y, x), encode_depth_index depth tri_index)
      let (is, xs) =
        pairings
        |> map f
        |> expand sz get
        |> unzip
      in reduce_by_index_2d dvis_buffer u64.min ne_dvis is xs

    def rasterize_tiled [n] 'target [h] [w]
                        bin_index_setup
                        tile_index_setup
                        (depth_type: #normal_z | #reversed_z)
                        (ne_depth: f32)
                        (dvis_buffer: [h][w]u64)
                        tri_infos
                        ((tile_ids, tri_idxs): ([n]u32, [n]u32)) =
      let encode_depth d = (encode_f32 d) ^ (u32.bool (depth_type == #reversed_z) * u32.highest)
      let encode_depth_index d tri_index = (u64.u32 (encode_depth d) << 32) | u64.u32 tri_index
      let ne_dvis = encode_depth_index ne_depth u32.highest
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
        let (bin_y, bin_x) = bin_pattern.unflatten bin_index_setup bin_index
        let bin_xmin = bin_x << bin_shift
        let bin_ymin = bin_y << bin_shift
        let (tile_y, tile_x) = coarse_pattern.unflatten tile_index_setup tile_index
        let tile_xmin = (tile_x << fine_shift) + bin_xmin
        let tile_ymin = (tile_y << fine_shift) + bin_ymin
        let g pixel_index =
          let pixel_x = pixel_index & (fine_size - 1)
          let pixel_y = pixel_index >> fine_shift
          let x = pixel_x + tile_xmin
          let y = pixel_y + tile_ymin
          in if x < w && y < h then dvis_buffer[y, x] else ne_dvis
        let tile_size = fine_size * fine_size
        in loop tile_buf = tabulate tile_size g
           for i < tri_count do
             let tri_index = tri_idxs[tri_offset + i]
             let {tri = (f0, f1, f2), wzero, wdelta, wbias, inv_area_2} = tri_infos[i64.u32 tri_index]
             let wdelta: {x: vec3fp.t, y: vec3fp.t} = wdelta
             let (f0, f1, f2): triangle = (f0, f1, f2)
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
                  then let w =
                         w vec3fp.- wbias
                         |> vec3fp.map fixedpoint.to_f32
                         |> (inv_area_2 vec3f.*)
                         |> vec3f.to_tuple
                       let Z_inv = barycentric f0.Z_inv f1.Z_inv f2.Z_inv w
                       let depth = barycentric_pc Z_inv (f0.depth, f0.Z_inv) (f1.depth, f1.Z_inv) (f2.depth, f2.Z_inv) w
                       in encode_depth_index depth tri_index
                  else ne_dvis
             in map2 u64.min (tabulate tile_size h) tile_buf
      let g i =
        let tile_id = unique_tile_ids[i >> (2 * fine_shift)]
        let pixel_index = i & (fine_size * fine_size - 1)
        let tile_index = i64.u32 (tile_id & u32.i64 (coarse_size * coarse_size - 1))
        let bin_index = i64.u32 (tile_id >> u32.i64 (2 * coarse_shift))
        let (bin_y, bin_x) = bin_pattern.unflatten bin_index_setup bin_index
        let bin_xmin = bin_x << bin_shift
        let bin_ymin = bin_y << bin_shift
        let (tile_y, tile_x) = coarse_pattern.unflatten tile_index_setup tile_index
        let tile_xmin = (tile_x << fine_shift) + bin_xmin
        let tile_ymin = (tile_y << fine_shift) + bin_ymin
        let pixel_y = pixel_index >> fine_shift
        let pixel_x = pixel_index & (fine_size - 1)
        let x = pixel_x + tile_xmin
        let y = pixel_y + tile_ymin
        in if x < w && y < h
           then (y, x)
           else (-1, -1)
      let k = length unique_tile_ids
      let num_phases = (k + num_intrablocks - 1) >> num_intrablocks_shift
      in loop dvis_buffer = copy dvis_buffer
         for phase_index < num_phases do
           let start = phase_index << num_intrablocks_shift
           let end = (phase_index + 1) << num_intrablocks_shift `i64.min` k
           let xs =
             #[incremental_flattening(only_intra)]
             iota (end - start)
             |> map (+ start)
             |> map f
             |> flatten
           let is =
             iota ((end - start) * (fine_size * fine_size))
             |> map (+ (start * (fine_size * fine_size)))
             |> map g
           in scatter_2d dvis_buffer is xs

    def rasterize 'target [h] [n] [w]
                  (plot: (fragment V.t -> target))
                  (depth_type: #normal_z | #reversed_z)
                  ((ne_target, ne_depth): (target, f32))
                  ((target_buffer, depth_buffer): ([h][w]target, [h][w]f32))
                  (tris: [n](fragment V.t, fragment V.t, fragment V.t)) : ([h][w]target, [h][w]f32) =
      let encode_depth d = (encode_f32 d) ^ (u32.bool (depth_type == #reversed_z) * u32.highest)
      let encode_depth_index d tri_index = (u64.u32 (encode_depth d) << 32) | u64.u32 tri_index
      let decode_depth d = decode_f32 (d ^ (u32.bool (depth_type == #reversed_z) * u32.highest))
      let dvis_buffer = map (map (\v -> encode_depth_index v u32.highest)) depth_buffer
      let tris = tris |> map ensure_cclockwise_winding_order
      let tri_idxs = assert (n <= i64.u32 u32.highest - 1) (map u32.i64 (indices tris))
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
      let bins_w = round_up_pow2 <| (w + bin_size - 1) >> bin_shift
      let bins_h = round_up_pow2 <| (h + bin_size - 1) >> bin_shift
      let (bins_h, bins_w) = assert (bins_h * bins_w - 1 <= i64.u16 u16.highest) (bins_h, bins_w)
      let coarse_size = assert (coarse_size * coarse_size == coarse_mask.num_bits) coarse_size
      let coarse_size = assert (coarse_size * coarse_size - 1 <= i64.u8 u8.highest) coarse_size
      let bin_index_setup = bin_pattern.setup {h = bins_h, w = bins_w}
      let tile_index_setup = coarse_pattern.setup {h = coarse_size, w = coarse_size}
      let total_tiles = bins_w * bins_h * (coarse_size * coarse_size)
      let num_bits_to_sort = ilog2_ceil total_tiles
      let (small_partition, other_partition) =
        bin_rasterize bin_index_setup
                      {h, w}
                      tri_idxs
                      tris
      let (tile_ids, tri_idxs) =
        other_partition
        |> coarse_rasterize bin_index_setup tile_index_setup {h, w} tris
        |> radix_sort_by_key (.0) (i32.i64 num_bits_to_sort) u32.get_bit
        |> unzip
      let dvis_buffer =
        rasterize_tiled bin_index_setup
                        tile_index_setup
                        depth_type
                        ne_depth
                        dvis_buffer
                        tri_infos
                        (tile_ids, tri_idxs)
      let dvis_buffer =
        rasterize_small_triangles depth_type
                                  ne_depth
                                  dvis_buffer
                                  tri_infos
                                  small_partition
      let (is, xs) =
        dvis_buffer
        |> flatten
        |> zip (iota (h * w))
        |> map (\(i, v) ->
                  let depth = decode_depth (u32.u64 (v >> 32))
                  let tri_index = u32.u64 v
                  in if tri_index == u32.highest
                     then (-1, (ne_target, ne_depth))
                     else let x = i %% w
                          let y = i / w
                          let pos = {x = 0.5 + f32.i64 x, y = 0.5 + f32.i64 y}
                          let {tri = (f0, f1, f2), wzero, wdelta, wbias = _, inv_area_2} = tri_infos[i64.u32 tri_index]
                          let W =
                            wzero
                            vec3fp.+ (((fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 x) vec3fp.* wdelta.x)
                            vec3fp.+ (((fixedpoint.f32 0.5) fixedpoint.+ fixedpoint.i64 y) vec3fp.* wdelta.y)
                            |> vec3fp.map fixedpoint.to_f32
                            |> (inv_area_2 vec3f.*)
                            |> vec3f.to_tuple
                          let Z_inv = barycentric f0.Z_inv f1.Z_inv f2.Z_inv W
                          let attr = barycentric_pc_attr Z_inv (f0.attr, f0.Z_inv) (f1.attr, f1.Z_inv) (f2.attr, f2.Z_inv) W
                          in (y * w + x, (plot {pos, Z_inv, depth, attr}, depth)))
        |> unzip
      let dest = zip (flatten target_buffer) (flatten depth_buffer)
      let (target_buf, depth_buf) = scatter dest is xs |> unzip
      in (unflatten target_buf, unflatten depth_buf)
  }

-- | triangle rasterizer for testing purposes. can use the REPL for this
module HybridTriangleRasterizerTest = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = HybridTriangleRasterizer HybridTriangleRasterizerDefaultOptions (V)

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
    in M.rasterize plot #reversed_z (false, -f32.inf) (target_buffer, depth_buffer) frags
       |> (.0)
       |> map (map i32.bool)
}

open HybridTriangleRasterizerTest
