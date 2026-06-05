-- immediate-mode scanline triangle rasterizer

import "../../../diku-dk/segmented/segmented"

import "../fragment"
import "../varying"
import "../math/vec"

-- | triangle rasterizer specfication
module type TriangleRasterizerSpec =
  (V: VaryingSpec)
  -> {
    val rasterize 'target [n] [h] [w] :
      (plot: fragment V.t -> target)
      -> ([h][w]target, [h][w]f32)
      -> [n](fragment V.t, fragment V.t, fragment V.t)
      -> ([h][w]target, [h][w]f32)
  }

-- | immediate-mode scanline triangle rasterizer
module ImmScanlineTriangleRasterizer : TriangleRasterizerSpec = \(V: VaryingSpec) ->
  {
    -- based on:
    -- futhark-book.readthedocs.io/en/latest/irregular-flattening.html#drawing-triangles
    -- github.com/melsman/canvas
    -- sunshine2k.de/coding/java/TriangleRasterization/TriangleRasterization.html

    local module Attr = VaryingExtensions V
    local module F32 = VaryingExtensions f32

    local
    type triangle = (fragment V.t, fragment V.t, fragment V.t)

    def highest_tri_count : i64 = (1 << 33) - 1
    def encode_depth d = f32.to_bits d
    def encode_depth_index d tri_index = (u64.u32 (encode_depth d) << 33) | (u64.i64 (tri_index + 1) & ((1 << 33) - 1))
    def decode_depth dvis = dvis >> 33 |> u32.u64 |> f32.from_bits
    def decode_index dvis = dvis & ((1 << 33) - 1) |> i64.u64 |> (i64.- 1)
    def ne_dvis = encode_depth_index 0 (-1)

    def calc_signed_tri_area_2 ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) : f32 =
      let v0v1 = v1 vec2f.- v0
      let v0v2 = v2 vec2f.- v0
      let signed_area_2 = v0v1 `vec2f.cross` v0v2
      in signed_area_2

    def ensure_cclockwise_winding_order ((f0, f1, f2): triangle) : triangle =
      if calc_signed_tri_area_2 (f0.pos, f1.pos, f2.pos) >= 0 then (f0, f1, f2) else (f0, f2, f1)

    def calc_barycentric_coeffs_normalized ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) (p: vec2f.t) : (f32, f32, f32) =
      let signed_area_2 = calc_signed_tri_area_2 (v0, v1, v2)
      let v1p = p vec2f.- v1
      let v2p = p vec2f.- v2
      let v1v2 = v2 vec2f.- v1
      let v2v0 = v0 vec2f.- v2
      let w0 = (v1v2 `vec2f.cross` v1p) / signed_area_2
      let w1 = (v2v0 `vec2f.cross` v2p) / signed_area_2
      let w2 = 1 - w0 - w1
      in (w0, w1, w2)

    def get_horizontal_line_size (((pl, pr), _): ((vec2i32.t, vec2i32.t), i64)) : i64 =
      -- exclusive range to fullfill top-left edge rule
      i64.i32 (pr.x - pl.x)

    def get_point_in_horizontal_line (tris: [](fragment V.t, fragment V.t, fragment V.t))
                                     (((pl, _), tri_index): ((vec2i32.t, vec2i32.t), i64))
                                     (i: i64) =
      let pos = {x = 0.5 + f32.i32 pl.x + f32.i64 i, y = 0.5 + f32.i32 pl.y}
      let (f0, f1, f2) = tris[tri_index]
      let (w0, w1, w2) = calc_barycentric_coeffs_normalized (f0.pos, f1.pos, f2.pos) pos
      -- workaround to fix rounding logic errors resulting in glitched pixels:
      let w0 = f32.max 0 (f32.min 1 w0)
      let w1 = f32.max 0 (f32.min 1 w1)
      let w2 = f32.max 0 (f32.min 1 w2)
      let w = (w0, w1, w2)
      let depth = F32.barycentric f0.depth f1.depth f2.depth w
      in ((i64.i32 pl.y, i64.i32 pl.x + i), encode_depth_index depth tri_index)

    def sort_y_ascending ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) =
      -- v0.y <= v1.y <= v2.y
      let bubble a b = if a.y > b.y then (b, a) else (a, b)
      let (v0, v1) = bubble v0 v1
      let (v1, v2) = bubble v1 v2
      let (v0, v1) = bubble v0 v1
      in (v0, v1, v2)

    def num_lines_in_triangle ( ((f0, f1, f2): (fragment V.t, fragment V.t, fragment V.t))
                              , (_: i64)
                              ) : i64 =
      let (v0, _, v2) = sort_y_ascending (f0.pos, f1.pos, f2.pos)
      let top = i64.f32 v2.y
      let bottom = i64.f32 v0.y
      in top - bottom

    def edge_intersect_x (a: vec2i32.t) (b: vec2i32.t) (y: i32) : i32 =
      let dy = b.y - a.y
      let dx = b.x - a.x
      in if dy == 0 then a.x else a.x + (dx * (y - a.y)) / dy

    def get_line_in_triangle ( ((f0, f1, f2): (fragment V.t, fragment V.t, fragment V.t))
                             , (tri_index: i64)
                             )
                             (i: i64) =
      let (v0, v1, v2) = (f0.pos, f1.pos, f2.pos)
      let (v0, v1, v2) = sort_y_ascending (v0, v1, v2)
      let (v0, v1, v2) = (vec2f.map i32.f32 v0, vec2f.map i32.f32 v1, vec2f.map i32.f32 v2)
      let y = v0.y + i32.i64 i
      in if y < v1.y
         then -- upper half
              let p0x = edge_intersect_x v0 v1 y
              let p1x = edge_intersect_x v0 v2 y
              let plx = i32.min p0x p1x
              let prx = i32.max p0x p1x
              in (({x = plx, y}, {x = prx, y}), tri_index)
         else -- lower half
              let p0x = edge_intersect_x v1 v2 y
              let p1x = edge_intersect_x v0 v2 y
              let plx = i32.min p0x p1x
              let prx = i32.max p0x p1x
              in (({x = plx, y}, {x = prx, y}), tri_index)

    def rasterize 'target [n] [h] [w]
                  (plot: (fragment V.t -> target))
                  ((target_buffer, depth_buffer): ([h][w]target, [h][w]f32))
                  (tris: [n](fragment V.t, fragment V.t, fragment V.t)) : ([h][w]target, [h][w]f32) =
      let ne_target = copy target_buffer[0, 0]
      let dvis_buffer = map (map (\v -> encode_depth_index v (-1))) depth_buffer
      let tris = assert (n < highest_tri_count) tris
      let tris = tris |> map ensure_cclockwise_winding_order
      let (is, xs) =
        zip tris (indices tris)
        |> expand num_lines_in_triangle get_line_in_triangle
        |> expand get_horizontal_line_size (get_point_in_horizontal_line tris)
        |> unzip
      let dvis_buffer = reduce_by_index_2d dvis_buffer u64.max ne_dvis is xs
      let (is, xs) =
        dvis_buffer
        |> flatten
        |> zip (iota (h * w))
        |> map (\(i, v) ->
                  let depth = decode_depth v
                  let tri_index = decode_index v
                  in if tri_index == -1
                     then (-1, (ne_target, 0))
                     else let x = i %% w
                          let y = i / w
                          let pos = {x = 0.5 + f32.i64 x, y = 0.5 + f32.i64 y}
                          let (f0, f1, f2) = tris[tri_index]
                          let (w0, w1, w2) = calc_barycentric_coeffs_normalized (f0.pos, f1.pos, f2.pos) pos
                          let w0 = f32.max 0 (f32.min 1 w0)
                          let w1 = f32.max 0 (f32.min 1 w1)
                          let w2 = f32.max 0 (f32.min 1 w2)
                          let W = (w0, w1, w2)
                          let Z_inv = F32.barycentric f0.Z_inv f1.Z_inv f2.Z_inv W
                          let attr = Attr.barycentric_pc_w_Zinv Z_inv (f0.attr, f0.Z_inv) (f1.attr, f1.Z_inv) (f2.attr, f2.Z_inv) W
                          in (y * w + x, (plot {pos, Z_inv, depth, attr}, depth)))
        |> unzip
      let dest = zip (flatten target_buffer) (flatten depth_buffer)
      let (target_buf, depth_buf) = scatter dest is xs |> unzip
      in (unflatten target_buf, unflatten depth_buf)
  }

local
-- | immediate-mode scanline triangle rasterizer for testing purposes. can use the REPL for this
module ImmScanlineTriangleRasterizerTest = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = ImmScanlineTriangleRasterizer (V)

  def test_triangle_imm_scanline [n] (h: i64) (w: i64) (vs: [n]((f32, f32), (f32, f32), (f32, f32))) : [h][w]i32 =
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

open ImmScanlineTriangleRasterizerTest
