-- immediate-mode scanline triangle rasterizer

import "../../../diku-dk/segmented/segmented"

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

-- | immediate-mode scanline triangle rasterizer
module ImmTriangleRasterizer : TriangleRasterizerSpec = \(V: VaryingSpec) ->
  {
    -- based on:
    -- futhark-book.readthedocs.io/en/latest/irregular-flattening.html#drawing-triangles
    -- github.com/melsman/canvas
    -- sunshine2k.de/coding/java/TriangleRasterization/TriangleRasterization.html

    local module V = VaryingExtensions (V)

    local module F32 = VaryingExtensions (f32)

    local
    type triangle = (fragment V.t, fragment V.t, fragment V.t)

    def barycentric = F32.barycentric
    def barycentric_affine = F32.barycentric_perspective_corrected_w_Z_inv_t
    def barycentric_affine_attr = V.barycentric_perspective_corrected_w_Z_inv_t

    def calc_signed_tri_area_2 ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) : f32 =
      let v0v1 = v1 vec2f.- v0
      let v0v2 = v2 vec2f.- v0
      let signed_area_2 = v0v1 `vec2f.cross` v0v2
      in signed_area_2

    def ensure_cclockwise_winding_order ((f0, f1, f2): triangle) : triangle =
      if calc_signed_tri_area_2 (f0.pos, f1.pos, f2.pos) >= 0 then (f0, f1, f2) else (f0, f2, f1)

    def calc_barycentric_coeffs ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) (p: vec2f.t) : (f32, f32, f32) =
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
      let pos = {x = pl.x + i32.i64 i, y = pl.y}
      let (f0, f1, f2) = tris[tri_index]
      let (w0, w1, w2) = calc_barycentric_coeffs (f0.pos, f1.pos, f2.pos) (vec2i32.map (f32.i32) pos)
      -- workaround to fix rounding errors resulting in glitched pixels:
      let w0 = f32.max 0 (f32.min 1 w0)
      let w1 = f32.max 0 (f32.min 1 w1)
      let w2 = f32.max 0 (f32.min 1 w2)
      let w = (w0, w1, w2)
      let pos = {x = 0.5 + f32.i32 pos.x, y = 0.5 + f32.i32 pos.y}
      let Z_inv = barycentric f0.Z_inv f1.Z_inv f2.Z_inv w
      let depth = barycentric_affine Z_inv (f0.depth, f0.Z_inv) (f1.depth, f1.Z_inv) (f2.depth, f2.Z_inv) w
      let attr = barycentric_affine_attr Z_inv (f0.attr, f0.Z_inv) (f1.attr, f1.Z_inv) (f2.attr, f2.Z_inv) w
      in {pos, Z_inv, depth, attr}

    def sort_y_ascending ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) =
      -- v0.y <= v1.y <= v2.y
      let bubble a b = if a.y > b.y then (b, a) else (a, b)
      let (v0, v1) = bubble v0 v1
      let (v1, v2) = bubble v1 v2
      let (v0, v1) = bubble v0 v1
      in (v0, v1, v2)

    def dxdy (a: vec2i32.t) (b: vec2i32.t) : f32 =
      let dx = b.x - a.x
      let dy = b.y - a.y
      in if dy == 0 then 0 else f32.i32 dx / f32.i32 dy

    def num_lines_in_triangle ( ((f0, f1, f2): (fragment V.t, fragment V.t, fragment V.t))
                              , (_: i64)
                              ) : i64 =
      let (v0, v1, v2) = sort_y_ascending (f0.pos, f1.pos, f2.pos)
      let top = i64.f32 (v2.y + 0.5)
      let middle = i64.f32 (v1.y + 0.5)
      let bottom = i64.f32 (v0.y + 0.5)
      -- fullfill top-left edge rule by excluding bottom edge
      let offset = i64.bool (bottom != middle)
      in offset + top - bottom

    def get_line_in_triangle ( ((f0, f1, f2): (fragment V.t, fragment V.t, fragment V.t))
                             , (tri_index: i64)
                             )
                             (i: i64) =
      let (v0, v1, v2) = sort_y_ascending (f0.pos, f1.pos, f2.pos)
      let (v0, v1, v2) =
        ( vec2f.map i32.f32 (v0 vec2f.+ vec2f.replicate 0.5)
        , vec2f.map i32.f32 (v1 vec2f.+ vec2f.replicate 0.5)
        , vec2f.map i32.f32 (v2 vec2f.+ vec2f.replicate 0.5)
        )
      let y = v0.y + i32.i64 i
      in if y <= v1.y
         then -- upper half
              let sl0 = dxdy v0 v1
              let sl1 = dxdy v0 v2
              let dy = y - v0.y
              let p0 = {x = v0.x + i32.f32 (f32.round (sl0 * f32.i32 dy)), y}
              let p1 = {x = v0.x + i32.f32 (f32.round (sl1 * f32.i32 dy)), y}
              let (pl, pr) = if p0.x <= p1.x then (p0, p1) else (p1, p0)
              in ((pl, pr), tri_index)
         else -- lower half
              let sl0 = dxdy v1 v2
              let sl1 = dxdy v0 v2
              let dy = y - v1.y
              let p0 = {x = v1.x + i32.f32 (f32.round (sl0 * f32.i32 dy)), y}
              let p1 = {x = v0.x + i32.f32 (f32.round (sl1 * f32.i64 i)), y}
              let (pl, pr) = if p0.x <= p1.x then (p0, p1) else (p1, p0)
              in ((pl, pr), tri_index)

    def unpack_fragment (f: fragment V.t) =
      let y = i64.f32 f.pos.y
      let x = i64.f32 f.pos.x
      in ((y, x), f, f.depth)

    local
    def tri_bbox_check {w = w: i64, h = h: i64} (f0: fragment V.t, f1: fragment V.t, f2: fragment V.t) : bool =
      let (p0, p1, p2) = (f0.pos, f1.pos, f2.pos)
      let xmin = (p0.x `f32.min` p1.x `f32.min` p2.x) |> (f32.floor >-> i64.f32)
      let ymin = (p0.y `f32.min` p1.y `f32.min` p2.y) |> (f32.floor >-> i64.f32)
      let xmax = (p0.x `f32.max` p1.x `f32.max` p2.x) |> (f32.ceil >-> i64.f32)
      let ymax = (p0.y `f32.max` p1.y `f32.max` p2.y) |> (f32.ceil >-> i64.f32)
      in !(w <= xmin || 0 >= xmax || h <= ymin || 0 >= ymax)

    def rasterize 'target [n] [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_select: f32 -> f32 -> f32)
                  ((ne_target, ne_depth): (target, f32))
                  ((target_buffer, depth_buffer): ([h][w]target, [h][w]f32))
                  (tris: [n](fragment V.t, fragment V.t, fragment V.t)) : ([h][w]target, [h][w]f32) =
      -- note: filter prepass as workaround for large number of triangles offscreen to spare some framerate
      let tris =
        tris
        |> filter (tri_bbox_check {w, h})
        |> map ensure_cclockwise_winding_order
      let (is, frag_values, depth_values) =
        zip tris (indices tris)
        |> expand num_lines_in_triangle get_line_in_triangle
        |> expand get_horizontal_line_size (get_point_in_horizontal_line tris)
        |> map unpack_fragment
        |> unzip3
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

-- | immediate-mode scanline triangle rasterizer for testing purposes. can use the REPL for this
module ImmTriangleRasterizerTest = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = ImmTriangleRasterizer (V)

  def rasterize_triangle_imm_test [n] (h: i64) (w: i64) (vs: [n]((f32, f32), (f32, f32), (f32, f32))) : [h][w]i32 =
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

open ImmTriangleRasterizerTest
