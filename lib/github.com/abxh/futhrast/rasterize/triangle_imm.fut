--  immediate-mode triangle rasterizer

import "../../../diku-dk/segmented/segmented"

import "../types"
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

-- | immediate triangle rasterizer
module ImmTriangleRasterizer : TriangleRasterizerSpec = \(V: VaryingSpec) ->
  {
    -- based on:
    -- futhark-book.readthedocs.io/en/latest/irregular-flattening.html#drawing-triangles
    -- github.com/melsman/canvas
    -- sunshine2k.de/coding/java/TriangleRasterization/TriangleRasterization.html

    local module V = VaryingExtensions (V)

    local module F32 = VaryingExtensions (f32)

    type fragment_generic 'a 'varying =
      {pos: {x: a, y: a}, depth: f32, Z_inv: f32, attr: varying}

    local
    type triangle = (fragment_generic i32 V.t, fragment_generic i32 V.t, fragment_generic i32 V.t)

    def barycentric = F32.barycentric
    def barycentric_affine = F32.barycentric_perspective_corrected_w_Z_inv_t
    def barycentric_affine_attr = V.barycentric_perspective_corrected_w_Z_inv_t

    def round_fragment (f: fragment_generic f32 V.t) : fragment_generic i32 V.t =
      { pos = {x = i32.f32 (f.pos.x + 0.5), y = i32.f32 (f.pos.y + 0.5)}
      , depth = f.depth
      , Z_inv = f.Z_inv
      , attr = f.attr
      }

    def calc_signed_tri_area_2 ((v0, v1, v2): (vec2i32.t, vec2i32.t, vec2i32.t)) : i32 =
      let v0v1 = v1 vec2i32.- v0
      let v0v2 = v2 vec2i32.- v0
      let signed_area_2 = v0v1 `vec2i32.cross` v0v2
      in signed_area_2

    def ensure_cclockwise_winding_order ((f0, f1, f2): triangle) : triangle =
      if calc_signed_tri_area_2 (f0.pos, f1.pos, f2.pos) >= 0 then (f0, f1, f2) else (f0, f2, f1)

    def calc_barycentric_coeffs ((v0, v1, v2): (vec2i32.t, vec2i32.t, vec2i32.t)) (p: vec2i32.t) : (f32, f32, f32) =
      let signed_area_2 = calc_signed_tri_area_2 (v0, v1, v2)
      let v1p = p vec2i32.- v1
      let v2p = p vec2i32.- v2
      let v1v2 = v2 vec2i32.- v1
      let v2v0 = v0 vec2i32.- v2
      let w0 = (f32.i32 (v1v2 `vec2i32.cross` v1p) / f32.i32 signed_area_2)
      let w1 = f32.i32 (v2v0 `vec2i32.cross` v2p) / f32.i32 signed_area_2
      let w2 = 1 - w0 - w1
      in (w0, w1, w2)

    def plot_fragment 'target (plot: (fragment V.t -> target)) (f: fragment_generic i32 V.t) =
      let f' =
        { pos = {x = f32.i32 f.pos.x, y = f32.i32 f.pos.y}
        , depth = f.depth
        , Z_inv = f.Z_inv
        , attr = f.attr
        }
      in ((i64.i32 f.pos.y, i64.i32 f.pos.x), plot f', f.depth)

    def get_horizontal_line_size (((pl, pr), _): ((vec2i32.t, vec2i32.t), triangle)) : i64 =
      -- exclusive range to fullfill top-left edge rule
      i64.i32 (pr.x - pl.x)

    def get_point_in_horizontal_line (((pl, _), (f0, f1, f2)): ((vec2i32.t, vec2i32.t), triangle))
                                     (i: i64) : fragment_generic i32 V.t =
      let pos = {x = pl.x + i32.i64 i, y = pl.y}
      let (w0, w1, w2) = calc_barycentric_coeffs (f0.pos, f1.pos, f2.pos) pos
      -- workaround to fix rounding errors resulting in glitched pixels:
      let w0 = f32.max 0 (f32.min 1 w0)
      let w1 = f32.max 0 (f32.min 1 w1)
      let w2 = f32.max 0 (f32.min 1 w2)
      let w = (w0, w1, w2)
      let Z_inv = barycentric f0.Z_inv f1.Z_inv f2.Z_inv w
      let depth = barycentric_affine Z_inv (f0.depth, f0.Z_inv) (f1.depth, f1.Z_inv) (f2.depth, f2.Z_inv) w
      let attr = barycentric_affine_attr Z_inv (f0.attr, f0.Z_inv) (f1.attr, f1.Z_inv) (f2.attr, f2.Z_inv) w
      in {pos, Z_inv, depth, attr}

    def sort_y_ascending ((v0, v1, v2): (vec2i32.t, vec2i32.t, vec2i32.t)) =
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

    def num_lines_in_triangle ((f0, f1, f2): triangle) : i64 =
      let (v0, v1, v2) = sort_y_ascending (f0.pos, f1.pos, f2.pos)
      let top = v2.y
      let bottom = v0.y
      -- fullfill top-left edge rule by excluding bottom edge
      let offset = if v0.y == v1.y then 0 else 1
      in offset + i64.i32 (top - bottom)

    def get_line_in_triangle ((f0, f1, f2): triangle) (i: i64) =
      let (v0, v1, v2) = sort_y_ascending (f0.pos, f1.pos, f2.pos)
      let y = v0.y + i32.i64 i
      in if y <= v1.y
         then -- upper half
              let sl0 = dxdy v0 v1
              let sl1 = dxdy v0 v2
              let dy = y - v0.y
              let p0 = {x = v0.x + i32.f32 (f32.round (sl0 * f32.i32 dy)), y}
              let p1 = {x = v0.x + i32.f32 (f32.round (sl1 * f32.i32 dy)), y}
              let (pl, pr) = if p0.x <= p1.x then (p0, p1) else (p1, p0)
              in ((pl, pr), (f0, f1, f2))
         else -- lower half
              let sl0 = dxdy v1 v2
              let sl1 = dxdy v0 v2
              let dy = y - v1.y
              let p0 = {x = v1.x + i32.f32 (f32.round (sl0 * f32.i32 dy)), y}
              let p1 = {x = v0.x + i32.f32 (f32.round (sl1 * f32.i64 i)), y}
              let (pl, pr) = if p0.x <= p1.x then (p0, p1) else (p1, p0)
              in ((pl, pr), (f0, f1, f2))

    def rasterize 'target [n] [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_cmp: f32 -> f32 -> #left | #right)
                  (ne: (target, f32))
                  (dest: [h][w](target, f32))
                  (frags: [n](fragment V.t, fragment V.t, fragment V.t)) : [h][w](target, f32) =
      let (is, target_values, depth_values) =
        frags
        |> map (\tri -> (round_fragment tri.0, round_fragment tri.1, round_fragment tri.2))
        |> map (ensure_cclockwise_winding_order)
        |> expand num_lines_in_triangle get_line_in_triangle
        |> expand get_horizontal_line_size get_point_in_horizontal_line
        |> map (plot_fragment plot)
        |> unzip3
      let as = zip target_values depth_values
      let cmp = (\f0 f1 -> match depth_cmp f0.1 f1.1 case #left -> f0 case #right -> f1)
      in reduce_by_index_2d (copy dest) cmp ne is as
  }

-- | immediate triangle rasterizer for testing purposes. can use the REPL for this
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
    let dest = replicate h (replicate w (false, -f32.inf))
    let in_fb v = 0 <= v.0 && v.0 < f32.i64 w && 0 <= v.1 && v.1 < f32.i64 h
    let frags =
      vs
      |> filter (\(v0, v1, v2) -> in_fb v0 && in_fb v1 && in_fb v2)
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

open ImmTriangleRasterizerTest
