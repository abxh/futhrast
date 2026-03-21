--  immediate-mode triangle rasterizer

import "../../../diku-dk/segmented/segmented"

import "../fragment"
import "../varying"
import "../math/vec"

local
-- | triangle rasterizer specfication
module type mk_triangle_rasterizer_spec =
  (V: VaryingSpec)
  -> {
    -- | rasterize triangle given plot function, depth comparision function,
    -- triangle fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: pfragment V.t -> target)
      -> (depth_cmp: f32 -> f32 -> #left | #right)
      -> [n](pfragment V.t, pfragment V.t, pfragment V.t)
      -> (ne: (target, f32))
      -> ([h][w]target, [h][w]f32)
      -> ([h][w]target, [h][w]f32)
  }

-- | immediate triangle rasterizer
module mk_imm_triangle_rasterizer : mk_triangle_rasterizer_spec = \(V: VaryingSpec) ->
  {
    -- based on:
    -- futhark-book.readthedocs.io/en/latest/irregular-flattening.html#drawing-triangles
    -- github.com/melsman/canvas
    -- sunshine2k.de/coding/java/TriangleRasterization/TriangleRasterization.html

    local module V = VaryingExtensions (V)

    local module F32 = VaryingExtensions (f32)

    local
    type triangle 'a =
      ( pfragment_generic a V.t
      , pfragment_generic a V.t
      , pfragment_generic a V.t
      )

    def barycentric = F32.barycentric
    def barycentric_affine = F32.barycentric_perspective_corrected_w_Z_inv_t
    def barycentric_attr = V.barycentric_perspective_corrected_w_Z_inv_t

    def calc_signed_tri_area_2 ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) : f32 =
      let v0v1 = v1 vec2f.- v0
      let v0v2 = v2 vec2f.- v0
      let signed_area_2 = v0v1 `vec2f.cross` v0v2
      in signed_area_2

    def plot_fragment 'target
                      (plot: (pfragment V.t -> target))
                      (pos: vec2f.t)
                      (w: vec2f.t)
                      ((f0, f1, f2): (pfragment V.t, pfragment V.t, pfragment V.t)) =
      let signed_area_2 = calc_signed_tri_area_2 (f0.pos, f1.pos, f2.pos)
      let (f1, f2) = if signed_area_2 >= 0 then (f1, f2) else (f2, f1)
      let area_2 = f32.abs signed_area_2
      let (w0, w1) = (w.x / area_2, w.y / area_2)
      let w = (w0, w1, 1 - w0 - w1)
      let Z_inv = barycentric f0.Z_inv f1.Z_inv f2.Z_inv w
      let depth = barycentric_affine Z_inv (f0.depth, f0.Z_inv) (f1.depth, f1.Z_inv) (f2.depth, f2.Z_inv) w
      let attr = barycentric_attr Z_inv (f0.attr, f0.Z_inv) (f1.attr, f1.Z_inv) (f2.attr, f2.Z_inv) w
      in ((i64.f32 (pos.y + 0.5), i64.f32 (pos.x + 0.5)), plot {pos, Z_inv, depth, attr}, depth)

    def get_horizontal_line_size (((pl, pr), _, _): ((vec2f.t, vec2f.t), (vec2f.t, vec2f.t), i64)) =
      -- assuming endpoints are sorted:
      let xl = f32.trunc (pl.x + 0.5)
      let xr = f32.trunc (pr.x + 0.5)
      in 1 + i64.f32 (xr - xl)

    def get_point_in_horizontal_line (((pl, _), (w0, wdelta), tri_index): ((vec2f.t, vec2f.t), (vec2f.t, vec2f.t), i64))
                                     (i: i64) : (vec2f.t, vec2f.t, i64) =
      let pos = {x = pl.x + f32.i64 i, y = pl.y}
      let w = w0 vec2f.+ f32.i64 i vec2f.* wdelta
      in (pos, w, tri_index)

    def calc_barycentric_coeffs ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) (p: vec2f.t) : vec2f.t =
      let signed_area_2 = calc_signed_tri_area_2 (v0, v1, v2)
      let (v1, v2) = if signed_area_2 >= 0 then (v1, v2) else (v2, v1)
      let v1p = p vec2f.- v1
      let v2p = p vec2f.- v2
      let v1v2 = v2 vec2f.- v1
      let v2v0 = v0 vec2f.- v2
      in {x = v1v2 `vec2f.cross` v1p, y = v2v0 `vec2f.cross` v2p}

    def calc_barycentric_delta_x ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) : vec2f.t =
      let signed_area_2 = calc_signed_tri_area_2 (v0, v1, v2)
      let (v1, v2) = if signed_area_2 >= 0 then (v1, v2) else (v2, v1)
      let v1v2 = v2 vec2f.- v1
      let v2v0 = v0 vec2f.- v2
      let delta_wx = (f32.neg v1v2.y, f32.neg v2v0.y) |> vec2f.from_tuple
      in delta_wx

    def sort_y_ascending ((v0, v1, v2): triangle f32) =
      let bubble a b = if a.pos.y > b.pos.y then (b, a) else (a, b)
      let (v0, v1) = bubble v0 v1
      let (v1, v2) = bubble v1 v2
      let (v0, v1) = bubble v0 v1
      in (v0, v1, v2)

    def dxdy (a: vec2f.t) (b: vec2f.t) : f32 =
      let dx = b.x - a.x
      let dy = b.y - a.y
      in if dy == 0
         then 0
         else dx / dy

    def num_lines_in_triangle (((bottom, _, top), _): (triangle f32, i64)) : i64 =
      1 + i64.f32 (top.pos.y - bottom.pos.y)

    def get_line_in_upper_half_triangle (((f0, f1, f2), tri_index): (triangle f32, i64)) (i: i64) =
      let (v0, v1, v2) = (f0.pos, f1.pos, f2.pos)
      let sl0 = dxdy v0 v1
      let sl1 = dxdy v0 v2
      let i = f32.i64 i
      let y = v0.y + i
      let p0 = {x = v0.x + sl0 * i, y}
      let p1 = {x = v0.x + sl1 * i, y}
      let (pl, pr) = if p0.x <= p1.x then (p0, p1) else (p1, p0)
      let wl = calc_barycentric_coeffs (f0.pos, f1.pos, f2.pos) pl
      let wdelta_x = calc_barycentric_delta_x (f0.pos, f1.pos, f2.pos)
      in ((pl, pr), (wl, wdelta_x), tri_index)

    def get_line_in_lower_half_triangle (((f0, f1, f2), tri_index): (triangle f32, i64)) (i: i64) =
      let (v0, v1, v2) = (f0.pos, f1.pos, f2.pos)
      let sl0 = dxdy v2 v0
      let sl1 = dxdy v2 v1
      let i = f32.i64 (i64.f32 (v2.y - v0.y) - i)
      let y = v2.y + i
      let p0 = {x = v2.x + sl0 * i, y}
      let p1 = {x = v2.x + sl1 * i, y}
      let (pl, pr) = if p0.x <= p1.x then (p0, p1) else (p1, p0)
      let wl = calc_barycentric_coeffs (f0.pos, f1.pos, f2.pos) pl
      let wdelta_x = calc_barycentric_delta_x (f0.pos, f1.pos, f2.pos)
      in ((pl, pr), (wl, wdelta_x), tri_index)

    def get_line_in_triangle (((f0, f1, f2), tri_index): (triangle f32, i64)) (i: i64) =
      let (v0, v1, v2) = (f0.pos, f1.pos, f2.pos)
      in if i64.f32 v1.y == i64.f32 v2.y
         then get_line_in_upper_half_triangle ((f0, f1, f2), tri_index) i
         else if i64.f32 v0.y == i64.f32 v1.y
         then get_line_in_lower_half_triangle ((f0, f1, f2), tri_index) i
         else if i < i64.f32 (v1.y - v0.y)
         then get_line_in_upper_half_triangle ((f0, f1, f2), tri_index) i
         else get_line_in_lower_half_triangle ((f0, f1, f2), tri_index) i

    def rasterize 'target [n] [h] [w]
                  (plot: (pfragment V.t -> target))
                  (depth_cmp: f32 -> f32 -> #left | #right)
                  (frags: [n](pfragment V.t, pfragment V.t, pfragment V.t))
                  (ne: (target, f32))
                  (target_buf: [h][w]target, depth_buf: [h][w]f32) : ([h][w]target, [h][w]f32) =
      let (is, target_values, depth_values) =
        frags
        |> filter (\t -> (f32.abs (calc_signed_tri_area_2 (t.0.pos, t.1.pos, t.2.pos))) > 1e-6)
        |> map sort_y_ascending
        |> (\x -> zip x (indices x))
        |> expand num_lines_in_triangle get_line_in_triangle
        |> expand get_horizontal_line_size get_point_in_horizontal_line
        |> map (\(p, w, i) -> plot_fragment plot p w frags[i])
        |> unzip3
      let as = zip target_values depth_values
      let dest = zip (flatten target_buf) (flatten depth_buf) |> unflatten
      let cmp = (\f0 f1 -> match depth_cmp f0.1 f1.1 case #left -> f0 case #right -> f1)
      in reduce_by_index_2d (copy dest) cmp ne is as
         |> flatten
         |> unzip
         |> (\(x, y) -> (unflatten x, unflatten y))
  }

-- immediate triangle rasterizer for testing purposes. can use the REPL for this
module immediate_triangle_rasterizer_test = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = mk_imm_triangle_rasterizer (V)

  def rasterize_triangle_imm_test [n] (h: i64) (w: i64) (vs: [n]((f32, f32), (f32, f32), (f32, f32))) : [h][w]i32 =
    let target_buf = replicate h (replicate w false)
    let depth_buf = replicate h (replicate w (-f32.inf))
    let in_fb v = 0 <= v.0 && v.0 < f32.i64 w && 0 <= v.1 && v.1 < f32.i64 h
    let frags =
      vs
      |> filter (\(v0, v1, v2) -> in_fb v0 && in_fb v1 && in_fb v2)
      |> map (\(f0, f1, f2) ->
                ( {pos = {x = f0.0, y = f0.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f1.0, y = f1.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f2.0, y = f2.1}, depth = 1, Z_inv = 1, attr = true}
                ))
    let plot = (\(f: pfragment bool) -> f.attr)
    let depth_cmp (x: f32) (y: f32) = if x > y then #left else #right
    in M.rasterize plot depth_cmp frags (false, -f32.inf) (target_buf, depth_buf) |> (.0)
       |> map (map i32.bool)
}

open immediate_triangle_rasterizer_test
