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
    type triangle = (pfragment V.t, pfragment V.t, pfragment V.t)

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

    def calc_barycentric_coeffs ((v0, v1, v2): (vec2f.t, vec2f.t, vec2f.t)) (p: vec2f.t) : (f32, f32) =
      let signed_area_2 = calc_signed_tri_area_2 (v0, v1, v2)
      let v1p = p vec2f.- v1
      let v2p = p vec2f.- v2
      let v1v2 = v2 vec2f.- v1
      let v2v0 = v0 vec2f.- v2
      in (((v1v2 `vec2f.cross` v1p) / signed_area_2), (v2v0 `vec2f.cross` v2p) / signed_area_2)

    def plot_fragment 'target (plot: (pfragment V.t -> target)) (f: pfragment V.t) =
      ((i64.f32 (f.pos.y + 0.5), i64.f32 (f.pos.x + 0.5)), plot f, f.depth)

    def get_horizontal_line_size (((pl, pr), _): ((vec2f.t, vec2f.t), triangle)) =
      let xl = i64.f32 (f32.trunc (pl.x + 0.5))
      let xr = i64.f32 (f32.trunc (pr.x + 0.5)) - 1
      in 1 + xr - xl

    def get_point_in_horizontal_line (((pl, _), (f0, f1, f2)): ((vec2f.t, vec2f.t), triangle)) (i: i64) : pfragment V.t =
      let pos = {x = pl.x + f32.i64 i, y = pl.y}
      let (w0, w1) = calc_barycentric_coeffs (f0.pos, f1.pos, f2.pos) pos
      let w = (w0, w1, 1 - w0 - w1)
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

    def dxdy (a: vec2f.t) (b: vec2f.t) : f32 =
      let dx = b.x - a.x
      let dy = b.y - a.y
      in if dy == 0 then 0 else dx / dy

    def num_lines_in_triangle ((f0, f1, f2): triangle) : i64 =
      let (v0, _, v2) = sort_y_ascending (f0.pos, f1.pos, f2.pos)
      let top = f32.trunc (v2.y + 0.5)
      let bottom = f32.trunc (v0.y + 0.5)
      in i64.f32 (top - bottom)

    def get_line_in_triangle ((f0, f1, f2): triangle) (i: i64) =
      let (v0, v1, v2) = sort_y_ascending (f0.pos, f1.pos, f2.pos)
      let v0 = v0 |> vec2f.map (\x -> f32.trunc (x + 0.5))
      let v1 = v1 |> vec2f.map (\x -> f32.trunc (x + 0.5))
      let v2 = v2 |> vec2f.map (\x -> f32.trunc (x + 0.5))
      let y = v0.y + f32.i64 i
      in if v0.y != v1.y && (v1.y == v2.y || i <= i64.f32 (v1.y - v0.y))
         then -- upper half
              let sl0 = dxdy v0 v1
              let sl1 = dxdy v0 v2
              let dy = y - v0.y
              let p0 = {x = v0.x + sl0 * dy, y}
              let p1 = {x = v0.x + sl1 * dy, y}
              let (pl, pr) = if p0.x <= p1.x then (p0, p1) else (p1, p0)
              in ((pl, pr), (f0, f1, f2))
         else -- lower half
              let sl0 = dxdy v1 v2
              let sl1 = dxdy v0 v2
              let dy = y - v1.y
              let p0 = {x = v1.x + sl0 * dy, y}
              let p1 = {x = v0.x + sl1 * f32.i64 i, y}
              let (pl, pr) = if p0.x <= p1.x then (p0, p1) else (p1, p0)
              in ((pl, pr), (f0, f1, f2))

    def rasterize 'target [n] [h] [w]
                  (plot: (pfragment V.t -> target))
                  (depth_cmp: f32 -> f32 -> #left | #right)
                  (frags: [n](pfragment V.t, pfragment V.t, pfragment V.t))
                  (ne: (target, f32))
                  (target_buf: [h][w]target, depth_buf: [h][w]f32) : ([h][w]target, [h][w]f32) =
      let (is, target_values, depth_values) =
        frags
        |> map (ensure_cclockwise_winding_order)
        |> expand num_lines_in_triangle get_line_in_triangle
        |> expand get_horizontal_line_size get_point_in_horizontal_line
        |> map (plot_fragment plot)
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
