import "../../../diku-dk/segmented/segmented"

import "../fragment"
import "../varying"
import "../math/vec"

local
-- | line rasterizer specfication
module type mk_line_rasterizer_spec =
  (V: VaryingSpec)
  -> {
    -- | rasterize line given plot function, depth comparision function,
    -- line fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: pfragment V.t -> target)
      -> (depth_cmp: f32 -> f32 -> #left | #right)
      -> [n](pfragment V.t, pfragment V.t)
      -> (ne: (target, f32))
      -> ([h][w]target, [h][w]f32)
      -> ([h][w]target, [h][w]f32)
  }

-- | line rasterizer
module mk_line_rasterizer : mk_line_rasterizer_spec = \(V: VaryingSpec) ->
  {
    -- based on:
    -- futhark-book.readthedocs.io/en/latest/irregular-flattening.html#drawing-lines

    module V = VaryingExtensions (V)
    module F32 = VaryingExtensions (f32)

    def lerp_affine = F32.lerp_perspective_corrected_w_Z_inv_t
    def lerp_attr = V.lerp_perspective_corrected_w_Z_inv_t

    def compare_f32 (l: f32) (r: f32) : i64 =
      i64.bool (l < r) - i64.bool (l > r)

    def plot_fragment 'target
                      (plot: (pfragment V.t -> target))
                      (pos: vec2f.t)
                      (t: f32)
                      ((v0, v1): (pfragment V.t, pfragment V.t)) =
      let Z_inv = f32.lerp v0.Z_inv v1.Z_inv t
      let depth = lerp_affine Z_inv (v0.depth, v0.Z_inv) (v1.depth, v1.Z_inv) t
      let attr = lerp_attr Z_inv (v0.attr, v0.Z_inv) (v1.attr, v1.Z_inv) t
      in ((i64.f32 (pos.y + 0.5), i64.f32 (pos.x + 0.5)), plot {pos, Z_inv, depth, attr}, depth)

    def get_line_slope {x = x0: f32, y = y0: f32} {x = x1: f32, y = y1: f32} : f32 =
      if x1 == x0
      then if y1 > y0 then 1f32 else -1f32
      else (y1 - y0) / f32.abs (x1 - x0)

    def get_line_size (((p0, p1), _): ((vec2f.t, vec2f.t), i64)) =
      let {x = x0, y = y0} = p0
      let {x = x1, y = y1} = p1
      in 1 + i64.f32 (f32.max (f32.abs (x1 - x0)) (f32.abs (y1 - y0)))

    def get_point_in_line (((p0, p1), line_index): ((vec2f.t, vec2f.t), i64)) (i: i64) : (vec2f.t, f32, i64) =
      if f32.abs (p0.x - p1.x) > f32.abs (p0.y - p1.y)
      then let dir = compare_f32 (p0.x) (p1.x)
           let sl = get_line_slope p0 p1
           let pos =
             { x = p0.x + f32.i64 (dir * i)
             , y = p0.y + sl * f32.i64 i
             }
           let t = f32.from_fraction i (get_line_size ((p0, p1), line_index) - 1)
           in (pos, t, line_index)
      else let dir = compare_f32 (p0.y) (p1.y)
           let sl = get_line_slope {x = p0.y, y = p0.x} {x = p1.y, y = p1.x}
           let pos =
             { x = p0.x + sl * f32.i64 i
             , y = p0.y + f32.i64 (i * dir)
             }
           let t = f32.from_fraction i (get_line_size ((p0, p1), line_index) - 1)
           in (pos, t, line_index)

    def rasterize 'target [n] [h] [w]
                  (plot: (pfragment V.t -> target))
                  (depth_cmp: f32 -> f32 -> #left | #right)
                  (frags: [n](pfragment V.t, pfragment V.t))
                  (ne: (target, f32))
                  (target_buf: [h][w]target, depth_buf: [h][w]f32) : ([h][w]target, [h][w]f32) =
      let (is, target_values, depth_values) =
        frags
        |> map (\(l0, l1) -> (l0.pos, l1.pos))
        |> (\x -> zip x (indices x))
        |> filter (\l -> get_line_size l != 1)
        |> expand get_line_size get_point_in_line
        |> map (\(p, t, i) -> plot_fragment plot p t frags[i])
        |> unzip3
      let as = zip target_values depth_values
      let dest = zip (flatten target_buf) (flatten depth_buf) |> unflatten
      let cmp = (\f0 f1 -> match depth_cmp f0.1 f1.1 case #left -> f0 case #right -> f1)
      in reduce_by_index_2d (copy dest) cmp ne is as
         |> flatten
         |> unzip
         |> (\(x, y) -> (unflatten x, unflatten y))
  }

-- line rasterizer for testing purposes. can use the REPL for this
module line_rasterizer_test = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = mk_line_rasterizer (V)

  def rasterize_line_demo [n] (h: i64) (w: i64) (vs: [n]((f32, f32), (f32, f32))) : [h][w]i32 =
    let target_buf = replicate h (replicate w false)
    let depth_buf = replicate h (replicate w (-f32.inf))
    let in_fb v = 0 <= v.0 && v.0 < f32.i64 w && 0 <= v.1 && v.1 < f32.i64 h
    let frags =
      vs
      |> filter (\(v0, v1) -> in_fb v0 && in_fb v1)
      |> map (\(f0, f1) ->
                ( {pos = {x = f0.0, y = f0.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f1.0, y = f1.1}, depth = 1, Z_inv = 1, attr = true}
                ))
    let plot = (\(f: pfragment bool) -> f.attr)
    let depth_cmp (x: f32) (y: f32) = if x > y then #left else #right
    in M.rasterize plot depth_cmp frags (false, -f32.inf) (target_buf, depth_buf) |> (.0)
       |> map (map i32.bool)
}

open line_rasterizer_test
