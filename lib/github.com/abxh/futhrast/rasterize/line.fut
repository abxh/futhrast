import "../../../diku-dk/segmented/segmented"

import "../fragment"
import "../varying"

local
-- | line rasterizer specfication
module type LineRasterizerSpec =
  (V: VaryingSpec)
  -> {
    -- | rasterize line given plot function, depth type,
    -- line fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: fragment V.t -> target)
      -> ([h][w]target, [h][w]f32)
      -> [n](fragment V.t, fragment V.t)
      -> ([h][w]target, [h][w]f32)
  }

-- | line rasterizer
module LineRasterizer : LineRasterizerSpec = \(V: VaryingSpec) ->
  {
    -- based on:
    -- futhark-book.readthedocs.io/en/latest/irregular-flattening.html#drawing-lines

    local module Attr = VaryingExtensions V

    local
    type line = (fragment V.t, fragment V.t)

    def compare_f32 (l: f32) (r: f32) : f32 =
      f32.bool (l < r) - f32.bool (l > r)

    def get_line_slope {x = x0: f32, y = y0: f32} {x = x1: f32, y = y1: f32} : f32 =
      if x1 == x0
      then if y1 > y0 then 1 else -1
      else (y1 - y0) / f32.abs (x1 - x0)

    def get_line_size ((v0, v1): line) =
      let {x = x0, y = y0} = v0.pos
      let {x = x1, y = y1} = v1.pos
      let size = i64.f32 (f32.max (f32.abs (x1 - x0)) (f32.abs (y1 - y0)) + 0.5)
      in i64.bool (size != 0) + size

    def get_point_in_line ((v0, v1): line) (i: i64) =
      let (p0, p1) = (v0.pos, v1.pos)
      in if f32.abs (p0.x - p1.x) > f32.abs (p0.y - p1.y)
         then let dir = compare_f32 (p0.x) (p1.x)
              let sl = get_line_slope p0 p1
              let x = 0.5 + p0.x + (dir * f32.i64 i)
              let y = 0.5 + p0.y + f32.floor (sl * f32.i64 i + 0.5)
              let t = f32.from_fraction i (get_line_size (v0, v1) - 1)
              let Z_inv = f32.lerp v0.Z_inv v1.Z_inv t
              let depth = f32.lerp v0.depth v1.Z_inv t
              let attr = Attr.lerp_pc_w_Zinv Z_inv (v0.attr, v0.Z_inv) (v1.attr, v1.Z_inv) t
              in ((i64.f32 y, i64.f32 x), {pos = {x, y}, Z_inv, depth, attr}, depth)
         else let dir = compare_f32 (p0.y) (p1.y)
              let sl = get_line_slope {x = p0.y, y = p0.x} {x = p1.y, y = p1.x}
              let x = 0.5 + p0.x + f32.floor (sl * f32.i64 i + 0.5)
              let y = 0.5 + p0.y + dir * f32.i64 i
              let t = f32.from_fraction i (get_line_size (v0, v1) - 1)
              let Z_inv = f32.lerp v0.Z_inv v1.Z_inv t
              let depth = f32.lerp v0.depth v1.Z_inv t
              let attr = Attr.lerp_pc_w_Zinv Z_inv (v0.attr, v0.Z_inv) (v1.attr, v1.Z_inv) t
              in ((i64.f32 y, i64.f32 x), {pos = {x, y}, Z_inv, depth, attr}, depth)

    def rasterize 'target [n] [h] [w]
                  (plot: (fragment V.t -> target))
                  ((target_buffer, depth_buffer): ([h][w]target, [h][w]f32))
                  (frags: [n](fragment V.t, fragment V.t)) : ([h][w]target, [h][w]f32) =
      let ne_target = copy target_buffer[0, 0]
      let (is, frag_values, depth_values) =
        frags
        |> expand get_line_size get_point_in_line
        |> unzip3
      let depth_buffer = reduce_by_index_2d (copy depth_buffer) f32.max 0 is depth_values
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

-- line rasterizer for testing purposes. can use the REPL for this
module LineRasterizerTest = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = LineRasterizer (V)

  def rasterize_line_demo [n] (h: i64) (w: i64) (vs: [n]((f32, f32), (f32, f32))) : [h][w]i32 =
    let target_buffer = replicate h (replicate w false)
    let depth_buffer = replicate h (replicate w (-f32.inf))
    let frags =
      vs
      |> map (\(f0, f1) ->
                ( {pos = {x = f0.0, y = f0.1}, depth = 1, Z_inv = 1, attr = true}
                , {pos = {x = f1.0, y = f1.1}, depth = 1, Z_inv = 1, attr = true}
                ))
    let plot = (\(f: fragment bool) -> f.attr)
    in M.rasterize plot (target_buffer, depth_buffer) frags
       |> (.0)
       |> map (map i32.bool)
}

open LineRasterizerTest
