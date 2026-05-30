import "../fragment"
import "../varying"

local
-- | point rasterizer specfication
module type PointRasterizerSpec = {
  val rasterize 'varying 'target [n] [h] [w] :
    (plot: fragment varying -> target)
    -> ([h][w]target, [h][w]f32)
    -> [n]fragment varying
    -> ([h][w]target, [h][w]f32)
}

-- | point rasterizer
module PointRasterizer : PointRasterizerSpec = {
  def unpack_fragment 'varying (f: fragment varying) =
    let y = i64.f32 (f.pos.y + 0.5)
    let x = i64.f32 (f.pos.x + 0.5)
    in ((y, x), f, f.depth)

  def rasterize 'varying 'target [n] [h] [w]
                (plot: (fragment varying -> target))
                ((target_buffer, depth_buffer): ([h][w]target, [h][w]f32))
                (frags: [n]fragment varying) : ([h][w]target, [h][w]f32) =
    let ne_target = copy target_buffer[0, 0]
    let (is, frag_values, depth_values) =
      frags
      |> map unpack_fragment
      |> unzip3
    let depth_buffer =
      reduce_by_index_2d (copy depth_buffer) f32.max 0 is depth_values
    let (is, target_values) =
      zip is frag_values
      |> map (\((y, x), f) ->
                if (0 <= x && x < w) && (0 <= y && y < h)
                && depth_buffer[y, x] == f.depth
                then ((y, x), plot f)
                else ((-1, -1), ne_target))
      |> unzip2
    let target_buffer = scatter_2d (copy target_buffer) is target_values
    in (target_buffer, depth_buffer)
}

-- point rasterizer for testing purposes. can use the REPL for this
local
module PointRasterizerTest = {
  local module M = PointRasterizer

  def test_rasterize_point [n] (h: i64) (w: i64) (vs: [n](f32, f32)) : [h][w]i32 =
    let target_buffer = replicate h (replicate w false)
    let depth_buffer = replicate h (replicate w 0)
    let frags =
      vs
      |> map (\(x, y) -> {pos = {x, y}, depth = 1, Z_inv = 1, attr = true})
    let plot = (\(f: fragment bool) -> f.attr)
    in M.rasterize plot (target_buffer, depth_buffer) frags
       |> (.0)
       |> map (map i32.bool)
}

open PointRasterizerTest
