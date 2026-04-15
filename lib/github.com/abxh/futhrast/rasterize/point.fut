import "../types"
import "../../../diku-dk/sorts/radix_sort"

local
-- | point rasterizer specfication
module type PointRasterizerSpec =
  (V: VaryingSpec)
  -> {
    -- | rasterize point given plot function, depth selection function,
    -- point fragments, a neutral value for the target/depth buffers and
    -- the target/depth buffers themselves
    val rasterize 'target [n] [h] [w] :
      (plot: fragment V.t -> target)
      -> (depth_select: f32 -> f32 -> f32)
      -> (ne: (target, f32))
      -> [h][w](target, f32)
      -> [n]fragment V.t
      -> [h][w](target, f32)
  }

-- | point rasterizer
module PointRasterizer : PointRasterizerSpec = \(V: VaryingSpec) ->
  {
    def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)

    def transform_fragment (f: fragment V.t) =
      let y = i64.f32 (f.pos.y + 0.5)
      let x = i64.f32 (f.pos.x + 0.5)
      in ((y, x), f, f.depth)

    def rasterize 'target [n] [h] [w]
                  (plot: (fragment V.t -> target))
                  (depth_select: f32 -> f32 -> f32)
                  ((ne_target, ne_depth): (target, f32))
                  (dest: [h][w](target, f32))
                  (frags: [n]fragment V.t) : [h][w](target, f32) =
      let (is, frag_values, depth_values) =
        frags
        |> map transform_fragment
        |> unzip3
      let depth_buffer = flatten dest |> map (.1) |> unflatten
      let depth_buffer = reduce_by_index_2d (copy depth_buffer) depth_select ne_depth is depth_values
      let (is, target_values) =
        zip3 is frag_values depth_values
        |> map (\((y, x), f, d) ->
                  if (0 <= x && x < w) && (0 <= y && y < h) && depth_buffer[y, x] == d
                  then ((y, x), plot f)
                  else ((-1, -1), ne_target))
        |> unzip2
      let target_buffer = flatten dest |> map (.0) |> unflatten
      let target_buffer = scatter_2d (copy target_buffer) is target_values
      in zip (flatten target_buffer) (flatten depth_buffer) |> unflatten
  }

-- point rasterizer for testing purposes. can use the REPL for this
module PointRasterizerTest = {
  local
  module V : VaryingSpec with t = bool = {
    type t = bool
    def (+) = (||)
    def (*) s x = if bool.f32 s then x else false
  }

  -- note: above do not satisfy all the algrebraic properties required for varying,
  -- but is defined such for testing purposes

  local module M = PointRasterizer (V)

  def rasterize_point_test [n] (h: i64) (w: i64) (vs: [n](f32, f32)) : [h][w]i32 =
    let dest = replicate h (replicate w (false, -f32.inf))
    let frags =
      vs
      |> map (\(x, y) -> {pos = {x, y}, depth = 1, Z_inv = 1, attr = true})
    let plot = (\(f: fragment bool) -> f.attr)
    let depth_select (x: f32) (y: f32) = if x > y then x else y
    let (target_buf, _) =
      M.rasterize plot depth_select (false, -f32.inf) dest frags
      |> flatten
      |> unzip
    in target_buf
       |> unflatten
       |> map (map i32.bool)
}

open PointRasterizerTest
